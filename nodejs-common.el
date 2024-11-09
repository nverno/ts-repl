;;; nodejs-common.el --- Node functionality shared b/w js and typescript -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-repl
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (xterm-color "2.0"))
;; Created:  5 April 2024
;; Keywords: languages, typescript, javascript, node, repl

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

(require 'cc-mode)                      ; syntax
(require 'comint)

(defvar nodejs-common-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st)
  "Syntax table in node repls.")

(defvar add-node-modules-path-command)
(declare-function add-node-modules-path "add-node-modules-path")

(defun nodejs-common--locate-executable (command)
  "Locate executable for COMMAND.
When `add-node-modules-path' is available, local executables will be found
first."
  (when (require 'add-node-modules-path nil t)
    ;; npm v >= 9 no longer has 'npm bin' command
    (when (version< (string-trim-right (shell-command-to-string "npm -v")) "9")
      (setq add-node-modules-path-command '("npm bin")))
    (add-node-modules-path))
  (or (executable-find command)
      (user-error "Executable \"%s\" not found" command)))

(defun nodejs-common--calculate-command
    (prompt-msg default-command default-args &optional force-prompt program)
  (let* ((program (let ((cmd (or program default-command)))
                    (if (functionp cmd)
                        (funcall cmd)
                      (nodejs-common--locate-executable cmd))))
         (cmdline (concat program " " (mapconcat 'identity default-args " "))))
    (if force-prompt (read-shell-command prompt-msg cmdline) cmdline)))

(defun nodejs-common--write-history (process _)
  "Write history file for inferior node PROCESS."
  (let ((buffer (process-buffer process)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer (comint-write-input-ring)))))

(defun nodejs-common--make-comint (cmd proc-name &optional envvars history-file start-file)
  "Create a Typescript comint buffer.
CMD is the Typescript command to be executed and PROC-NAME is the process name
that will be given to the comint buffer.
If STARTFILE is non-nil, use that instead of `ts-repl-startfile'
which is used by default. See `make-comint' for details of STARTFILE.
If SHOW is non-nil, display the Typescript comint buffer after it is created.
Returns the name of the created comint buffer."
  (let ((proc-buff-name (format "*%s*" proc-name)))
    (unless (comint-check-proc proc-buff-name)
      (let* ((cmdlist (split-string-and-unquote cmd))
             (program (car cmdlist))
             (args (cdr cmdlist))
             (buffer (apply #'make-comint-in-buffer
                            proc-name
                            proc-buff-name
                            "env"
                            start-file
                            `("TERM=xterm" ,@envvars ,program ,@args))))
        (when history-file
          (set-process-sentinel
           (get-buffer-process buffer) #'nodejs-common--write-history))))
    proc-buff-name))


;;; Output filter

(defvar-local nodejs-common--prompt-internal nil)

(defun nodejs-common--calculate-prompt-regexps (prompt prompt-continue)
  (setq nodejs-common--prompt-internal
        (rx-to-string `(: (or ,prompt ,prompt-continue)))
        comint-prompt-regexp nodejs-common--prompt-internal))

(defun nodejs-common--preoutput-filter (string)
  ;; Ignore repeated prompts when switching windows
  (if (and (not (if (comint-after-pmark-p)
                    (bolp)
                  (when-let* ((proc (get-buffer-process (current-buffer))))
                    (save-excursion
                      (goto-char (process-mark proc))
                      (bolp)))))
           (string-match-p
            (rx-to-string
             `(seq bos (+ (regexp ,nodejs-common--prompt-internal)) eos))
            string))
      ""
    ;; Filter out the extra prompt characters that
    ;; accumulate in the output when sending regions
    ;; to the inferior process.
    (replace-regexp-in-string
     (rx-to-string `(: bol
                       (* (regexp ,nodejs-common--prompt-internal))
                       (group (regexp ,nodejs-common--prompt-internal) (* nonl))))
     "\\1" string)))


;;; Major mode

(define-derived-mode nodejs-common-mode comint-mode "Node"
  "Major mode for Node repls.

This mode resets `comint-output-filter-functions' locally, so you
may want to re-add functions to it using `nodejs-common-mode-hook'."
  :syntax-table nodejs-common-syntax-table
  (setq-local mode-line-process '(":%s")
              comment-start "//"
              comment-end ""
              comment-start-skip "//+ *"
              parse-sexp-ignore-comments t
              parse-sexp-lookup-properties t
              paragraph-separate "\\'")

  (setq-local comint-input-ignoredups t
              comint-input-history-ignore "^\\."
              comint-highlight-input nil
              comint-prompt-read-only t
              comint-process-echoes t

              comint-scroll-to-bottom-on-input 'this
              comint-scroll-to-bottom-on-output 'this
              scroll-conservatively 1
              comint-output-filter-functions '(comint-watch-for-password-prompt)
              comint-preoutput-filter-functions
              '(xterm-color-filter nodejs-common--preoutput-filter)))

(provide 'nodejs-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nodejs-common.el ends here
