;;; ts-repl.el --- Run a Typescript repl in an inferior process -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ts-repl
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (xterm-color "2.0"))
;; Created: 30 March 2024
;; Keywords: languages, typescript, repl

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
;;
;; Run a Typescript repl in an inferior process.
;;
;; When `add-node-modules-path' is available, `ts-repl-command' will be found in
;; local node_modules.
;;
;;; Code:

(require 'cc-mode)                      ; syntax
(require 'comint)

(defgroup ts-repl nil
  "Run Typescript process in a buffer."
  :group 'languages
  :prefix "ts-repl-")

(defcustom ts-repl-command "ts-node"
  "Command to run inferior Typescript process."
  :type 'string
  :risky t)

(defcustom ts-repl-arguments '("--cwdMode")
  "Command line arguments for `ts-repl-command'."
  :type '(repeat string))

(defcustom ts-repl-buffer-name "Typescript"
  "Default buffer name for the Typescript interpreter."
  :type 'string
  :safe 'stringp)

(defcustom ts-repl-prompt "> "
  "Top-level prompt used by the inferior Typescript process."
  :type 'string
  :safe 'stringp)

(defcustom ts-repl-prompt-continue "... "
  "Continuation prompt used by the inferior Typescript process."
  :type 'string
  :safe 'stringp)

(defcustom ts-repl-history-filename nil
  "File used to save command history of the inferior Typescript process."
  :type '(choice (const :tag "None" nil) file)
  :safe 'string-or-null-p)

(defcustom ts-repl-startfile nil
  "File to load into the inferior Typescript process at startup."
  :type '(choice (const :tag "None" nil) (file :must-match t)))

(defcustom ts-repl-enable-font-lock t
  "Non-nil to enable font-locking in the inferior Typescript buffer."
  :type 'boolean)

(defcustom ts-repl-enable-completion t
  "Enable/disable inferior Typescript completion at point."
  :type 'boolean)


(defvar ts-repl-debug nil
  "Non-nil to set TS_NODE_DEBUG.")

(defvar ts-repl-compilation-regexp-alist
  ;; FIXME: where should errors like "<repl>.ts:0:1" go?
  '(("^\\s-*\\([^\n:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
  "Regexp to match errors in Typescript repl.")

(defvar add-node-modules-path-command)

(defun ts-repl--locate-executable (command)
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

(defun ts-repl--calculate-command (&optional prompt program)
  (let* ((program (let ((cmd (or program ts-repl-command)))
                    (if (functionp cmd)
                        (funcall cmd)
                      (ts-repl--locate-executable cmd))))
         (command (concat program " " (mapconcat 'identity ts-repl-arguments " "))))
    (if prompt (read-shell-command "Run Typescript: " command) command)))

(defun ts-repl-buffer ()
  "Return inferior Typescript buffer for current buffer."
  (if (derived-mode-p 'ts-repl-mode)
      (current-buffer)
    (let* ((proc-name ts-repl-buffer-name)
           (buffer-name (format "*%s*" proc-name)))
      (when (comint-check-proc buffer-name)
        buffer-name))))

(defun ts-repl-process ()
  "Return inferior Typescript process for current buffer."
  (get-buffer-process (ts-repl-buffer)))

;;;###autoload
(defun ts-repl-run (&optional prompt cmd startfile show)
  "Run a Typescript interpreter in an inferior process.
With prefix, PROMPT, read command.
If CMD is non-nil, use it to start repl.
STARTFILE overrides `ts-repl-startfile' when present.
When called interactively, or with SHOW, show the repl buffer after starting."
  (interactive (list current-prefix-arg nil nil t))
  (let* ((cmd (ts-repl--calculate-command prompt cmd))
         (buffer (ts-repl-make-comint
                  cmd
                  ts-repl-buffer-name
                  (or startfile ts-repl-startfile)
                  show)))
    (get-buffer-process buffer)))

(defun ts-repl--write-history (process _)
  "Write history file for inferior Typescript PROCESS."
  (when ts-repl-history-filename
    (let ((buffer (process-buffer process)))
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer (comint-write-input-ring))))))

(defun ts-repl-make-comint (cmd proc-name &optional startfile show)
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
             (buffer (apply #'make-comint-in-buffer proc-name
                            proc-buff-name
                            "env"
                            startfile
                            `("TERM=xterm"
                              ,@(and ts-repl-debug '("TS_NODE_DEBUG=1"))
                              ,program ,@args))))
        (set-process-sentinel
         (get-buffer-process buffer) #'ts-repl--write-history)
        (with-current-buffer buffer
          (ts-repl-mode))))
    (when show
      (pop-to-buffer proc-buff-name))
    proc-buff-name))

(defvar-local ts-repl--prompt-internal nil)

(defun ts-repl-calculate-prompt-regexps ()
  (setq ts-repl--prompt-internal
        (rx-to-string `(: (or ,ts-repl-prompt-continue
                              ,ts-repl-prompt)))))

(defun ts-repl--preoutput-filter (string)
  ;; Ignore repeated prompts when switching windows
  (if (and (not (bolp))
           (string-match-p (rx-to-string
                            `(seq bos (or (+ ,ts-repl-prompt)
                                          (+ ,ts-repl-prompt-continue))
                                  eos))
                           string))
      ""
    ;; Filter out the extra prompt characters that
    ;; accumulate in the output when sending regions
    ;; to the inferior process.
    (replace-regexp-in-string
     (rx-to-string `(: bol
                       (* (regexp ,ts-repl--prompt-internal))
                       (group (regexp ,ts-repl--prompt-internal) (* nonl))))
     "\\1" string)))


;; -------------------------------------------------------------------
;;;  Completion

(defun ts-repl--completion-filter (proc string)
  (setq string (replace-regexp-in-string
                "\r+" "" (xterm-color-filter string)))
  (process-put proc 'done
               (or (string-prefix-p ts-repl-prompt string)
                   (string-match-p
                    (concat "^\\s-*" (process-get proc 'initial)) string)))
  (with-current-buffer (process-buffer proc)
    (insert string)
    (goto-char (point-max))))

(defun ts-repl--send-redirected (proc string)
  (process-put proc 'done nil)
  (process-put proc 'initial (string-trim string))
  (process-send-string proc string)
  (while (and (null (process-get proc 'done))
              (accept-process-output proc 0.1))))

;; Same procedure as `nodejs-repl--send-string'
(defun ts-repl--get-completions-from-process (input)
  (with-temp-buffer
    (let* ((proc (ts-repl-process))
           (orig-marker (marker-position (process-mark proc)))
           (orig-filter (process-filter proc))
           (orig-buf (process-buffer proc)))
      (unwind-protect
          (progn
            (set-process-buffer proc (current-buffer))
            (set-process-filter proc #'ts-repl--completion-filter)
            (set-marker (process-mark proc) (point-min))
            ;; send two tabs: see `nodejs-repl--get-completions-from-process'
            (ts-repl--send-redirected proc (concat input "\t"))
            (ts-repl--send-redirected proc "\t"))
        (set-process-buffer proc orig-buf)
        (set-process-filter proc orig-filter)
        (set-marker (process-mark proc) orig-marker orig-buf))
      (let ((beg (progn (goto-char (point-min))
                        (forward-line 1)
                        (point)))
            (end (progn (goto-char (point-max))
                        (forward-line -1)
                        (point))))
        (prog1 (split-string
                (buffer-substring-no-properties beg end)
                "[ \t\n]" t "[ \t\n]")
          ;; Clear repl line
          (process-send-string proc "\x15"))))))

(defvar ts-repl-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    (modify-syntax-entry ?$ "_" st)
    st)
  "Syntax table in `ts-repl-mode'.")

(defvar ts-repl--completion-syntax
  (let ((tab (copy-syntax-table ts-repl-syntax-table)))
    (modify-syntax-entry ?. "w" tab)
    tab))

;;; FIXME:
;; - not completing for repl commands, eg. '.b'
;; - use filename completion in strings
;; - better bounds for completion candidate
(defun ts-repl-completion-at-point ()
  (when (comint-after-pmark-p)
    (let ((end (point))
          (beg (save-excursion
                 (with-syntax-table ts-repl--completion-syntax
                   (backward-sexp))
                 (point))))
      (when (and (>= beg (comint-line-beginning-position))
                 (< beg end))
        (list beg end
              (completion-table-with-cache
               #'ts-repl--get-completions-from-process))))))

;;; Font-locking

(defvar ts-repl--commands
  '("break" "clear" "exit" "help" "save" "load" "editor" "type")
  "Repl commands available in ts-node.")

(defvar ts-repl-font-lock-keywords
  `((,(rx-to-string `(: bol  "." (or ,@ts-repl--commands) eow))
     . font-lock-keyword-face)))

(defvar-keymap ts-repl-mode-map
  :doc "Keymap in inferior Typescript buffer."
  "TAB" #'completion-at-point)

;;;###autoload
(define-derived-mode ts-repl-mode comint-mode "Typescript"
  "Major mode for Typescript repl.

\\<ts-repl-mode-map>"
  :syntax-table ts-repl-syntax-table
  (setq-local mode-line-process '(":%s")
              comment-start "//"
              comment-end ""
              comment-start-skip "//+ *"
              parse-sexp-ignore-comments t
              parse-sexp-lookup-properties t
              paragraph-separate "\\'"
              paragraph-start ts-repl-prompt)

  (ts-repl-calculate-prompt-regexps)
  (setq-local comint-input-ignoredups t
              comint-input-history-ignore "^\\."
              comint-prompt-read-only t
              comint-process-echoes t
              comint-input-ring-file-name ts-repl-history-filename
              comint-prompt-regexp ts-repl--prompt-internal
              comint-scroll-to-bottom-on-input 'this
              comint-scroll-to-bottom-on-output 'this
              ;; comint-scroll-show-maximum-output nil
              ;; comint-output-filter-functions '(ansi-color-process-output)
              comint-preoutput-filter-functions
              '(xterm-color-filter ts-repl--preoutput-filter))

  (when comint-input-ring-file-name
    ;; XXX: write history on exit?
    (comint-read-input-ring t))
  
  ;; Completion
  (when ts-repl-enable-completion
    (add-hook 'completion-at-point-functions #'ts-repl-completion-at-point nil t))

  ;; Font-locking
  (setq-local font-lock-defaults '(ts-repl-font-lock-keywords t))
  (setq comint-highlight-input nil
        comint-indirect-setup-function
        (lambda ()
          (let ((inhibit-message t)
                (message-log-max nil))
            (cond ((fboundp 'typescript-ts-mode)
                   (typescript-ts-mode))
                  ((fboundp 'typescript-mode)
                   (typescript-mode))
                  (t nil)))))
  (when (and (null comint-use-prompt-regexp)
             ts-repl-enable-font-lock
             (or (require 'typescript-ts-mode nil t)
                 (require 'typescript-mode nil t)))
    (comint-fontify-input-mode))

  ;; Errors
  (setq-local compilation-error-regexp-alist ts-repl-compilation-regexp-alist)
  (compilation-shell-minor-mode t))


(provide 'ts-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-repl.el ends here
