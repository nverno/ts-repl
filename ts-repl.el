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

(require 'nodejs-common)
(require 'comint)

(defgroup ts-repl nil
  "Run Typescript process in a buffer."
  :group 'languages
  :prefix "ts-repl-")

(defcustom ts-repl-command "ts-node"
  "Command to run inferior Typescript process."
  :type 'string
  :risky t)

(defcustom ts-repl-arguments '("--cwdMode" "--pretty")
  "Command line arguments for `ts-repl-command'."
  :type '(repeat string))

(defcustom ts-repl-process-name "Typescript"
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
  `(("^\\s-*\\([^<\n:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
  "Regexp to match errors in Typescript repl.")


(defun ts-repl-buffer ()
  "Return inferior Typescript buffer for current buffer."
  (if (derived-mode-p 'ts-repl-mode)
      (current-buffer)
    (let* ((proc-name ts-repl-process-name)
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
  (let* ((cmd (nodejs-common--calculate-command
               "Run typescript: " ts-repl-command ts-repl-arguments prompt cmd))
         (buffer (nodejs-common--make-comint
                  cmd
                  ts-repl-process-name
                  (and ts-repl-debug '("TS_NODE_DEBUG=1"))
                  ts-repl-history-filename
                  (or startfile ts-repl-startfile))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ts-repl-mode)
        (ts-repl-mode)))
    (when show
      (pop-to-buffer buffer))
    (get-buffer-process buffer)))


;; -------------------------------------------------------------------
;;;  Completion

(defun ts-repl--completion-filter (proc string)
  (setq string (replace-regexp-in-string
                "\r+" "" (xterm-color-filter string)))
  (process-put proc 'done
               (or (string-match-p (concat "^" ts-repl-prompt) string)
                   (string-match-p
                    (concat "^\\s-*" (process-get proc 'initial)) string)))
  (with-current-buffer (process-buffer proc)
    (insert string)
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))))

(defun ts-repl--send-redirected (proc string)
  (process-put proc 'done nil)
  (process-put proc 'initial string)
  (process-send-string proc string)
  (while (and (null (process-get proc 'done))
              (accept-process-output proc 0.1))))

;; Same procedure as `nodejs-repl--send-string'
(defun ts-repl--get-completions-from-process (input)
  "Get completions for input from inferior process."
  (with-temp-buffer
    (erase-buffer)
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
        (prog1 (when (> end beg)
                 (split-string
                  (buffer-substring-no-properties beg end)
                  "[ \t\n]" t "[ \t\n]"))
          ;; Clear repl line
          (process-send-string proc "\x15"))))))

(defvar ts-repl--completion-syntax
  (let ((tab (copy-syntax-table nodejs-common-syntax-table)))
    (modify-syntax-entry ?. "w" tab)
    tab))

;;; TODO:
;; - not completing for repl commands, eg. '.b'
;; - better bounds for completion candidate
(defun ts-repl-completion-at-point ()
  (when (and (comint-after-pmark-p)
             (not (let* ((syn (syntax-ppss nil)))
                    (or (car (setq syn (nthcdr 3 syn)))
                        (car (setq syn (cdr syn))) (nth 3 syn)))))
    (let ((end (point))
          (beg (save-excursion
                 (with-syntax-table ts-repl--completion-syntax
                   (ignore-errors (backward-sexp)))
                 (point))))
      (when (and beg (>= beg (comint-line-beginning-position))
                 (< beg end))
        (let ((completions (ts-repl--get-completions-from-process
                            (buffer-substring-no-properties beg end))))
          (when completions
            (list beg end (completion-table-with-cache (lambda (_s) completions)))))))))


;;; Sending Repl input
;; XXX(5/31/24): coould share this with nodejs-repl
(defun ts-repl-send-region (start end)
  "Send region from START to END to the `ts-repl-process'.
The region is sent the repl with editor mode enabled."
  (interactive "r")
  (let ((proc (ts-repl-process)) pos)
    (or proc (user-error "Typescript repl process not found."))
    ;; Erase the echoed output from
    ;; .editor ...
    (with-current-buffer (process-buffer proc)
      (setq pos (and comint-last-prompt
                     (marker-position (cdr comint-last-prompt))))
      (comint-snapshot-last-prompt))
    (process-send-string
     proc (concat
           ;; Note: preceding "\n" here helps prompt fontification stay sharp
           "\n.editor\n" (buffer-substring-no-properties start end) "\n"))
    (with-current-buffer (process-buffer proc)
      (while (accept-process-output proc 0.1))
      (when pos
        (let ((inhibit-read-only t)
              (pmark (progn (goto-char (process-mark proc))
                            (forward-line 0)
                            (point-marker))))
          (delete-region pos pmark)
          (goto-char pos)
          (comint-set-process-mark)
          (comint-snapshot-last-prompt))))
    (process-send-string proc "\x04")))


;;; Font-locking

(defvar ts-repl--commands
  '("break" "clear" "exit" "help" "save" "load" "editor" "type")
  "Repl commands available in ts-node.")

(defvar ts-repl-font-lock-keywords
  `((,(rx-to-string `(: bol  "." (or ,@ts-repl--commands) eow))
     . font-lock-keyword-face)
    (,(rx bol (or "..." (seq (* white) ".editor")))
     . font-lock-comment-face)
    ("^\\(Invalid\\) REPL keyword" (1 'error))
    ;; Documentation links in '.type <symbol>' output
    ("^\\[MDN Reference\\](\\([^)]+\\))"
     (0 (prog1 () (help-xref-button 0 'help-url (match-string 1))))
     (0 'link t)))
  "Additional font-locking keywords in `ts-repl-mode'.")


(defvar-keymap ts-repl-mode-map
  :doc "Keymap in inferior Typescript buffer."
  "TAB" #'completion-at-point)

;;;###autoload
(define-derived-mode ts-repl-mode nodejs-common-mode "Typescript"
  "Major mode for Typescript repl.

\\<ts-repl-mode-map>"
  :syntax-table nodejs-common-syntax-table
  (nodejs-common--calculate-prompt-regexps
   ts-repl-prompt ts-repl-prompt-continue)

  (setq-local paragraph-start ts-repl-prompt
              comint-input-ring-file-name ts-repl-history-filename)

  (when comint-input-ring-file-name
    (comint-read-input-ring t))

  ;; Completion
  (when ts-repl-enable-completion
    (add-hook 'completion-at-point-functions #'ts-repl-completion-at-point nil t))

  ;; Font-locking
  (setq-local font-lock-defaults '(ts-repl-font-lock-keywords nil nil))
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
  (compilation-shell-minor-mode t)
  ;; XXX: remove this?
  (face-remap-add-relative 'compilation-error 'link))

(provide 'ts-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ts-repl.el ends here
