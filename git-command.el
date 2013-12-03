;;; git-command.el --- Dead simple git command interface

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/git-command.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility git

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Dead simple git interface.  No major-mode, only provides command-line like
;; interface using minibuffer.  You need not remember additional keybinds for
;; using git from Emacs.

;;; Code:

(defvar git-command-default-options
  ""
  "Options always passed to git.")

(defvar git-command-max-mini-window-height
  max-mini-window-height
  "Maximum height for resizing mini-window when showing result.
This value means nothing when `resize-mini-window' is nil.")

(defvar git-command-ps1-showdirtystate "t"
  "Set the value of GIT_PS1_SHOWDIRTYSTATE to this when running __git_ps1.")

(defvar git-command-ps1-showstashstate ""
  "Set the value of GIT_PS1_SHOWSTASHSTATE to this when running __git_ps1.")

(defvar git-command-ps1-showuntrackedfiles ""
  "Set the value of GIT_PS1_SHOWUNTRACKEDFILES to this when running __git_ps1.")

(defvar git-command-ps1-showupstream "auto"
  "Set the value of GIT_PS1_SHOWUPSTREAM to this when running __git_ps1.")

(defvar git-command-history nil
  "History list for `git-command'.")

(defvar git-command-major-mode-alist
  '(
    ("diff" . diff-mode)
    )
  "Alist of major mode for git commands.
Each element should be like (CMD . MAJOR-MODE).")

(defun git-command-find-git-ps1 (f)
  "Return F if F exists and it contain function \"__git_ps1\"."
  (and (file-readable-p f)
       (with-temp-buffer
         (insert ". " f "; "
                 "__git_ps1 %s;")
         (eq 0 (shell-command-on-region (point-min)
                                        (point-max)
                                        "bash -s"
                                        nil
                                        t)))
       f))

(defvar git-command-prompt-file
  (or (git-command-find-git-ps1 "/usr/share/git/completion/git-prompt.sh")
      (git-command-find-git-ps1
       "/opt/local/share/doc/git-core/contrib/completion/git-prompt.sh")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git-prompt")
      (git-command-find-git-ps1
       "/opt/local/share/git-core/git-prompt.sh")
      (git-command-find-git-ps1 "/opt/local/etc/bash_completion.d/git")
      ))

(defun git-command-ps1 (fmt)
  "Generate git ps1 string from FMT."
  (let ((gcmpl (or git-command-prompt-file))
        (process-environment `(,(concat "GIT_PS1_SHOWDIRTYSTATE="
                                        git-command-ps1-showdirtystate)
                               ,(concat "GIT_PS1_SHOWSTASHSTATE="
                                        git-command-ps1-showstashstate)
                               ,(concat "GIT_PS1_SHOWUNTRACKEDFILES="
                                        git-command-ps1-showuntrackedfiles)
                               ,(concat "GIT_PS1_SHOWUPSTREAM="
                                        git-command-ps1-showupstream)
                               ,@process-environment)))
    (if (and (executable-find "bash")
             gcmpl
             (file-readable-p gcmpl))
        (with-temp-buffer
          (insert ". " gcmpl
                  "; __git_ps1 "
                  (shell-quote-argument fmt)
                  ";")
          (shell-command-on-region (point-min)
                                   (point-max)
                                   "bash -s"
                                   nil
                                   t)
          (buffer-substring-no-properties (point-min)
                                          (point-max)))
      "")))

(defun git--command-get-major-mode (cmd)
  "Return apropriate major mode for CMD by `git-command-major-mode-alist'."
  (cdr (assoc (car (split-string cmd))
              git-command-major-mode-alist)))

(eval-when-compile
  (require 'ansi-color nil t))

(defun git-command (cmd)
  "Shell like git command interface.  CMD is the command to run."
  (interactive (list (read-shell-command (format "[%s]%s $ git : "
                                                 (abbreviate-file-name
                                                  default-directory)
                                                 (git-command-ps1 "[GIT:%s]"))
                                         nil
                                         'git-command-history)))
  (let ((bname (concat "*"
                       "git "
                       (car (split-string cmd
                                          " "))
                       "*"))
        (majormode (git--command-get-major-mode cmd)))
    (if majormode
        (progn
          (and (get-buffer bname)
               (kill-buffer bname))
          (display-buffer (get-buffer-create bname))
          (with-current-buffer bname
            (let ((inhibit-read-only t))
              (erase-buffer)
              (view-mode)
              (if (featurep 'ansi-color)
                  (progn
                    (shell-command (concat "git -c color.ui=always "
                                           cmd)
                                   t)
                    (ansi-color-apply-on-region (point-min)
                                                (point-max)))
                (shell-command (concat "git "
                                       cmd)
                               t))
              (funcall majormode))))
      (git-command-term-shell-command (concat "git "
                                              git-command-default-options
                                              " "
                                              cmd)
                                      bname))))

(eval-when-compile
  (require 'term nil t))
(defvar term-shell-command-history nil
  "History for term-shell-command.")
(defun git-command-term-shell-command (command &optional buffer-or-name)
  "Run COMMAND in terminal emulator.
If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in the
buffer is destroyed.  Otherwise, new buffer is generated automatically from
COMMAND."
  (interactive (list (read-shell-command "Run program: "
                                         nil
                                         'term-shell-command-history)))
  (let* ((name (car (split-string command
                                  " ")))
         (buf (if buffer-or-name
                  (get-buffer-create buffer-or-name)
                (generate-new-buffer (concat "*"
                                             name
                                             "*"))))
         (proc (get-buffer-process buf))
         (dir default-directory))
    (and proc
         (delete-process proc))
    (display-buffer buf)
    (with-current-buffer buf
      (cd dir)
      (set (make-local-variable 'term-scroll-to-bottom-on-output)
           t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert "Start executing "
                command)
        (add-text-properties (point-at-bol)
                             (point-at-eol)
                             '(face bold))
        (insert "\n\n"))
      (require 'term)
      (term-mode)
      (term-exec buf
                 (concat "term-" name)
                 shell-file-name
                 nil
                 (list shell-command-switch
                       command))
      (term-char-mode)
      (if (ignore-errors (get-buffer-process buf))
          (set-process-sentinel (get-buffer-process buf)
                                (lambda (proc change)
                                  (with-current-buffer (process-buffer proc)
                                    (term-sentinel proc change)
                                    (goto-char (point-max)))))
        ;; (goto-char (point-max))
        ))))

(provide 'git-command)

;;; git-command.el ends here
