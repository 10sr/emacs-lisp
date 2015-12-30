;;; terminal-title.el --- Set terminal title

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp
;; Package-Version: 20151230.1832
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: terminal title console

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

;; Set terminal title.

;;; Code:

;; todo: use format-mode-line

(defvar buffer-file-changed-functions nil "Hook run when buffer file changed.
Each function is called with two args, the filename before changing and after
changing.")

(defvar terminal--title-previous-file
  nil
  "File name that was previously visited.")

(add-hook 'post-command-hook
          'run-buffer-file-changed-functions)

(defun run-buffer-file-changed-functions ()
  "Run `buffer-file-changed-functions'."
  (unless (and terminal--title-previous-file
               (equal terminal--title-previous-file
                      (expand-file-name (or buffer-file-name
                                            default-directory))))
    (let ((pfile terminal--title-previous-file)
          (cfile (expand-file-name (or buffer-file-name
                                       default-directory))))
      (setq terminal--title-previous-file cfile)
      (run-hook-with-args 'buffer-file-changed-functions pfile cfile))))
;; (add-hook 'buffer-file-changed-function
;;           (lambda (pdir cdir)
;;             (message "dir changed %s to %s !" pdir cdir)))

(defvar terminal-title-term-regexp "^\\(rxvt\\|xterm\\|aterm$\\|screen\\)"
  "Rexexp for `set-terminal-title'.")

(defun terminal-title-set (&rest args)
  "Set terminal title by concatinating ARGS."
  (interactive "sString to set as title: ")
  (let ((tty (frame-parameter nil
                              'tty-type)))
    (when (and tty
               (string-match terminal-title-term-regexp
                             tty))
      (send-string-to-terminal (apply 'concat
                                      "\033]0;"
                                      `(,@args "\007"))))))

(defvar terminal-title-format
  '(
    "["
    user-login-name
    "@"
    system-name
    ":"
    (abbreviate-file-name (or buffer-file-name
                              default-directory))
    "]["
    invocation-name
    " "
    emacs-version
    " "
    (symbol-name system-type)
    "]["
    "FRAME:"
    (frame-parameter nil 'name)
    "]"
    )
  "List of elements for terminal title.

 Each element must return string when evaluated.")

(defun terminal-title-set-tmux-window-name (&rest args)
  "Set tmux window name by concatinating ARGS."
  (interactive "sString to set as title: ")
  (when (getenv "TMUX")
    (send-string-to-terminal (apply 'concat
                                    "\033k"
                                    `(,@args "\033\\")))))

(defvar terminal-title-tmux-window-name-format nil
  "List of elements for tmux window name.
Each element must return string when evaluated.")

;;;###autoload
(define-minor-mode terminal-title-mode
  "Set terminal title."
  :init-value nil
  :global t
  :lighter "")

;;;###autoload
(define-minor-mode terminal-title-tmux-window-name-mode
  "Set tmux window name."
  :init-value nil
  :global t
  :lighter "")

(defun terminal-title-update (&rest args)
  "Update terminal titles.  ARGS are ignored.

 `terminal-title-set' and `terminal-title-set-tmux-window-name' using
 `terminal-title-format' and `terminal-title-tmux-window-name-format' when
`terminal-title-mode' and `terminal-title-tmux-window-name-mode' are enabled"
  (interactive)
  (and terminal-title-mode
       terminal-title-format
       (apply 'terminal-title-set
              (mapcar 'eval
                      terminal-title-format)))
  (and terminal-title-tmux-window-name-mode
       terminal-title-tmux-window-name-format
       (apply 'terminal-title-set-tmux-window-name
              (mapcar 'eval
                      terminal-title-tmux-window-name-format))))

(add-hook 'buffer-file-changed-functions
          'terminal-title-update)

(add-hook 'suspend-resume-hook
          'terminal-title-update)

(provide 'terminal-title)

;;; terminal-title.el ends here
