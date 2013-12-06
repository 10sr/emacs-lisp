;; todo: use format-mode-line

(progn
  (defvar buffer-file-changed-functions nil "Hook run when buffer file changed.
Each function is called with two args, the filename before changing and after
changing.")
  (declare-function run-buffer-file-changed-functions "terminal-title.el")
  (add-hook 'post-command-hook
            'run-buffer-file-changed-functions)
  (lexical-let (previous-file)
    (defun run-buffer-file-changed-functions ()
      ""
      (unless (and previous-file
                   (equal previous-file
                          (expand-file-name (or buffer-file-name
                                                default-directory))))
        (let ((pfile previous-file)
              (cfile (expand-file-name (or buffer-file-name
                                           default-directory))))
          (setq previous-file cfile)
          (run-hook-with-args 'buffer-file-changed-functions pfile cfile)))))
  ;; (add-hook 'buffer-file-changed-function
  ;;           (lambda (pdir cdir)
  ;;             (message "dir changed %s to %s !" pdir cdir)))
  )

(defvar terminal-title-term-regexp "^\\(rxvt\\|xterm\\|aterm$\\|screen\\)"
  "Rexexp for `set-terminal-title'.")

(defun terminal-title-set (&rest args)
  "Set terminal title."
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
  "List of elements for terminal title.")

(defun terminal-title-set-tmux-window-name (&rest args)
  "Set tmux window name."
  (interactive "sString to set as title: ")
  (when (getenv "TMUX")
    (send-string-to-terminal (apply 'concat
                                    "\033k"
                                    `(,@args "\033\\")))))

(defvar terminal-title-tmux-window-name-format nil
  "List of elements for tmux window name.")

(define-minor-mode terminal-title-mode
  "Set terminal title."
  :init-value nil
  :global t
  :lighter "")

(define-minor-mode terminal-title-tmux-window-name-mode
  "Set tmux window name."
  :init-value nil
  :global t
  :lighter "")

(defun terminal-title-update (&rest args)
  "Update terminal titles `terminal-title-set' and
`terminal-title-set-tmux-window-name' using `terminal-title-format' and
`terminal-title-tmux-window-name-format' when `terminal-title-mode' and
`terminal-title-tmux-window-name-mode' are enabled respectively.
ARGS are ignored."
  (interactive)
  (when terminal-title-mode
    (apply 'terminal-title-set
           (mapcar 'eval
                 terminal-title-format)))
  (when terminal-title-tmux-window-name-mode
    (apply 'terminal-title-set-tmux-window-name
           (mapcar 'eval
                   terminal-title-tmux-window-name-format))))

(add-hook 'buffer-file-changed-functions
          'terminal-title-update)

(add-hook 'suspend-resume-hook
          'terminal-title-update)

(provide 'terminal-title)
;;; terminal-title.el ends here
