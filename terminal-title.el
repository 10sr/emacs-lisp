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

(defvar set-terminal-title-term-regexp "^\\(rxvt\\|xterm\\|aterm$\\|screen\\)"
  "Rexexp for `set-terminal-title'.")

(defun set-terminal-title (&rest args)
  ""
  (interactive "sString to set as title: ")
  (let ((tty (frame-parameter nil
                              'tty-type)))
    (when (and tty
               (string-match set-terminal-title-term-regexp
                             tty))
      (send-string-to-terminal (apply 'concat
                                      "\033]0;"
                                      `(,@args "\007"))))))

(defun set-screen-name(&rest args)
  ""
  (interactive "sString to set as title: ")
  (when (getenv "TMUX")
    (send-string-to-terminal (apply 'concat
                                    "\033k"
                                    `(,@args "\033\\")))))

(defun my-set-terminal-title ()
  ""
  (set-terminal-title "["
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
                      ":"
                      (number-to-string (length
                                         (buffer-list-not-start-with-space)))
                      "]"
                      )
  (set-screen-name "emacs:"
                   (file-name-nondirectory
                    (directory-file-name default-directory))
                   "/"))

(add-hook 'buffer-file-changed-functions
          (lambda (p c)
            (my-set-terminal-title)))

(add-hook 'suspend-resume-hook
          'my-set-terminal-title)
