;; inspired by http://www.emacswiki.org/emacs-en/ChangingCursorDynamically
;; usage: (require 'set-modeline-color nil t)

(defvar set-modeline-color-color-alist
  `((readonly "white" "blue")
    (overwrite "white" "red")
    (insert "white" "black"))
"Alist of write state and modeline color.
Each element looks like (STATE FOREGROUND-COLOR BACKGROUND-COLOR).
STATE should be `insert', `readonly', or `overwrite'.")

(defvar set-modeline-color-state nil)

(defun set-modeline-color-according-to-write-mode ()
  ""
  (let ((state (if buffer-read-only
                   'readonly
                 (if overwrite-mode
                     'overwrite
                   'insert))))
    (unless (eq state set-modeline-color-state)
      (if (face-inverse-video-p 'mode-line)
          (progn
            (set-face-foreground 'mode-line
                                 (nth 2
                                      (assq state
                                            set-modeline-color-color-alist)))
            (set-face-background 'mode-line
                                 (nth 1
                                      (assq state
                                            set-modeline-color-color-alist))))
        (progn
          (set-face-foreground 'mode-line
                               (nth 1
                                    (assq state
                                          set-modeline-color-color-alist)))
          (set-face-background 'mode-line
                               (nth 2
                                    (assq state
                                          set-modeline-color-color-alist)))))
      (setq set-modeline-color-state state))))
(add-hook 'post-command-hook 'set-modeline-color-according-to-write-mode)
(add-hook 'after-init-hook 'set-modeline-color-according-to-write-mode)

(provide 'set-modeline-color)
