(defvar smart-revert--last-buffer nil)

(defun smart-revert ()
  "Revert current buffer when and only when changes are found."
  (interactive)
  (unless (eq smart-revert--last-buffer (current-buffer))
    (setq smart-revert--last-buffer (current-buffer))
    (when (or (and (eq major-mode 'dired-mode)
                   (dired-directory-changed-p default-directory))
              (not (verify-visited-file-modtime (current-buffer))))
      (revert-buffer t t)
      (message "%s reverted." (buffer-name))
      )))

(defun smart-revert-on ()
  "Enable `smart-revert'"
  (interactive)
  (add-hook 'post-command-hook ; 'window-configuration-change-hook
            'smart-revert))

(defun smart-revert-off ()
  "Disable `smart-revert'"
  (interactive)
  (remove-hook 'post-command-hook
               'smart-revert))

(provide 'smart-revert)
