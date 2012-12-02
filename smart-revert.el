(defvar smart-revert--last-buffer nil
  "Last buffer.")

(defun smart-revert ()
  "Call `smart-revert-revert' if current buffer is changed since last call."
  (unless (eq smart-revert--last-buffer (current-buffer))
    (setq smart-revert--last-buffer (current-buffer))
    (smart-revert-revert)))

(defun smart-revert-revert ()
  "Revert current buffer when and only when changes are found."
  (interactive)
  (when (or (and (eq major-mode 'dired-mode)
                 (dired-directory-changed-p default-directory))
            (not (verify-visited-file-modtime (current-buffer))))
    (if (file-readable-p buffer-file-name)
        (progn
          (revert-buffer t t)
          (message "%s reverted." (buffer-name)))
      (message "%s not found."
               buffer-file-nmae))))

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
