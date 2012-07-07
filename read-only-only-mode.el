(define-minor-mode read-only-only-mode
  "Visit all files with view mode enabled."
  :global t
  :init-value nil
  :lighter " ROO"
  (ignore)
  ;; (if read-only-only-mode
  ;;     (add-hook 'find-file-hook
  ;;               'view-mode-enable)
  ;;   (remove-hook 'find-file-hook
  ;;                'view-mode-disable))
  )

(defun read-only-only-set ()
  "enable `view-mode' if `read-only-only-mode' is non-nil." 
  (and buffer-file-name
       read-only-only-mode
       (view-mode 1)))

(add-hook 'find-file-hook
          'read-only-only-set)

(provide 'read-only-only-mode)
