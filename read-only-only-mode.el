(define-minor-mode read-only-only-mode
  "Visit all files in view mode."
  :global t
  :init-value nil
  :lighter " RO"
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
       (view-mode-enable)))

(add-hook 'find-file-hook
          'read-only-only-set)

(provide 'read-only-only-mode)
