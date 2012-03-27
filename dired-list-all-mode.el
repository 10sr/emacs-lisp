;; add to your dot.emacs as below:
;; (when (require 'dired-list-all-mode nil t)
;;    (setq dired-listing-switches "-lhFG")
;;    (add-hook 'dired-mode-hook
;;              (lambda ()
;;                (define-key dired-mode-map "a" 'dired-list-all-mode)
;;                )))


(defvar dired-list-all-switch "-A"
  "Switch for listing dot files.
Should be \"-a\" or \"-A\". Additional switch can be included.")

(define-minor-mode dired-list-all-mode
  "Toggle whether list dot files in dired.
When using this mode the value of `dired-listing-switches' should not contain \"-a\" or \"-A\" option."
  :init-value nil
  :global nil
  :lighter " ALL"
  (when (eq major-mode 'dired-mode)
    (dired-list-all-set)
    (revert-buffer)))

(defun dired-list-all-set ()
  ""
  (if dired-list-all-mode
      (or (string-match-p dired-list-all-switch
                          dired-actual-switches)
          (setq dired-actual-switches
                (concat dired-list-all-switch
                        " "
                        dired-actual-switches)))
    (setq dired-actual-switches
          (replace-regexp-in-string (concat dired-list-all-switch
                                            " ")
                                    ""
                                    dired-actual-switches))))

(add-hook 'dired-mode-hook
          'dired-list-all-set)

(provide 'dired-list-all-mode)
