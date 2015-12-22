A buffer local minor mode for dired to toggle whether
list dot files.

For example add to your dot.emacs as below:
(when (require 'dired-list-all-mode nil t)
   (setq dired-listing-switches "-lhFG")
   (add-hook 'dired-mode-hook
             (lambda ()
               (local-set-key "a" 'dired-list-all-mode)
               )))
