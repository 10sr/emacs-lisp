Flycheck black check.
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-black-check-setup))
