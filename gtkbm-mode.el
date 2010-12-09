(define-derived-mode gtkbm-mode fundamental-mode "gtkbm"
  "major mode handling gtk-bookmark"
  (use-local-map gtkbm-mode-map))

(defvar gtkbm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "o" 'gtkbm-open)
    (define-key map (kbd "C-g") 'gtkbm-abort)
    map))

(defun gtkbm ()
  ""
  (interactive)
  ())

(defun gtkbm-open ()
  ""
  (interactive)
  (dired (gtkbm-get-dir)))

(defun gtkbm-get-dir ()
  ""
  (interactive)
  (buffer-substring-no-properties (+ (point-at-bol)
                                     7)
                                  (point-at-eol)))
