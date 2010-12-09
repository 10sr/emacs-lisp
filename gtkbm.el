(defvar gtkbm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "o") 'gtkbm-open)
    (define-key map (kbd "<return>") 'gtkbm-open)
    (define-key map (kbd "C-g") 'gtkbm-abort)
    map))

(define-derived-mode gtkbm-mode fundamental-mode "gtkbm"
  "major mode handling gtk-bookmark"
  t)

(defun gtkbm-abort ()
  ""
  (interactive)
  (set-window-configuration gtkbm--window-configuration))

(defun gtkbm ()
  ""
  (interactive)
  (setq gtkbm--window-configuration (current-window-configuration))
  (let ((bf (find-file-noselect "~/.gtk-bookmarks")))
    (with-current-buffer bf
      (gtkbm-mode)
      (rename-buffer "*gtkbm*" t))
    (pop-to-buffer bf t t)))

(defvar gtkbm--window-configuration nil "")

(defvar gtkbm--last-window nil "not needed?")

(defun gtkbm-open ()
  ""
  (interactive)
  (let ((dir (gtkbm-get-dir)))
    (set-window-configuration gtkbm--window-configuration)
    (dired dir)))

(defun gtkbm-get-dir ()
  ""
  (interactive)
  (buffer-substring-no-properties (progn (goto-char (point-at-bol))
                                         (forward-char 7)
                                         (point))
                                  (point-at-eol)))
