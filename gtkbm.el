(defvar gtkbm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "o") 'gtkbm-open)
    (define-key map (kbd "d") 'kill-whole-line)
    (define-key map (kbd "q") 'gtkbm-close)
    (define-key map (kbd "<return>") 'gtkbm-open)
    (define-key map (kbd "C-g") 'gtkbm-close)
    map))

(define-derived-mode gtkbm-mode fundamental-mode "gtkbm"
  "major mode handling gtk-bookmark"
  t)

(defun gtkbm-close ()
  ""
  (interactive)
  (save-buffer)
  (kill-buffer)
  (set-window-configuration gtkbm--window-configuration))

(defun gtkbm ()
  ""
  (interactive)
  (setq gtkbm--window-configuration (current-window-configuration))
  (let* ((recentf-exclude (list gtkbm-file-path))
         (bf (find-file-noselect gtkbm-file-path)))
    (with-current-buffer bf
      (gtkbm-mode)
      (rename-buffer "*gtkbm*" t))
    (shrink-window-if-larger-than-buffer (get-buffer-window (pop-to-buffer bf t t)))))

(defvar gtkbm--window-configuration nil "")

(defvar gtkbm--last-window nil "not needed?")

(defun gtkbm-open ()
  ""
  (interactive)
  (let ((dir (gtkbm-get-dir)))
    (gtkbm-close)
    (dired dir)))

(defun gtkbm-get-dir ()
  ""
  (interactive)
  (buffer-substring-no-properties (save-excursion
                                    (goto-char (point-at-bol))
                                    (forward-char 7)
                                    (point))
                                  (point-at-eol)))

(defvar gtkbm-file-path (expand-file-name "~/.gtk-bookmarks"))

(defun gtkbm-add-current-dir ()
  ""
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (recentf-exclude (list gtkbm-file-path))
         (bf (find-file-noselect gtkbm-file-path)))
    (with-current-buffer bf
      (goto-char (point-max))
      (unless (eq (point) (point-at-bol))
        (newline))
     (insert "file://"
              dir)
      (save-buffer))
    (kill-buffer bf)
    (message "%s added to %s." dir gtkbm-file-path)))

(defun gtkbm-move-upward ()
  ""
  (interactive)
  ())

(defun gtkbm-move-downward ()
  ""
  (interactive)
  ())

(provide 'gtkbm)
