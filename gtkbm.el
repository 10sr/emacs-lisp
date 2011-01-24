(defvar gtkbm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map (kbd "<up>") 'previous-line)
    (define-key map (kbd "<down>") 'next-line)
    (define-key map "o" 'gtkbm-open)
    (define-key map "d" 'kill-whole-line)
    (define-key map "u" 'undo)
    (define-key map "p" 'gtkbm-move-upward)
    (define-key map "n" 'gtkbm-move-downward)
    (define-key map "q" 'gtkbm-close)
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
  (let ((st (buffer-substring-no-properties (point-at-bol)
                                            (point-at-eol))))
    (kill-whole-line)
    (forward-line -1)
    (insert st
            "\n")
    (forward-line -1)))

(defun gtkbm-move-downward ()
  ""
  (interactive)
  (let ((st (buffer-substring-no-properties (point-at-bol)
                                            (point-at-eol))))
    (kill-whole-line)
    (forward-line 1)
    (insert st
            "\n")
    (forward-line -1)))

(provide 'gtkbm)

