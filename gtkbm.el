;; gtkbm.el: Handle ~/.gtk-bookmarks with emacs.
;; usage: bind some keys to `gtkbm' and `gtkbm-add-current-dir'.

(require 'url-util)

(defvar gtkbm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "j" 'gtkbm-next-line)
    (define-key map "k" 'gtkbm-previous-line)
    (define-key map (kbd "<up>") 'gtkbm-previous-line)
    (define-key map (kbd "<down>") 'gtkbm-next-line)
    (define-key map "o" 'gtkbm-open)
    (define-key map "d" 'kill-whole-line)
    (define-key map "u" 'undo)
    (define-key map "p" 'gtkbm-move-upward)
    (define-key map "n" 'gtkbm-move-downward)
    (define-key map "q" 'gtkbm-close)
    (define-key map (kbd "C-m") 'gtkbm-open)
    (define-key map (kbd "C-g") 'gtkbm-close)
    map))

(defvar gtkbm-file-path (expand-file-name "~/.gtk-bookmarks"))

(defvar gtkbm--window-configuration nil "")

(define-derived-mode gtkbm-mode fundamental-mode "gtkbm"
  "major mode handling gtk-bookmark"
  (set (make-local-variable 'scroll-margin)
       0))

(eval-after-load "recentf"
  '(add-to-list 'recentf-exclude
               (rx-to-string gtkbm-file-path)))

(defun gtkbm-close ()
  ""
  (interactive)
  (save-buffer)
  (kill-buffer (current-buffer))
  (set-window-configuration gtkbm--window-configuration))

(defun gtkbm ()
  ""
  (interactive)
  (setq gtkbm--window-configuration (current-window-configuration))
  (let ((bf (find-file-noselect gtkbm-file-path)))
    (with-current-buffer bf
      (gtkbm-mode)
      (rename-buffer "*gtkbm*" t)
      (setq buffer-read-only t))
    (shrink-window-if-larger-than-buffer (get-buffer-window (pop-to-buffer bf t t)))))

(defun gtkbm-open ()
  ""
  (interactive)
  (let ((dir (gtkbm-get-dir)))
    (gtkbm-close)
    (dired dir)))

(defun gtkbm-encode-filename (string)
  (concat "file://"
          (mapconcat 'url-hexify-string
                     (split-string string
                                   "/")
                     "/")))

(defun gtkbm-decode-filename (string)
  (decode-coding-string (url-unhex-string (replace-regexp-in-string "^file://"
                                                                    ""
                                                                    string)
                                          t)
                        (or file-name-coding-system
                            default-file-name-coding-system
                            'utf-8)))

(defun gtkbm-get-dir ()
  ""
  (interactive)
  (gtkbm-decode-filename (save-excursion
                           (buffer-substring-no-properties (goto-char (point-at-bol))
                                                           (progn
                                                             (while (not (or (eq (aref (thing-at-point 'char)
                                                                                       0)
                                                                                 ? )
                                                                             (eq (point)
                                                                                 (point-at-eol))))
                                                               (forward-char 1))
                                                             (point))))))

(defun gtkbm-add-current-dir ()
  ""
  (interactive)
  (let* ((dir (directory-file-name (expand-file-name default-directory)))
         (dirname (file-name-nondirectory dir))
         (bf (find-file-noselect gtkbm-file-path))
         (inhibit-read-only t))
    (with-current-buffer bf
      (goto-char (point-max))
      (unless (eq (point) (point-at-bol))
        (newline))
      (insert (gtkbm-encode-filename dir)
              " "
              dirname)
      (save-buffer))
    (kill-buffer bf)
    (message "%s added to %s." dir gtkbm-file-path)))

(defun gtkbm-move-upward ()
  ""
  (interactive)
  (let ((st (buffer-substring-no-properties (point-at-bol)
                                            (point-at-eol)))
        (inhibit-read-only t))
    (kill-whole-line)
    (forward-line -1)
    (insert st
            "\n")
    (forward-line -1)))

(defun gtkbm-move-downward ()
  ""
  (interactive)
  (let ((st (buffer-substring-no-properties (point-at-bol)
                                            (point-at-eol)))
        (inhibit-read-only t))
    (kill-whole-line)
    (forward-line 1)
    (insert st
            "\n")
    (forward-line -1)))

(defun gtkbm-forward-line (arg)
  ""
  (interactive "p")
  (forward-line arg)
  (goto-char (+ (point-at-bol)
                7)))

(defun gtkbm-backward-line (arg)
  ""
  (interactive "p")
  (forward-line (- 0 arg))
  (goto-char (+ (point-at-bol)
                7)))

(provide 'gtkbm)

