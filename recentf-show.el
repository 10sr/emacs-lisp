(require 'recentf)

(defvar recentf-show-window-height 10)

(defvar recentf-show-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "C-m") 'recentf-show-find-file) 
    (define-key map "q" 'recentf-show-close)
    (define-key map (kbd "C-g") 'recentf-show-close)
    map))

(defvar recentf-show-window-configuration nil)

(defvar recentf-show-abbreviate t
  "Non-nil means `abbreviate-file-name' in `recentf-show'.")

(define-derived-mode recentf-show-mode fundamental-mode "recentf-show"
  "major mode for `recentf-show'"
  ;; (set (make-local-variable 'scroll-margin)
  ;;      0)
  )

(defun recentf-show ()
  (interactive)
  (setq recentf-show-window-configuration (current-window-configuration))
  (pop-to-buffer (recentf-show-create-buffer) t t)
  (set-window-text-height (selected-window)
                          recentf-show-window-height)
  (shrink-window-if-larger-than-buffer (selected-window)))

(defun recentf-show-create-buffer ()
  (let ((bname "*recentf-show*"))
    (and (get-buffer bname)
         (kill-buffer bname))
    (let ((bf (get-buffer-create bname)))
      (with-current-buffer bf
        (recentf-show-mode)
        (mapc (lambda (f)
                (insert (if recentf-show-abbreviate
                            (abbreviate-file-name f)
                          f)
                        "\n"))
              recentf-list)
        (goto-char (point-min))
        (setq buffer-read-only t))
      bf)))

(defun recentf-show-close ()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration recentf-show-window-configuration))

(defun recentf-show-find-file ()
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (find-file f)))

(defun recentf-show-view-file ()
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (view-file f)))

(defun recentf-show-get-filename ()
  (buffer-substring-no-properties (point-at-bol)
                                  (point-at-eol)))

(provide 'recentf-show)
