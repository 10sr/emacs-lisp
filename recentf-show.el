(require 'recentf)

(defvar recentf-show-window-height 10
  "Max height of window of `recentf-show'")

(defvar recentf-show-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "C-m") 'recentf-show-find-file)
    (define-key map (kbd "SPC") 'recentf-show-find-file)
    (define-key map "v" 'recentf-show-view-file)
    (define-key map "@" 'recentf-show-dired)
    (define-key map "q" 'recentf-show-close)
    (define-key map (kbd "C-g") 'recentf-show-close)
    (define-key map "?" 'describe-mode)
    (define-key map "/" 'isearch-forward)
    map))

(defvar recentf-show-window-configuration nil)

(defvar recentf-show-abbreviate t
  "Non-nil means use `abbreviate-file-name' when listing recently opened files.")

(define-derived-mode recentf-show-mode fundamental-mode "recentf-show"
  "Major mode for `recentf-show'"
  ;; (set (make-local-variable 'scroll-margin)
  ;;      0)
  )

(defun recentf-show (&optional files buffer-name)
  "Show simplified list of recently opened files.
If optional argument FILES is non-nil, it is a list of recently-opened
files to choose from. It defaults to the whole recent list.
If optional argument BUFFER-NAME is non-nil, it is a buffer name to
use for the buffer. It defaults to \"*recetf-show*\"."
  (interactive)
  ;; (recentf-save-list)
  (setq recentf-show-window-configuration (current-window-configuration))
  (pop-to-buffer (recentf-show-create-buffer files buffer-name) t t)
  (set-window-text-height (selected-window)
                          recentf-show-window-height)
  (shrink-window-if-larger-than-buffer (selected-window)))

(defun recentf-show-create-buffer (&optional files buffer-name)
  "Create buffer listing recentf files"
  (let ((bname (or buffer-name
                   "*recentf-show*")))
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
              (or files
                  recentf-list))
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

(defun recentf-show-dired()
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (dired (if (file-directory-p f)
               f
             (or (file-name-directory f)
                 ".")))))

(provide 'recentf-show)
