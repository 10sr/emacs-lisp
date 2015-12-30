;;; recentf-show.el --- Neat recentf view

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp
;; Version: 0.1
;; Package-Requires: ((recentf "0"))
;; Keywords: recentf view show

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Neat recentf viewer.

;;; Code:

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

(defvar recentf-show-before-listing-hook nil
  "Hook run before creating buffer of `recentf-show'.")

(defvar recentf-show-window-configuration nil
  "Used for internal")

(defvar recentf-show-abbreviate t
  "Non-nil means use `abbreviate-file-name' when listing recently opened files.")

(define-derived-mode recentf-show-mode fundamental-mode "recentf-show"
  "Major mode for `recentf-show'."
  ;; (set (make-local-variable 'scroll-margin)
  ;;      0)
  )

;;;###autoload
(defun recentf-show (&optional files buffer-name)
  "Show simplified list of recently opened files.
If optional argument FILES is non-nil, it is a list of recently-opened
files to choose from. It defaults to the whole recent list.
If optional argument BUFFER-NAME is non-nil, it is a buffer name to
use for the buffer. It defaults to \"*recetf-show*\"."
  (interactive)
  (let ((bf (recentf-show-create-buffer files buffer-name)))
    (if bf
        (progn
          ;; (recentf-save-list)
          (setq recentf-show-window-configuration (current-window-configuration))
          (pop-to-buffer bf t t)
          (set-window-text-height (selected-window)
                                  recentf-show-window-height)
          (shrink-window-if-larger-than-buffer (selected-window)))
      (message "No recent file!"))))

(defun recentf-show-create-buffer (&optional files buffer-name)
  "Create buffer listing recentf files."
  (run-hooks 'recentf-show-before-listing-hook)
  (let ((bname (or buffer-name
                   "*recentf-show*"))
        (list (or files
                  recentf-list)))
    (when list
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
                list)
          (goto-char (point-min))
          (setq buffer-read-only t))
        bf))))

(defun recentf-show-close ()
  "Close recentf-show window."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration recentf-show-window-configuration))

(defun recentf-show-find-file ()
  "Fine file of current line."
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (find-file f)))

(defun recentf-show-view-file ()
  "view file of current line."
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (view-file f)))

(defun recentf-show-get-filename ()
  "Get filename of current line."
  (buffer-substring-no-properties (point-at-bol)
                                  (point-at-eol)))

(defun recentf-show-dired()
  "Open dired buffer of directory containing file of current line."
  (interactive)
  (let ((f (recentf-show-get-filename)))
    (recentf-show-close)
    (dired (if (file-directory-p f)
               f
             (or (file-name-directory f)
                 ".")))))

(provide 'recentf-show)

;;; recentf-show.el ends here
