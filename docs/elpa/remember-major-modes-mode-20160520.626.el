;;; remember-major-modes-mode.el --- Remember major-modes for files

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/remember-major-modes-mode.el
;; Package-Version: 20160520.626
;; Package-Commit: cd48564ce0b18d532029d699dadb606d903faa32
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: major-mode

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

;; `remember-major-modes-mode' is a global minor mode to remember major-modes
;; for files.

;; Usually major-modes for files are detected automatically using the value of
;; `auto-mode-alist' or shebang.  However sometimes these detection do not work
;; and yet updating `auto-mode-alist' is too match.
;; Interactive function `remember-major-modes-remember' remember current buffer
;; filename and major-mode so that the mode will be enabled next time the file
;; opened.

;; To use, add to your dot.emacs as below:
;; (when (require 'remember-major-modes-mode nil t)
;;    (remember-major-modes-mode 1))

;; You can use M-x remember-major-modes-memorize to remember the pair of current
;; file and major-mdoe and M-x remember-major-modes-forget to forget the
;; major-mode for current visiting file.

;;; Code:

;; TODO: use hash-table: make-hash-tabke, gethash, puthash

;;;###autoload
(define-minor-mode remember-major-modes-mode
  "Remember major mode for specific file."
  :init-value nil
  :global t
  :lighter ""
  (if remember-major-modes-mode
      (remember-major-modes-load)
    (remember-major-modes-save)))

(defvar remember-major-modes-modes-alist
  nil
  "Alist of filenames and major-modes.")

(defvar remember-major-modes-file
  (expand-file-name (concat user-emacs-directory
                            "remember-major-modes-file-modes.el"))
  "File name to save and load alist of filename and major-modes.")

(defun remember-major-modes-load ()
  "Load file `remember-major-modes-file'."
  (interactive)
  (and (file-readable-p remember-major-modes-file)
       (setq remember-major-modes-modes-alist
             (with-temp-buffer
               (let ((inhibit-read-only t))
                 (insert-file-contents remember-major-modes-file)
                 (goto-char (point-min))
                 (read (current-buffer)))))))

(defun remember-major-modes-save ()
  "Save remembered pairs of filenames and major-modes.
This pair will be written into the file `remember-major-modes-file'."
  (interactive)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (print remember-major-modes-modes-alist (current-buffer))
      (write-region (point-min)
                    (point-max)
                    remember-major-modes-file))))

(defun remember-major-modes-memorize (&optional mode filename)
  "Remember the pair of MODE and FILENAME.
If these arguments are omitted current ones are used."
  (interactive)
  (if remember-major-modes-mode
      (let ((f (or filename
                   buffer-file-name))
            (m (or mode
                   major-mode)))
        (when f
          (remember-major-modes-forget f)
          (message "Remember: %s => `%s'" f m)
          (setq remember-major-modes-modes-alist
                (cons (cons f
                            m)
                      remember-major-modes-modes-alist))))
    (message "remember-major-modes-mode not enabled.")))

(defun remember-major-modes-forget (&optional filename)
  "Forget major-modes for FILENAME."
  (let ((elem (assoc (or filename
                         buffer-file-name)
                     remember-major-modes-modes-alist)))
    (and elem
         (message "Remember: forget %s" elem)
         (setq remember-major-modes-modes-alist
               (delq elem
                     remember-major-modes-modes-alist)))))

(defun remember-major-modes-set ()
  "Set major mode for current file if I remembered it."
  (interactive)
  (when remember-major-modes-mode
    (let ((mode (cdr (assoc buffer-file-name
                            remember-major-modes-modes-alist))))
      (and mode
           (message "Remember: %s <= `%s'"
                    buffer-file-name mode)
           (funcall mode)))))

(add-hook 'find-file-hook
          'remember-major-modes-set)
(add-hook 'kill-emacs-hook
          'remember-major-modes-save)

(provide 'remember-major-modes-mode)

;;; remember-major-modes-mode.el ends here
