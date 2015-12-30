;;; smart-revert.el --- Revert buffers wisely

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: buffer revert

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

;; Revert buffers wisely..

;;; Code:

(defvar smart-revert--last-buffer nil
  "Last buffer.")

(declare-function dired-directory-changed-p "dired.el")

;;;###autoload
(defun smart-revert ()
  "Call `smart-revert-revert' if current buffer is changed since last call."
  (unless (eq smart-revert--last-buffer (current-buffer))
    (setq smart-revert--last-buffer (current-buffer))
    (smart-revert-revert)))

(defun smart-revert-revert ()
  "Revert current buffer when and only when change is found."
  (interactive)
  (when (or (and (eq major-mode 'dired-mode)
                 (dired-directory-changed-p default-directory))
            (and buffer-file-name
                 (file-readable-p buffer-file-name)
                 (not (verify-visited-file-modtime (current-buffer)))))
    (revert-buffer t t)
    (message "%s reverted." (buffer-name))))

;;;###autoload
(defun smart-revert-on ()
  "Enable `smart-revert'."
  (interactive)
  (add-hook 'post-command-hook ; 'window-configuration-change-hook
            'smart-revert))

;;;###autoload
(defun smart-revert-off ()
  "Disable `smart-revert'."
  (interactive)
  (remove-hook 'post-command-hook
               'smart-revert))

(provide 'smart-revert)

;;; smart-revert.el ends here
