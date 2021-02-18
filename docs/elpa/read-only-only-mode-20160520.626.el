;;; read-only-only-mode.el --- Always view-mode

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/read-only-only-mode.el
;; Package-Version: 20160520.626
;; Package-Commit: 33a1d920e032007b39d5e7240867f0b8f9557946
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility

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

;; `read-only-only-mode' is a global minor mode to Visit all files with
;; view-mode enabled.

;;; Code:

;;;###autoload
(define-minor-mode read-only-only-mode
  "Visit all files with view mode enabled."
  :global t
  :init-value nil
  :lighter " ROO"
  (ignore)
  ;; (if read-only-only-mode
  ;;     (add-hook 'find-file-hook
  ;;               'view-mode-enable)
  ;;   (remove-hook 'find-file-hook
  ;;                'view-mode-disable))
  )

(defun read-only-only-set ()
  "Enable function `view-mode' if `read-only-only-mode' is non-nil."
  (and buffer-file-name
       read-only-only-mode
       (view-mode 1)))

(add-hook 'find-file-hook
          'read-only-only-set)

(provide 'read-only-only-mode)

;;; read-only-only-mode.el ends here
