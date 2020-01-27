;;; divide-window.el --- Split window evenly

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp
;; Package-Version: 20160520.626
;; Package-Commit: 9fb6794a888f70d7c888b372b64a5e2fe9f4ae4f
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: window

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

;; Split window evenly.

;;; Code:

(defvar divide-window-default-num
  3
  "Default number of windows `divide-window-vertically' and
`divide-window-horizontally' make.")

;;;###autoload
(defun divide-window-vertically (arg &optional window)
  "Divide window equally vertically."
  (interactive "p")
  (let ((n (if (eq arg 1)
               (or divide-window-default-num
                   3)
             arg))
        (height (window-height window)))
    (divide-window--internal window
                             n
                             (/ height n)
                             nil)))

;;;###autoload
(defun divide-window-horizontally (arg &optional window)
  "Divide window equally horizontally."
  (interactive "p")
  (let ((n (if (eq arg 1)
               (or divide-window-default-num
                   3)
             arg))
        (width (window-width window)))
    (divide-window--internal window
                             n
                             (/ width n)
                             t)))

(defun divide-window--internal (window num size side)
  "Used for internal"
  (unless (eq num 1)
    (let ((nw (split-window window
                            size
                            side)))
      (divide-window--internal nw
                               (- num 1)
                               size
                               side))))

(provide 'divide-window)

;;; divide-window.el ends here
