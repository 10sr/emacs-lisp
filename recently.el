;;; recently.el --- Handle recently visited files

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/recently.el
;; Version: 0.2
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

;; `recently-mode'

;;; Code:

(defvar recently-default-database-path
  (concat user-emacs-directory
          "recently.sqlite"))

(define-minor-mode recently-mode
  "Save recently visited files."
  :global t
  :init-value nil
  :lighter ""
  (if recently-mode
      (add-hook 'find-file-hook
                'recently-register-current)
    (remove-hook 'find-file-hook
                 'recently-register-current))
  )

(defun recently-register (filename)
  "Push FILENAME into the recently list.")

(defun recently-register-current ()
  "Register currently visiting file into the recently list."
  (and buffer-file-name
       (recently-register buffer-file-name)))

(defun recently-get-list ()
  "Get recently list.")

(provide 'recently)

;;; recently.el ends here
