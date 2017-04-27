;;; editorconfig-charset-extras.el --- Extra EditorConfig Charset Support

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/editorconfig-charset-extras.el
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

;; This library adds extra charset supports to editorconfig-emacs.
;; Charsets defined in `coding-system-alist' are newly supported,

;; For example, add following to your `.editorconfig`
;; and `sjis.txt` will be opend with `sjis' encoding:

;;     [sjis.txt]
;;     charset = sjis

;; Alternatively, you can specify `emacs_charset` instead:

;;     [sjis.txt]
;;     emacs_charset = sjis

;; If both `charset` and `emacs_charset` are defined, the value of
;; `emacs_charset' takes precedence.

;;; Code:

;;;###autoload
(defun editorconfig-charset-extras (hash)
  "Add support for all charset to editorconfig from editorconfig HASH."
  coding-system-alist)

(provide 'editorconfig-charset-extras)

;;; editorconfig-charset-extras.el ends here
