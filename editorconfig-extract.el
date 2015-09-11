;;; editorconfig-extract.el --- Extract EditorConfig Properties

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/editorconfig-extract.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility editorconfig

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

;; Extract EditorConfig properties.


;;; Code:

(defvar editorconfig-extract-properties-alist
  (setq editorconfig-extract-properties-alist
  '(
    ("indent_style" . (if indent-tabs-mode
                          "tab"
                        "space"))
    ("indent_size" . nil)
    ("tab_width" . (int-to-string tab-width))
    ("end_of_line" . (let ((type (car (last (split-string (symbol-name buffer-file-coding-system)
                                                       "-")))))
                       (cond ((string-equal type "unix")
                              "lf")
                             ((string-equal type "mac")
                              "cr")
                             ((string-equal type "dos")
                              "crlf")
                             )))
    ("charset" . (let ((coding (symbol-name buffer-file-coding-system)))
                   (cond ((or (string-match-p "^utf-8" coding)
                              (string-match-p "^prefer-utf-8" coding))
                          "utf-8")
                         ((string-match-p "^latin-1" coding)
                          "latin1")
                         ((string-match-p "^utf-16-be" coding)
                          "utf-16be")
                         ((string-match-p "^utf-16-le" coding)
                          "utf-16le")
                         )))
    ("trim_trailing_whitespace" . (if (or (memq 'delete-trailing-whitespace
                                                before-save-hook)
                                          (memq 'delete-trailing-whitespace
                                                write-file-functions)
                                          ;; NOTE: Are there other hooks that
                                          ;; can contain this function?
                                          )
                                      "true"
                                    "false"))
    ("insert_final_newline" . (if require-final-newline
                                  ;; require-final-newline can take some sort of
                                  ;; values, but here only nil is translated
                                  ;; into false
                                  "true"
                                "false"))
    ))
  "Alist of EditorConfig properties to extract.
Each element should be like (PROP . SEXP)")

(defun editorconfig-extract-current-buffer ()
  "Extract EditorConfig properties for current buffer."
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-name))
         (output-buf (generate-new-buffer (concat "*editorconfig<"
                                                  filename
                                                  ">*"))))
    (with-current-buffer output-buf
      (insert "["
              filename
              "]\n\n"))
    (dolist (prop editorconfig-extract-properties-alist)
      (let ((value (eval (cdr prop))))
        (message "val:%S" output-buf)
        (when value
          (with-current-buffer output-buf
            (insert (car prop)
                    " = "
                    value
                    "\n")))))
    (display-buffer output-buf)))
(editorconfig-extract-current-buffer)
(defun editorconfig-extract (buf)
  "Extract EditorConfig properties for BUF."
  (with-current-buffer buf
    (editorconfig-extract-current-buffer)))

;;; editorconfig-extract.el ends here
