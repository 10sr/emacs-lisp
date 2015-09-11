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

;; Variable Definition derived from editorconfig.el `edconf-indentation-alist'
;; http://github.com/editorconfig/editorconfig-emacs#readme
;; Copyright (C) 2011-2015 EditorConfig Team
;; Released under GPL
(defvar editorconfig-extract-mode-offset-alist
  '((awk-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (css-mode css-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (erlang-mode erlang-indent-level)
    (groovy-mode c-basic-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  shm-indent-spaces)
    (idl-mode c-basic-offset)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js2-mode js2-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (latex-mode tex-indent-basic)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (mustache-mode mustache-basic-offset)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (perl-mode perl-indent-level)
    (pike-mode c-basic-offset)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent-offset
                 python-indent
                 py-indent-offset)
    (ruby-mode ruby-indent-level)
    (scala-mode scala-indent:step)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (web-mode web-mode-markup-indent-offset)
    (yaml-mode yaml-indent-offset))
  "Alist of modes and its candidate for indentation offset.

Each element should be like (MODE . VARIABLE-CANDIDATES) .
The indentation offset will be gotten from the first valid value
 (varible is defined it value is not nil)."
  )


(defvar editorconfig-extract-properties-alist
  (setq editorconfig-extract-properties-alist
  '(
    ("indent_style" . (if indent-tabs-mode
                          "tab"
                        "space"))
    ("indent_size" . (let ((s (editorconfig-extract-indent-size major-mode)))
                       (if s
                           (int-to-string s)
                         nil)))
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

(defun editorconfig-extract-indent-size (mode)
  "Get indentation offset for major mode MODE.

If MODE is a derived mode of other mode and no suitable offset value was found,
it will go up recursively and take the first valid value.
If MODE is nil this function allways returns nil."
  (when mode
    (let ((var-list (cdr (assq mode
                               editorconfig-extract-mode-offset-alist))))
      (or (editorconfig-extract-take-first-valid var-list)
          (editorconfig-extract-indent-size (get mode
                                                 'derived-mode-parent))))))

(defun editorconfig-extract-take-first-valid (l)
  "Accept list of variables L and return the first valid value."
  (when l
    (let ((v (car l)))
      (or (and (boundp v)
               (eval v))
          (editorconfig-extract-take-first-valid (cdr l))))))

(editorconfig-extract-take-first-valid '(a lisp-indent-offset))

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
