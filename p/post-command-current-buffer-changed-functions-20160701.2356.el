;;; post-command-current-buffer-changed-functions.el --- Hook for current buffer change

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/emacs-lisp
;; Package-Version: 20160701.2356
;; Version: 0.0.1
;; Keywords: hook utility

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

;; This package provides a hook variable
;; `post-command-current-buffer-changed-functions'.

;;; Code:

;;;###autoload
(defvar post-command-current-buffer-changed-functions
  nil
  "Hook run when the result of `current-buffer' has been changed.")

(defvar post-command-current-buffer-changed-functions--last-buffer
  nil
  "The last current buffer.")

;;;###autoload
(defun post-command-current-buffer-changed-functions-run ()
  "Run `post-command-current-buffer-changed-functions' if needed.

This function checks the result of `current-buffer', and run
`post-command-current-buffer-changed-functions' when it has been changed from
the last buffer.

This function should be hooked to `post-command-hook'."
  (unless (eq (current-buffer)
              post-command-current-buffer-changed-functions--last-buffer)
    (let ((current (current-buffer))
          (previous post-command-current-buffer-changed-functions--last-buffer))
      (setq post-command-current-buffer-changed-functions--last-buffer
            current)
      (run-hook-with-args 'post-command-current-buffer-changed-functions
                          current
                          previous))))

;;;###autoload
(add-hook 'post-command-hook
          'post-command-current-buffer-changed-functions-run)

(provide 'post-command-current-buffer-changed-functions)

;;; post-command-current-buffer-changed-functions.el ends here
