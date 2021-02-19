;;; dired-list-all-mode.el --- Toggle listing dot files in dired

;; Author: 10sr <8slashes+el@gmail.com>
;; URL: https://github.com/10sr/emacs-lisp
;; Package-Version: 20161114.1618
;; Package-Commit: c4baef802f8a8431a2e0a463d025fcfa26915d61
;; Version: 0.1
;; Keywords: dired

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

;; A buffer local minor mode for dired to toggle whether
;; list dot files.

;; For example add to your dot.emacs as below:
;; (when (require 'dired-list-all-mode nil t)
;;    (setq dired-listing-switches "-lhFG")
;;    (add-hook 'dired-mode-hook
;;              (lambda ()
;;                (local-set-key "a" 'dired-list-all-mode)
;;                )))

;;; Code:

(eval-when-compile
  (require 'dired nil t))

(defvar dired-list-all-switch "-A"
  "Switch for listing dot files.
Should be \"-a\" or \"-A\". Additional switch can be included.")

;;;###autoload
(define-minor-mode dired-list-all-mode
  "Toggle whether list dot files in dired.
When using this mode the value of `dired-listing-switches' should not contain
\"-a\" or \"-A\" option."
  :init-value nil
  :global nil
  :lighter " ALL"
  (when (eq major-mode 'dired-mode)
    (dired-list-all-set)
    (revert-buffer)))

;;;###autoload
(defun dired-list-all-set ()
  "Update `dired-actual-switches'."
  (if dired-list-all-mode
      (or (string-match-p dired-list-all-switch
                          dired-actual-switches)
          (setq dired-actual-switches
                (concat dired-list-all-switch
                        " "
                        dired-actual-switches)))
    (setq dired-actual-switches
          (replace-regexp-in-string (concat dired-list-all-switch
                                            " ")
                                    ""
                                    dired-actual-switches))))

;;;###autoload
(add-hook 'dired-mode-hook
          'dired-list-all-set)

(provide 'dired-list-all-mode)

;;; dired-list-all-mode.el ends here
