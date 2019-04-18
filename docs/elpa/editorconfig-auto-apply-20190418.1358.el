;;; editorconfig-auto-apply.el --- Auto apply editorconfig properties  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 10sr

;; Author: 10sr<>
;; Keywords: tools
;; Package-Version: 20190418.1358
;; Version: 0.0.1
;; URL: https://github.com/10sr/editorconfig-auto-apply-el
;; Package-Requires: ((editorconfig "0.0.1") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Apply EditorConfig properties automatically on .editorconfig save.

;;; Code:

(require 'editorconfig)

;;;###autoload
(define-minor-mode editorconfig-auto-apply-mode
  "When saving .editorconfig file update buffer configs."
  :lighter " ECAA"
  (if editorconfig-auto-apply-mode
      (add-hook 'after-save-hook
                'editorconfig-auto-apply-mode--run nil t)
    (remove-hook 'after-save-hook
                 'editorconfig-auto-apply-mode--run t)))

;;;###autoload
(defun editorconfig-auto-apply-enable ()
  "Turn on command `editorconfig-auto-apply-mode'."
  (unless editorconfig-auto-apply-mode
    (editorconfig-auto-apply-mode 1)))

(defun editorconfig-auto-apply-disable ()
  "Turn off command `editorconfig-auto-apply-mode'."
  (when editorconfig-auto-apply-mode
    (editorconfig-auto-apply-mode -1)))

(defun editorconfig-auto-apply-mode--run ()
  "When saving .editorconfig file walk all buffers and update configs."
  (when (eq major-mode
            'editorconfig-conf-mode)
    (let ((dir (file-name-directory buffer-file-name)))
      (cl-dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (file-in-directory-p (buffer-file-name buf)
                                        dir))
          (with-current-buffer buf
            (editorconfig-mode-apply)))))))


(provide 'editorconfig-auto-apply)
;;; editorconfig-auto-apply.el ends here
