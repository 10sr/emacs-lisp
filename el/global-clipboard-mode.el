;;; global-clipboard-mode.el --- Share Clipboard among Multiple Emacs Instances

;; Copyright (C) 2015 10sr

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; Keywords: convenience, tools, clipboard, kill-ring
;; Version: 0.0.1
;; Contributor: Leo Shidai Liu <shidai.liu@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; `global-clipboard-mode' is a global minor-mode to share the content of Emacs
;; clipboard among multiple Emacs instances.

;; To use, simply enable `global-clipboard-mode':

;; (global-clipboard-mode 1)

;; Optionally, you can change the file which clipboard contents will be stored:

;; (setq global-clipboard-mode-clipboard-file
;;       "/tmp/clipboard.dat")


;;; Code:

(defvar global-clipboard-mode-clipboard-file
  (concat temporary-file-directory
          "clipboard.dat")
  "Path to file that contains the content of clipboard.")

(defvar global-clipboard-mode-last-clipboard nil
  "The value of the clipboard.")



(defun global-clipboard-mode-select-text (text)
  "Update clipboard content to TEXT.
This function will be set to `interprogram-cut-function'."
  (with-temp-buffer
    (insert text)
    (write-region (point-min)
                  (point-max)
                  global-clipboard-mode-clipboard-file))
  (setq global-clipboard-mode-last-clipboard text))

(defun global-clipboard-mode-selection-value ()
  "Get current clipboard text.
This function will be set to `interprogram-paste-function'."
  (let ((text (with-temp-buffer
                (insert-file-contents-literally
                 global-clipboard-mode-clipboard-file)
                (buffer-substring-no-properties (point-min)
                                                (point-max)))))

    (cond
     ((or (not text) (string= text ""))
      (setq global-clipboard-mode-last-clipboard nil))

     ((eq text global-clipboard-mode-last-clipboard)
      nil)

     ((string= text global-clipboard-mode-last-clipboard)
      ;; Record the newer string,
      ;; so subsequent calls can use the `eq' test.
      (setq global-clipboard-mode-last-clipboard text)
      nil)

     (t
      (setq global-clipboard-mode-last-clipboard text)))))


;; Minor-mode

;;;###autoload
(define-minor-mode global-clipboard-mode
  "Minor-mode to share clipboard among multiple Emacs instances."
  :global t
  :lighter ""
  (if global-clipboard-mode
      (progn
        (setq interprogram-cut-function 'global-clipboard-mode-select-text)
        (setq interprogram-paste-function 'global-clipboard-mode-selection-value))
    (setq interprogram-cut-function nil)
    (setq interprogram-paste-function nil)))


(provide 'global-clipboard-mode)
;;; global-clipboard-mode.el ends here
