;;; pasteboard.el --- Use osx pasteboard for yank and paste

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/pasteboard.el
;; Package-Version: 20151230.1832
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility clipboard osx

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

;; Use osx pasteboard for yank and paste.

;;; Code:

(defvar pasteboard-paste-program (executable-find "pbpaste")
  "Program to get text from osx pasteboard.")
(defvar pasteboard-copy-program (executable-find "pbcopy")
  "Program to put text to osx pasteboard.")
(defvar pasteboard-rtun-program (executable-find "reattach-to-user-namespace")
  "Program reattach-to-user-namespace.

It is needed when you use pasteboard within tmux.  For details see
https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard")

(defvar pasteboard-paste-command pasteboard-paste-program
  "Command run to get text.")
(defvar pasteboard-copy-command pasteboard-copy-program
  "Command run to put text.")

;;;###autoload
(defun turn-on-pasteboard ()
  "Enable pasteboard for yank and paste."
  (interactive)
  (setq interprogram-paste-function 'pasteboard-paste)
  (setq interprogram-cut-function 'pasteboard-cut))

;;;###autoload
(defun turn-off-pasteboard ()
  "Disable pasteboard for yank and paste."
  (interactive)
  (setq interprogram-paste-function nil)
  (setq interprogram-cut-function nil))

(defun pasteboard-enable-rtun ()
  "Set to use reattach-to-user-namespace."
  (interactive)
  (if pasteboard-rtun-program
      (progn
        (setq pasteboard-paste-command
              (concat pasteboard-rtun-program
                      " "
                      pasteboard-paste-program))
        (setq pasteboard-copy-command
              (concat pasteboard-rtun-program
                      " "
                      pasteboard-copy-program)))
    (message
     "Cannot find reattach-to-user-namespace. First you must install it!")))

(defun pasteboard-disable-rtun ()
  "Set not to use reattach-to-user-namespace."
  (interactive)
  (setq pasteboard-paste-command
        pasteboard-paste-program)
  (setq pasteboard-copy-command
        pasteboard-copy-program))

(defun pasteboard-paste ()
  "Pasting function using pasteboard.

Intended to be set to `interprogram-paste-function'."
  (shell-command-to-string pasteboard-paste-command))

(defun pasteboard-cut (text &optional push)
  "Cutting function using pasteboard.

Intended to be set to `interprogram-cut-function'."
  (let ((process-connection-type nil))
    (let ((proc (start-process-shell-command pasteboard-copy-program
                                             "*Messages*"
                                             pasteboard-copy-command)))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'pasteboard)

;;; pasteboard.el ends here
