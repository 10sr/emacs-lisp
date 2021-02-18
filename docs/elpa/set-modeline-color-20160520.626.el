;;; set-modeline-color.el --- Set mode-line color according to writability state

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/set-modeline-color.el
;; Package-Version: 20160520.626
;; Package-Commit: a533698f2e533cfed90834f7037754ede8d5d346
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility modeline

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

;; Define and change modeline color according to current buffer state: readonly,
;; insert or overwrite.
;; Inspired by http://www.emacswiki.org/emacs-en/ChangingCursorDynamically

;;; Code:

(defvar set-modeline-color-color-alist
  `((readonly "white" "blue")
    (overwrite "white" "red")
    (insert nil nil))
"Alist of write state and modeline color.
Each element looks like (STATE FOREGROUND-COLOR BACKGROUND-COLOR).
STATE should be `insert', `readonly', or `overwrite'.")

(defvar set-modeline-color-state nil)

(defun set-modeline-color-according-to-write-mode ()
  ""
  (let ((state (if buffer-read-only
                   'readonly
                 (if overwrite-mode
                     'overwrite
                   'insert))))
    (unless (eq state set-modeline-color-state)
      (if (face-inverse-video-p 'mode-line)
          (progn
            (set-face-foreground 'mode-line
                                 (or (nth 2
                                      (assq state
                                            set-modeline-color-color-alist))
                                     (face-foreground 'default)))
            (set-face-background 'mode-line
                                 (or (nth 1
                                      (assq state
                                            set-modeline-color-color-alist))
                                     (face-background 'default))))
        (progn
          (set-face-foreground 'mode-line
                               (or (nth 1
                                    (assq state
                                          set-modeline-color-color-alist))
                                   (face-background 'default)))
          (set-face-background 'mode-line
                               (or (nth 2
                                    (assq state
                                          set-modeline-color-color-alist))
                                   (face-foreground 'default)))))
      (setq set-modeline-color-state state))))
(add-hook 'post-command-hook 'set-modeline-color-according-to-write-mode)
(add-hook 'after-init-hook 'set-modeline-color-according-to-write-mode)

(provide 'set-modeline-color)

;;; set-modeline-color.el ends here
