;;; etig.el --- tig clone for emacs

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/etig.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility git

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

;;; Comentary:

;; tig clone for emacs.

;;; Code:

;; variables

(defvar etig-sha1-regexp
  "^[^1234567890abcdef]*\\([1234567890abcdef]*\\) "
  "Regexp to extract commit sha1 from main buffer.")

(defvar etig-main-command
  (concat "log --graph --date-order -C -M --all --date=iso "
          "--pretty=tformat:'%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)'"
          )
  "git command used to open log")

(defvar etig--ansi-color-library
  (locate-library "ansi-color")
  "Non-nil if library `ansi-color' exists.")

(defvar etig--window-configurations-stack nil
  "etig configuration stack. Bottom of this stack is the one before etig
is invoked. Top is current one.")

(defvar etig--parent-buffer nil
  "etig parent buffer.")
(make-variable-buffer-local 'etig--parent-buffer)

;; utilities

(defun etig--pop-window-configuration ()
  "Pop window configuation and set to current state."
  nil)

(defun etig--push-window-configuration ()
  "Push current window configuration to stack."
  nil)

(defun etig--command-str (cmd buf color-p mode)
  "Run git command CMD and output result to BUF.
CMD should be a string of git command."
  (let ((color-enabled (and color-p
                            etig--ansi-color-library)))
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (call-process "sh" nil (current-buffer) nil
                    "-c"
                    (concat "git "
                            (if color-enabled "-c color.ui=always "
                              "")
                            cmd))
      (when color-enabled
        (require 'ansi-color)
        (ansi-color-apply-on-region (point-min) (point-max)))
      (when mode
        (funcall mode)))))

(defun etig--command (cmds buf color-p mode)
  "Run git command CMDS and output result to BUF.
CMDS should be a list of args for git."
  (etig--command-str (mapconcat 'shell-quote-argument
                            cmds
                            " ")
                 buf color-p mode))

(defun etig-git-repository-p ()
  "Return non-nil if git is installed and current directory is git repository."
  (and (executable-find "git")
       (eq 0 (call-process "git" nil nil nil "rev-parse" "--git-dir"))))

(defvar etig--base-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-m") 'etig-enter)
    (define-key map (kbd "C-n") 'etig-enter-next)
    (define-key map (kbd "C-p") 'etig-enter-previous)
    (define-key map "j" 'etig-next-line)
    (define-key map "k" 'etig-previous-line)
    (define-key map "q" 'etig-quit-buffer)
    map)
  "Base keymap for major-modes of etig.el . Keymaps of other major-modes are
a copy of this var.")


;; views

;; diff view

(defvar etig-diff-mode-map
  (copy-keymap etig--base-map))

(define-derived-mode etig-diff-mode diff-mode
  "diff-mode for etig.")


(defun etig-diff ()
  "Open commit of current line. Return buffer."
  (let ((sha1 (etig--diff-extract-sha1
               (buffer-substring-no-properties (point-at-bol)
                                               (point-at-eol)))))
    (if sha1
        (etig-diff-open-buffer sha1)
      (message "No commit found in this line"))))

(defun etig-diff-open-buffer (sha1)
  "Open etig-diff buffer of commit of SHA1."
  (let ((buf (generate-new-buffer "*etig-diff*")))
    (etig--command-str (concat "show " sha1)
                       buf nil 'etig-diff-mode)
    buf))

(defun etig--diff-extract-sha1 (line)
  (let ((str
         ;;"| * | 85d2901 2013-09-21 18:15:00 +0900 10sr alias make as compile"
         line))
    (and (string-match etig-sha1-regexp
                       str)
         (match-string 1 str))))

;; main

(defvar etig-main-mode-map
  (copy-keymap etig--base-map))

(define-derived-mode etig-main-mode text-mode
  "etig main"
  "main mode for etig.")

(defun etig-main ()
  "Invole etig."
  (interactive)
  (let ((buf (get-buffer-create "*etig-main*")))
    (with-current-buffer buf
      (erase-buffer)
      (etig--command-str etig-main-command buf t 'etig-main-mode))
    (etig--push-window-configuration)
    (delete-other-windows)
    (switch-to-buffer buf)
    (goto-char (point-min))))


;; interactive command for etig modes

(defun etig-enter ()
  "Function for enter key."
  (interactive)
  (let ((parent (current-buffer))
        (buf (save-excursion
               (case major-mode
                 ('etig-main-mode (message "etig-main-mode.") (etig-diff))
                 (t (message "Not etig modes.") nil)
                 ))))
    (when buf
      (pop-to-buffer buf)               ; switch buffer
      (setq etig--parent-buffer parent)
      (etig--push-window-configuration))))

(defalias 'etig-next-line 'next-line)
(defalias 'etig-previous-line 'previous-line)

(defun etig-quit-buffer ()
  "etig quit"
  (interactive)
  (etig--pop-window-configuration))

(provide 'etig)

;;; etig.el ends here.
