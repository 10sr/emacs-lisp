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

(defvar etig--ansi-color-library
  (locate-library "ansi-color")
  "Non-nil if library `ansi-color' exists.")

(defvar etig--window-configurations-stack nil
  "etig configuration stack. At bottom of this stack is the one before etig
is invoked, at top is current one.")

(defvar etig--parent-buffer nil
  "etig parent buffer.")
(make-variable-buffer-local 'etig--parent-buffer)

(defvar etig--create-buffer-function nil
  "Function to create new buffer from current line.
This function accept no args and return newly created buffer or nil of no
apropriate buffer.")
(make-variable-buffer-local 'etig--create-buffer-function)

;; utilities

(defun etig--pop-window-configuration (&optional destroy-p)
  "Pop window configuation and set to current state unless destroy-p is nil."
  (let ((wc (pop etig--window-configurations-stack)))
    (when (and wc
               (not destroy-p))
      (set-window-configuration wc))
    wc))

(defun etig--replace-window-configuration ()
  "Pop and destroy window configuration, then push current window
configuration."
  (etig--pop-window-configuration t)
  (etig--push-window-configuration))

(defun etig--push-window-configuration ()
  "Push current window configuration to stack."
  (push (current-window-configuration)
        etig--window-configurations-stack))

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


;;; views

;; diff view buffer

(defvar etig-diff-mode-map
  (copy-keymap etig--base-map))

(define-derived-mode etig-diff-mode diff-mode
  "etig-diff"
  "diff-mode for etig."
  (view-mode t))


(defun etig-diff (sha1)
  "Open etig-diff buffer of commit of SHA1."
  (let ((buf (generate-new-buffer "*etig-diff*")))
    (etig--command-str (concat "show " sha1)
                       buf nil 'etig-diff-mode)
    (message "etig-diff: %s" sha1)
    buf))

;; main buffer

(defvar etig-main-sha1-regexp
  "^[^1234567890abcdef]*\\([1234567890abcdef]*\\) "
  "Regexp to extract commit sha1 from main buffer.")

(defvar etig-main-command
  (concat "log --graph --date-order -C -M --all --date=iso "
          "--pretty=tformat:'%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)'"
          )
  "git command used to open log")

(defvar etig-main-mode-map
  (copy-keymap etig--base-map))

(define-derived-mode etig-main-mode text-mode
  "etig-main"
  "main mode for etig."
  (setq etig--create-buffer-function 'etig--main-create-buffer-function))

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

(defun etig--main-create-buffer-function ()
  "Open commit of current line. Return buffer."
  (let ((sha1 (etig--main-extract-sha1
               (buffer-substring-no-properties (point-at-bol)
                                               (point-at-eol)))))
    (if sha1
        (etig-diff sha1)
      (message "No commit found on this line")
      nil)))

(defun etig--main-extract-sha1 (line)
  (let ((str
         ;;"| * | 85d2901 2013-09-21 18:15:00 +0900 10sr alias make as compile"
         line))
    (and (string-match etig-main-sha1-regexp
                       str)
         (let ((match (match-string 1 str)))
           (if (eq 0
                   (length match))
               nil
             match)))))


;; interactive command for etig modes

(defun etig-enter ()
  "Function for enter key."
  (interactive)
  (let ((parent (current-buffer))
        (buf (etig--create-buffer)))
    (when buf
      (pop-to-buffer buf)               ; switch buffer
      (setq etig--parent-buffer parent)
      (etig--push-window-configuration))))

(defun etig--create-buffer (&optional parent)
  "Create etig buffer according to current mode. Return that buffer."
  (save-excursion
    (if etig--create-buffer-function
        (let ((buf (funcall etig--create-buffer-function)))
          (when buf
            (with-current-buffer buf
              (goto-char (point-min))
              (and parent
                   (setq etig--parent-buffer parent)))
            buf))
      (message "I dont know how to create new buffer")
      nil)))

(defun etig-enter-next ()
  "Enter next item."
  (interactive)
  (let ((win (get-buffer-window (current-buffer)))
        (parent etig--parent-buffer))
    (if parent
        (with-current-buffer parent
          (forward-line 1)
          (let ((buf (etig--create-buffer parent)))
            (when buf
              (set-window-buffer win buf)
              (switch-to-buffer buf)
              (etig--replace-window-configuration))))
      (message "No parent buffer."))))

(defun etig-enter-previous ()
  "Enter previous item."
  (interactive)
  (let ((win (get-buffer-window (current-buffer)))
        (parent etig--parent-buffer))
    (if parent
        (with-current-buffer parent
          (forward-line -1)
          (let ((buf (etig--create-buffer parent)))
            (when buf
              (set-window-buffer win buf)
              (switch-to-buffer buf)
              (etig--replace-window-configuration))))
      (message "No parent buffer."))))

(defalias 'etig-next-line 'next-line)
(defalias 'etig-previous-line 'previous-line)

(defun etig-quit-buffer ()
  "etig quit"
  (interactive)
  (etig--pop-window-configuration))

(provide 'etig)

;;; etig.el ends here.
