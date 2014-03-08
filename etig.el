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

;; internal variables

(defvar etig--ansi-color-library
  (locate-library "ansi-color")
  "Non-nil if library `ansi-color' exists.")

(defvar etig--window-configurations-stack nil
  "etig window configuration stack. At bottom of this stack is the one before
etig is invoked.")

(defvar etig--parent-buffer nil
  "etig parent buffer.")
(make-variable-buffer-local 'etig--parent-buffer)

;; (defvar etig--create-buffer-function nil
;;   "Function to create new buffer from current line.
;; This function accept no args and return newly created buffer or nil of no
;; apropriate buffer.")
;; (make-variable-buffer-local 'etig--create-buffer-function)

(defvar etig--next-create-buffer-function 'ignore
  "Function to create a buffer for next item of current buffer.
It accept two argument, parent buffer and number of movement.")
(make-variable-buffer-local 'etig--next-create-buffer-function)




;; utilities

(defun etig--pop-window-configuration (&optional destroy-p)
  "Pop window configuation and set to current state unless destroy-p is nil."
  (message "stack poped")
  (let ((wc (car etig--window-configurations-stack)))
    (setq etig--window-configurations-stack
          (cdr etig--window-configurations-stack))
    (when (and wc
               (not destroy-p))
      (set-window-configuration wc))
    wc))

;; (defun etig--replace-window-configuration ()
;;   "Pop and destroy window configuration, then push current window
;; configuration."
;;   (etig--pop-window-configuration t)
;;   (etig--push-window-configuration))

(defun etig--push-window-configuration ()
  "Push current window configuration to stack."
  (message "stack pushed")
  (setq etig--window-configurations-stack
        (cons (current-window-configuration)
              etig--window-configurations-stack)))

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

;; (defun etig--create-buffer (&optional parent)
;;   "Create etig buffer according to current mode. Return that buffer."
;;   (save-excursion
;;     (if etig--create-buffer-function
;;         (let ((buf (funcall etig--create-buffer-function)))
;;           (when buf
;;             (with-current-buffer buf
;;               (goto-char (point-min))
;;               (and parent
;;                    (setq etig--parent-buffer parent)))
;;             buf))
;;       (message "I dont know how to create new buffer")
;;       nil)))

(defvar etig--base-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-m") 'etig-enter-key)
    (define-key map (kbd "C-n") 'etig-open-next-item)
    (define-key map (kbd "C-p") 'etig-open-previous-item)
    (define-key map (kbd "C-i") 'etig-other-window)
    (define-key map "j" 'etig-next-line)
    (define-key map "k" 'etig-previous-line)
    (define-key map "q" 'etig-quit-buffer)
    map)
  "Base keymap for major-modes of etig.el . Keymaps of other major-modes are
a copy of this var.")



;; modes

;; etig-xxx will create and return etig xxx buffer *without* changing window
;; configuration

;;; views

;; diff view buffer

(defvar etig-diff-mode-map
  (let ((map (copy-keymap etig--base-map)))
    ;; (define-key map (kbd "C-n") 'etig-diff-open-next)
    ;; (define-key map (kbd "C-p") 'etig-diff-open-previous)
    map))

(define-derived-mode etig-diff-mode diff-mode
  "etig-diff"
  "diff-mode for etig."
  (setq buffer-read-only t)
  )


(defun etig-diff (sha1)
  "Open etig-diff buffer of commit of SHA1."
  (save-window-excursion
    (let ((buf (get-buffer-create (format "*etig-diff<%s>*"
                                          sha1))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (etig--command-str (concat "show " sha1)
                             buf nil 'etig-diff-mode))
        (goto-char (point-min)))
      (message "etig-diff: %s" sha1)
      buf)))

(defun etig-diff-open-next (n)
  "Open N next item."
  (interactive "p")
  (let ((win (get-buffer-window (current-buffer)))
        (parent etig--parent-buffer))
    (if parent
        (let ((buf (with-current-buffer parent
                     (forward-line 1)
                     (etig--create-buffer parent))))
          (when buf
            (set-window-buffer win buf)))
      (forward-line 1)
      (message "No parent buffer."))))






;; main buffer

(defvar etig-main-sha1-regexp
  "^[^1234567890abcdef]*\\([1234567890abcdef]*\\) "
  "Regexp to extract commit sha1 from main buffer.")

(defvar etig-main-command
  (concat "log --graph --date-order -C -M --all --date=iso "
          "--pretty=tformat:'%C(green)%h%C(reset) %C(white)%ad%C(reset) %C(red)%an%C(reset)%C(yellow)%d%C(reset) %C(white bold)%s%C(reset)'"
          )
  "git command used to open main")

(defvar etig-main-mode-map
  (let ((map (copy-keymap etig--base-map)))
    (define-key map (kbd "C-m") 'etig-main-open-current)
    map))

(define-derived-mode etig-main-mode text-mode
  "etig-main"
  "main mode for etig."
  (setq buffer-read-only t))

(defun etig-main (&optional dir)
  "Create and return new etig main buffer."
  (interactive)
  (save-window-excursion
    (let ((buf (generate-new-buffer "*etig-main*")))
      (with-current-buffer buf
        (erase-buffer)
        (and dir
             (cd dir))
        (etig--command-str etig-main-command buf t 'etig-main-mode)
        (goto-char (point-min)))
      buf)))

(defun etig-main-open-current ()
  "Open commit of current line."
  (interactive)
  (let ((sha1 (etig--main-extract-current-sha1)))
    (if sha1
        (let ((buf (etig--main-create-diff-buffer sha1)))
          (when buf
            (etig--push-window-configuration)
            (pop-to-buffer buf)))
      (message "No commit found on this line")
      nil)))

(defun etig--main-create-diff-buffer (sha1)
  (let ((parent (current-buffer))
        (buf (etig-diff sha1)))
    (when buf
      (with-current-buffer buf
        (setq etig--parent-buffer
              parent)
        (setq etig--next-create-buffer-function
              'etig--main-next-create-diff-buffer))
      buf)))

;; TODO: highlight current line
(defun etig--main-next-create-diff-buffer (parent n)
  (with-current-buffer parent
    (let ((sha1 (etig--main-extract-current-sha1)))
      (if (eq n 0)
          (if sha1
              (etig--main-create-diff-buffer sha1)
            ;; wont happen because n will not be decremented when sha1 is nil
            (etig--main-next-create-diff-buffer parent
                                                0))
        ;; n is not 0
        (progn
          (if (< 0 n)
              (forward-line 1)
            (forward-line -1))
          (if sha1
              (if (< 0 n)
                  ;; n is positive
                  (etig--main-next-create-diff-buffer parent
                                                      (- n 1))
                ;; n is negative
                (etig--main-next-create-diff-buffer parent
                                                    (+ n 1)))
            (etig--main-next-create-diff-buffer parent
                                                n)))))))

(defun etig--main-extract-current-sha1 ()
  "Extract sha1 of current line."
  (etig--main-extract-sha1 (buffer-substring-no-properties (point-at-bol)
                                                           (point-at-eol))))

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

(defun etig-other-window ()
  "Focus to other etig window."
  (interactive)
  (other-window 1))

(defun etig-open-next-item (n)
  "Enter next item."
  (interactive "p")
  (and etig--next-create-buffer-function
       etig--parent-buffer
       (let ((buf (funcall etig--next-create-buffer-function
                           etig--parent-buffer
                           n))
             (win (get-buffer-window (current-buffer))))
         (when buf
           (set-window-buffer win
                              buf)))))

(defun etig-open-previous-item (n)
  "Enter previous item."
  (interactive "p")
  (etig-open-next-item (- n)))

(defalias 'etig-next-line 'next-line)
(defalias 'etig-previous-line 'previous-line)

(defun etig-quit-buffer ()
  "etig quit"
  (interactive)
  (etig--pop-window-configuration))


;; open etig buffer

(defun etig-open (&optional dir)
  "Invole etig."
  (interactive)
  (etig--push-window-configuration)
  (switch-to-buffer (etig-main dir))
  (delete-other-windows))


(provide 'etig)

;;; etig.el ends here.
