;;; end-mark.el --- Show mark at the end of buffer
;; Package-Version: 20120708.802
;; Package-Commit: b9f6a7c1002248ce0f6a56bc720dafb5b77437be

;; Author: INA Lintaro <ina@kuis.kyoto-u.ac.jp>

;;; License:

;; NYSL Version 0.9982 (en)
;; ----------------------------------------
;; A. This software is "Everyone'sWare". It means:
;;   Anybody who has this software can use it as if you're
;;   the author.
;;
;;   A-1. Freeware. No fee is required.
;;   A-2. You can freely redistribute this software.
;;   A-3. You can freely modify this software. And the source
;;       may be used in any software with no limitation.
;;   A-4. When you release a modified version to public, you
;;       must publish it with your name.
;;
;; B. The author is not responsible for any kind of damages or loss
;;   while using or misusing this software, which is distributed
;;   "AS IS". No warranty of any kind is expressed or implied.
;;   You use AT YOUR OWN RISK.
;;
;; C. Copyrighted to INA Lintaro
;;
;; D. Above three clauses are applied both to source and binary
;;   form of this software.

;;; Commentary:

;; Show mark at the end of buffer.
;;
;; To use this mode, copy end-mark.el to your load path
;; and add to your .emacs:
;;
;;   (require 'end-mark)
;;
;; Then M-x end-mark-on enables end-mark-mode in the current buffer.
;;
;; To automatically enable end-mark-mode in every buffers, add to your .emacs:
;;
;;   (global-end-mark-mode)

;;; Code:

(defconst end-mark-version "0.01")

(defgroup end-mark nil
  "Show mark at the end of buffer"
  :group 'convenience)

(defcustom end-mark-string "[EOF]"
  "String used to indicate the end of buffer."
  :group 'end-mark
  :type 'string)

(defface end-mark-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "seagreen1")
    (((class color) (min-colors 88) (background light))
     :foreground "seagreen3")
    (((class color) (min-colors 16))
     :foreground "brightgreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face of the end mark."
  :group 'end-mark)

(defcustom end-mark-face 'end-mark-face
  "Face of the end mark."
  :group 'end-mark
  :type 'face)

(defcustom end-mark-exclude-modes '(dired-mode hexl-mode term-mode)
  "List of major mode symbols not to enable end-mark-mode automatically."
  :group 'end-mark
  :type '(repeat (symbol :tag "Major Mode")))

(defcustom end-mark-mode-buffers-regexp '("^\\*scratch\\*$")
  "List of regular expressions of buffer names to enable end-mark-mode automatically."
  :group 'end-mark
  :type '(repeat 'string))

(defcustom end-mark-exclude-buffers-regexp '("^ .*" "^\\*")
  "List of regular expressions of buffer names not to enable end-mark-mode automatically."
  :group 'end-mark
  :type '(repeat 'string))

(defvar end-mark-overlay nil)
(make-variable-buffer-local 'end-mark-overlay)

(defun end-mark-overlay-p () end-mark-overlay)

(defun end-mark-adjust ()
  (interactive)
  (when (end-mark-overlay-p)
    (move-overlay end-mark-overlay (point-max) (point-max))))

;;;###autoload
(define-minor-mode end-mark-mode
  "Toggle display of mark at the end of buffer."
  :lighter ""                           ; for desktop.el
  (if end-mark-mode
    (progn
      ;; destructor
      (make-local-variable 'change-major-mode-hook)
      (add-hook 'change-major-mode-hook 'end-mark-off)
      ;; end mark object
      (unless (end-mark-overlay-p)
        (setq end-mark-overlay (make-overlay (point-max) (point-max))))
      ;; overlay face
      (set-text-properties 0 (length end-mark-string)
                           `(face ,end-mark-face) end-mark-string)
      (overlay-put end-mark-overlay 'after-string end-mark-string)
      ;; auto adjust
      (overlay-put end-mark-overlay 'insert-behind-hooks
                   '((lambda (overlay after beg end &optional len)
                       (when after (end-mark-adjust))))))
    (when (end-mark-overlay-p)
      (delete-overlay end-mark-overlay)
      (setq end-mark-overlay nil))
    (remove-hook 'change-major-mode-hook 'end-mark-off t)))

;;;###autoload
(define-globalized-minor-mode
  global-end-mark-mode end-mark-mode end-mark-install)

(defun end-mark-off ()
  (interactive)
  (end-mark-mode 0))

(defun end-mark-on ()
  (interactive)
  (end-mark-mode 1))

;;; adjust when inserting file
(add-hook 'after-insert-file-functions
          '(lambda (count) (end-mark-adjust) count))

;;; install
(defun end-mark-install ()
  (let ((buf (buffer-name (current-buffer)))
        (mem-pat
         '(lambda (x l)
            (member t (mapcar '(lambda (r) (when (string-match r x) t)) l)))))
    (when (and (not (minibufferp))
               (not (buffer-base-buffer))
               (or (funcall mem-pat buf end-mark-mode-buffers-regexp)
                   (not (funcall mem-pat buf end-mark-exclude-buffers-regexp)))
               (null (memq major-mode end-mark-exclude-modes)))
      (end-mark-on))))

(provide 'end-mark)
;;; end-mark.el ends here
