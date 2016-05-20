;;; 10sr-eshell.el --- 10sr Eshell configurations

;; Author: 10sr <>
;; Version: 0.0.1
;; Package-Version: 20160502.2329
;; URL: https://github.com/10sr/emacs-lisp/blob/master/10sr-eshell.el

;;; Commentary:

;;; Code:

(require 'eshell)
(require 'em-hist)

(set-variable 'eshell-banner-message (format "Welcome to the Emacs shell
%s
C-x t to toggling emacs-text-mode

"
                                             (shell-command-to-string "uname -a")
                                             ))

(defvar eshell-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x t") 'eshell-text-mode-toggle)
    map))

(define-derived-mode eshell-text-mode text-mode
  "Eshell-Text"
  "Text-mode for Eshell."
  nil)

(defun eshell-text-mode-toggle ()
  "Toggle eshell-text-mode and eshell-mode."
  (interactive)
  (cond ((eq major-mode
             'eshell-text-mode)
         (goto-char (point-max))
         (message "Eshell text mode disabled")
         (eshell-mode))
        ((eq major-mode
             'eshell-mode)
         (message "Eshell text mode enabled")
         (eshell-write-history)
         (eshell-text-mode))
        (t
         (message "Not in eshell buffer")
         nil)))

(defun my-eshell-backward-delete-char ()
  (interactive)
  (when (< (save-excursion
             (eshell-bol)
             (point))
           (point))
    (backward-delete-char 1)))

(defun my-file-owner-p (file)
  "t if FILE is owned by me."
  (eq (user-uid) (nth 2 (file-attributes file))))

"http://www.bookshelf.jp/pukiwiki/pukiwiki.php\
?Eshell%A4%F2%BB%C8%A4%A4%A4%B3%A4%CA%A4%B9"
;; ;; written by Stefan Reichoer <reichoer@web.de>
;; (defun eshell/less (&rest args)
;;   "Invoke `view-file' on the file.
;; \"less +42 foo\" also goes to line 42 in the buffer."
;;   (if args
;;       (while args
;;         (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
;;             (let* ((line (string-to-number (match-string 1 (pop args))))
;;                    (file (pop args)))
;;               (view-file file)
;;               (goto-line line))
;;           (view-file (pop args))))))

;; (defun eshell/o (&optional file)
;;   (my-x-open (or file ".")))

;; (defun eshell/vi (&rest args)
;;   "Invoke `find-file' on the file.
;; \"vi +42 foo\" also goes to line 42 in the buffer."
;;   (while args
;;     (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
;;         (let* ((line (string-to-number (match-string 1 (pop args))))
;;                (file (pop args)))
;;           (find-file file)
;;           (goto-line line))
;;       (find-file (pop args)))))

(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defvar eshell-prompt-function)
(defun eshell-clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (funcall eshell-prompt-function))))

(defun eshell/d (&optional dirname switches)
  "if first arg is omitted open current directory."
  (dired (or dirname ".") switches))

(defun eshell/v ()
  (view-mode 1))

;; (defun eshell/aaa (&rest args)
;;   (message "%S"
;;            args))

(defalias 'eshell/: 'ignore)
(defalias 'eshell/type 'eshell/which)
;; (defalias 'eshell/vim 'eshell/vi)
(defalias 'eshell/ff 'find-file)
(defalias 'eshell/q 'eshell/exit)

(defun eshell-goto-prompt ()
  ""
  (interactive)
  (goto-char (point-max)))

(defun eshell-delete-char-or-logout (n)
  (interactive "p")
  (if (equal (eshell-get-old-input)
             "")
      (progn
        (insert "exit")
        (eshell-send-input))
    (delete-char n)))

(defun eshell-kill-input ()
  (interactive)
  (delete-region (point)
                 (progn (eshell-bol)
                        (point))))

(defalias 'eshell/logout 'eshell/exit)

(defun eshell-cd-default-directory (&optional eshell-buffer-or-name)
  "open eshell and change wd
if arg given, use that eshell buffer, otherwise make new eshell buffer."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (switch-to-buffer (or eshell-buffer-or-name
                          (eshell t)))
    (unless (equal dir (expand-file-name default-directory))
      ;; (cd dir)
      ;; (eshell-interactive-print (concat "cd " dir "\n"))
      ;; (eshell-emit-prompt)
      (goto-char (point-max))
      (eshell-kill-input)
      (insert "cd " dir)
      (eshell-send-input))))

(defadvice eshell-next-matching-input-from-input
    ;; do not cycle history
    (around eshell-history-do-not-cycle activate)
  (if (= 0
         (or eshell-history-index
             0))
      (progn
        (delete-region eshell-last-output-end (point))
        (insert-and-inherit eshell-matching-input-from-input-string)
        (setq eshell-history-index nil))
    ad-do-it))

(set-variable 'eshell-directory-name (concat user-emacs-directory
                                             "eshell/"))
(set-variable 'eshell-term-name "eterm-color")
(set-variable 'eshell-scroll-to-bottom-on-input 'this)
(set-variable 'eshell-cmpl-ignore-case t)
(set-variable 'eshell-cmpl-cycle-completions nil)
(set-variable 'eshell-highlight-prompt nil)
(if (eq system-type 'darwin)
    (set-variable 'eshell-ls-initial-args '("-hCFG")
                  (set-variable 'eshell-ls-initial-args '("-hCFG"
                                                          "--color=auto"
                                                          "--time-style=long-iso"))     ; "-hF")
                  ))

(set (defvar eshell-prompt-function)
     'my-eshell-prompt-function)

(defvar eshell-last-command-status)
(defun my-eshell-prompt-function()
  "Prompt function.

It looks like:

:: [10sr@darwin:~/][ESHELL]
:: $
"
  (concat ":: ["
          (let ((str (concat user-login-name
                             "@"
                             (car (split-string system-name
                                                "\\."))
                             )))
            (put-text-property 0
                               (length str)
                               'face
                               'underline
                               str)
            str)
          ":"
          (let ((str (abbreviate-file-name default-directory)))
            (put-text-property 0
                               (length str)
                               'face
                               'underline
                               str)
            str)
          "][ESHELL]\n:: "
          (if (eq 0
                  eshell-last-command-status)
              ""
            (format "[STATUS:%d] "
                    eshell-last-command-status))
          (if (= (user-uid)
                 0)
              "# "
            "$ ")
          ))

(with-eval-after-load 'eshell
  (defvar eshell-mode-map (make-sparse-keymap))
  ;; (define-key eshell-mode-map (kbd "C-x C-x") (lambda ()
  ;;                                               (interactive)
  ;;                             (switch-to-buffer (other-buffer))))
  ;; (define-key eshell-mode-map (kbd "C-g") (lambda ()
  ;;                                           (interactive)
  ;;                                           (eshell-goto-prompt)
  ;;                                           (keyboard-quit)))
  (define-key eshell-mode-map (kbd "C-x t") 'eshell-text-mode-toggle)
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delete-char-or-logout)
  ;; (define-key eshell-mode-map (kbd "C-l")
  ;;   'eshell-clear)
  (define-key eshell-mode-map (kbd "DEL") 'my-eshell-backward-delete-char)
  (define-key eshell-mode-map (kbd "<up>") 'scroll-down-line)
  (define-key eshell-mode-map (kbd "<down>") 'scroll-up-line)
  ;; (define-key eshell-mode-map
  ;;   (kbd "C-p") 'eshell-previous-matching-input-from-input)
  ;; (define-key eshell-mode-map
  ;;   (kbd "C-n") 'eshell-next-matching-input-from-input)

  (defvar eshell-virtual-targets nil)
  (add-to-list 'eshell-virtual-targets
               '("/dev/less"
                 (lambda (str)
                   (if str
                       (with-current-buffer nil)))
                 nil))

  (defvar eshell-visual-commands nil)
  (add-to-list 'eshell-visual-commands "vim")

  (defvar eshell-output-filter-functions nil)
  (add-to-list 'eshell-output-filter-functions
               'eshell-truncate-buffer)

  (defvar eshell-command-aliases-list nil)
  (mapcar (lambda (alias)
            (add-to-list 'eshell-command-aliases-list
                         alias))
          '(
            ;; ("ll" "ls -l $*")
            ;; ("la" "ls -a $*")
            ;; ("lla" "ls -al $*")
            ("git" "git -c color.ui=always $*")
            ("g" "git $*")
            ("eless"
             (concat "cat >>> (with-current-buffer "
                     "(get-buffer-create \"*eshell output\") "
                     "(erase-buffer) "
                     "(setq buffer-read-only nil) "
                     "(current-buffer)) "
                     "(view-buffer (get-buffer \"*eshell output*\"))"))
            ))
  )

(add-hook 'eshell-mode-hook
          (lambda ()

            (apply 'eshell/addpath exec-path)
            (set (make-local-variable 'scroll-margin) 0)
            ;; (eshell/export "GIT_PAGER=")
            ;; (eshell/export "GIT_EDITOR=")
            (eshell/export "LC_MESSAGES=C")
            (switch-to-buffer (current-buffer)) ; move buffer top of list
            (set (make-local-variable (defvar hl-line-range-function))
                 (lambda ()
                   '(0 . 0)))

            ;; (add-to-list 'eshell-visual-commands "git")
            ))

(provide '10sr-eshell)
;;; 10sr-eshell.el ends here
