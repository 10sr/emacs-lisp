;;; 10sr-extras.el --- 10sr extra utilities
;; Package-Version: 20160101.1532

;;; Commentary:

;;; Code:

(require 'dired)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gmail

(setq mail-interactive t
      send-mail-function 'smtpmail-send-it)
;; message-send-mail-function 'smtpmail-send-it
(set-variable 'smtpmail-smtp-server "smtp.gmail.com")
(set-variable 'smtpmail-smtp-service 587)
(set-variable 'smtpmail-starttls-credentials '(("smtp.gmail.com" 587
                                                "8.slashes@gmail.com" nil)))
(set-variable 'smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                            "8.slashes@gmail.com" nil)))
(set-variable 'user-mail-address "8.slashes@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fonts

(defun my-set-ascii-and-jp-font (list)
  "Set font configuration to LIST."
  (let ((fspec1 (if (> emacs-major-version 22)
                    ;; font spec is available in emacs23 and later
                    (font-spec :family (nth 2 list) :size (nth 3 list))
                  (cons (nth 2 list) "jisx0208.*")))
        (fspec2 (if (> emacs-major-version 22)
                    (font-spec :family (nth 2 list) :size (nth 3 list))
                  (cons (nth 2 list) "jisx0201.*"))))
    (set-face-attribute 'default nil
                        :family (nth 0 list)
                        :height (nth 1 list))
    (set-fontset-font "fontset-default"
                      'japanese-jisx0208
                      fspec1)
    (set-fontset-font "fontset-default"
                      'katakana-jisx0201
                      fspec2)))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 100 "takaogothic" 14))
;; (my-set-ascii-and-jp-font '("dejavu sans mono" 100 "ms gothic" 14))
;; (my-set-ascii-and-jp-font '("monaco" 75 "takaogothic" 11))
;; (my-set-ascii-and-jp-font '("monaco" 90 "takaogothic" 13))
;; (my-set-ascii-and-jp-font '("ProggyCleanTTSZ" 120 "takaogothic" 11))
;; あ a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system info

(defun my-message-current-info ()
  "Echo current login name, hostname and directory."
  (interactive)
  (message "%s@%s:%s"
           user-login-name
           system-name
           (abbreviate-file-name default-directory)))

;; (run-with-idle-timer 3
;;                      t
;;                      'my-message-current-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for windows

(defun 10sr-w32-add-export-path (&rest args)
  "Add pathes ARGS for windows."
  (mapc (lambda (path)
          (add-to-list 'exec-path (expand-file-name path)))
        (reverse args))
  (setenv "PATH"
          (mapconcat 'convert-standard-filename
                     exec-path
                     ";")))

(when (eq system-type 'windows-nt)
  ;; (setq scheme-program-name "\"c:/Program Files/Gauche/bin/gosh.exe\" -i")
  ;; (setq python-python-command "c:/Python26/python.exe")

  ;; (define-key my-prefix-map (kbd "C-c") 'start-ckw-bash)
  (my-w32-add-export-path "c:/Windows/system"
                          "c:/Windows/System32"
                          "c:/Program Files/Git/bin"
                          "c:/MinGW/bin"
                          "c:/MinGW/mingw32/bin"
                          (expand-file-name "~/.local/bin")
                          (expand-file-name "~/dbx/apps/bin"))

  (when window-system
    (set-variable 'w32-enable-synthesized-fonts t))
  (set-variable 'w32-apps-modifier 'meta)
  (setq file-name-coding-system 'sjis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; japanese input method

(defun 10sr-load-scim ()
  "Use scim-bridge.el as japanese im."
  ;; Load scim-bridge.
  (when (require 'scim-bridge nil t)
    ;; Turn on scim-mode automatically after loading .emacs
    (scim-mode-on)
    (set-variable 'scim-cursor-color "red")
    (scim-define-preedit-key ?\^h t)
    (scim-define-common-key ?\* nil)
    (scim-define-common-key ?\^/ nil)))

(defun 10sr-load-anthy ()
  "Use anthy.el as japanese im."
  ;; anthy
  (when (require 'anthy nil t)
    (global-set-key
     (kbd "<muhenkan>") (lambda () (interactive) (anthy-mode-off)))
    (global-set-key (kbd "<henkan>") (lambda () (interactive) (anthy-mode-on)))
    (when (>= emacs-major-version 23)
      (set-variable 'anthy-accept-timeout 1))))

;; quail
;; aproposs input-method for some information
;; (setq default-input-method "japanese")
(defun 10sr-load-mozc-el ()
  "Use mozc.el as japanese im."
  (when (require 'mozc nil t)
    (set-variable 'defauit-input-method "japanese-mozc")
    (set-variable 'mozc-leim-title "[MZ]")
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired

(defun my-replace-nasi-none ()
  ""
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (search-forward "なし" nil t)
        (replace-match "none")))))

(defun my-dired-print-current-dir-and-file ()
  (message "%s  %s"
           default-directory
           (buffer-substring-no-properties (point-at-bol)
                                           (point-at-eol))))

(defun dired-do-execute-as-command ()
  ""
  (interactive)
  (let ((file (dired-get-filename t)))
    (if (file-executable-p file)
        (start-process file nil file)
      (when (y-or-n-p
             "This file cant be executed.  Mark as executable and go? ")
        (set-file-modes file
                        (file-modes-symbolic-to-number "u+x" (file-modes file)))
        (start-process file nil file)))))

(defun my-dired-x-open ()
  ""
  (interactive)
  (my-x-open (dired-get-filename t t)))

(defun my-pop-to-buffer-erase-noselect (buffer-or-name)
  "pop up buffer using `display-buffer' and return that buffer."
  (let ((bf (get-buffer-create buffer-or-name)))
    (with-current-buffer bf
      (cd ".")
      (erase-buffer))
    (display-buffer bf)
    bf))


;; dired mark
;; http://blog.livedoor.jp/tek_nishi/archives/4693204.html

(defvar dired-marker-char)
(defun my-dired-toggle-mark()
  (let ((cur (cond ((eq (following-char) dired-marker-char) ?\040)
                   (t dired-marker-char))))
    (delete-char 1)
    (insert cur)))

(defun my-dired-mark (arg)
  "Toggle mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks
and \\[dired-unmark] on a subdir to remove the marks in
this subdir."

  (interactive "P")
  (if (dired-get-subdir)
      (save-excursion (dired-mark-subdir-files))
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       'my-dired-toggle-mark))))

(defun my-dired-mark-backward (arg)
  "In Dired, move up lines and toggle mark there.
Optional prefix ARG says how many lines to unflag; default is one line."
  (interactive "p")
  (my-dired-mark (- arg)))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "SPC") 'my-dired-mark)
            (local-set-key (kbd "S-SPC") 'my-dired-mark-backward))
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x open

(defvar my-filer nil)
(setq my-filer (or (executable-find "pcmanfm")
                   (executable-find "nautilus")))
(defun my-x-open (file)
  "Open FILE."
  (interactive "FOpen File: ")
  (setq file (expand-file-name file))
  (message "Opening %s..." file)
  (cond ((eq system-type 'windows-nt)
         (call-process "cmd.exe" nil 0 nil
                       "/c" "start" "" (convert-standard-filename file)))
        ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        ((getenv "DISPLAY")
         (call-process (or my-filer "xdg-open") nil 0 nil file))
        (t
         (find-file file))
        )
  ;; (recentf-add-file file)
  (message "Opening %s...done" file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(defun my-git-apply-index-from-buffer (&optional buf)
  "Git apply buffer.  BUF is buffer to apply.  nil to use current buffer."
  (interactive)
  (let ((buf (or buf
                 (current-buffer)))
        (file (make-temp-file "git-apply-diff.emacs")))
    (with-current-buffer buf
      (write-region (point-min)
                    (point-max)
                    file)
      (call-process "git"
                    nil
                    nil
                    nil
                    "apply"
                    "--cached"
                    file))))


(defvar sed-in-place-history nil
  "History of `sed-in-place'.")

(defvar sed-in-place-command "sed --in-place=.bak -e")
(defun sed-in-place (command)
  "Issue sed in place COMMAND."
  (interactive (list (read-shell-command "sed in place: "
                                         (concat sed-in-place-command " ")
                                         'sed-in-place-history)))
  (shell-command command
                 "*sed in place*"))
(defun dired-do-sed-in-place (&optional arg)
  "Issue sed in place dired.  If ARG is given, use the next ARG files."
  (interactive "p")
  (require 'dired-aux)
  (let* ((files (dired-get-marked-files t arg))
         (expr (dired-mark-read-string "Run sed-in-place for %s: "
                                       nil
                                       'sed-in-place
                                       arg
                                       files)))
    (if (equal expr
               "")
        (error "No expression specified")
      (shell-command (concat sed-in-place-command
                             " '"
                             expr
                             "' "
                             (mapconcat 'shell-quote-argument
                                        files
                                        " "))
                     "*sed in place*"))))

(defun dir-show (&optional dir)
  "Show DIR list."
  (interactive)
  (let ((bf (get-buffer-create "*dir show*"))
        (list-directory-brief-switches "-C"))
    (with-current-buffer bf
      (list-directory (or nil
                          default-directory)
                      nil))
    ))

(defun my-convmv-sjis2utf8-test ()
  "Run `convmv -r -f sjis -t utf8 *'.
this is test, does not rename files."
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 *"))

(defun my-convmv-sjis2utf8-notest ()
  "Run `convmv -r -f sjis -t utf8 * --notest'."
  (interactive)
  (shell-command "convmv -r -f sjis -t utf8 * --notest"))

(defun kill-ring-save-buffer-file-name ()
  "Get current filename."
  (interactive)
  (let ((file buffer-file-name))
    (if file
        (progn (kill-new file)
               (message file))
      (message "not visiting file."))))

(defvar kill-ring-buffer-name "*kill-ring*"
  "Buffer name for `kill-ring-buffer'.")
(defun open-kill-ring-buffer ()
  "Open kill- ring buffer."
  (interactive)
  (pop-to-buffer
   (with-current-buffer (get-buffer-create kill-ring-buffer-name)
     (erase-buffer)
     (yank)
     (text-mode)
     (current-local-map)
     (goto-char (point-min))
     (yank)
     (current-buffer))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; savage emacs
;; ;; when enabled emacs fails to complete
;; ;; http://e-arrows.sakura.ne.jp/2010/05/emacs-should-be-more-savage.html
;; (defadvice message (before message-for-stupid (arg &rest arg2) activate)
;;   (setq arg
;;         (concat arg
;;                 (if (eq nil
;;                         (string-match "\\. *$"
;;                                       arg))
;;                     ".")
;;                 " Stupid!")))


(defun set-terminal-header (string)
  "Set terminal header STRING."
  (let ((savepos "\033[s")
        (restorepos "\033[u")
        (movecursor "\033[0;%dH")
        (inverse "\033[7m")
        (restorecolor "\033[0m")
        (cols (frame-parameter nil 'width))
        (length (length string)))
    ;; (redraw-frame (selected-frame))
    (send-string-to-terminal (concat savepos
                                     (format movecursor
                                             (1+ (- cols length)))
                                     inverse
                                     string
                                     restorecolor
                                     restorepos))
    ))

(defun my-set-terminal-header ()
  "Set terminal header."
  (interactive)
  (set-terminal-header (concat " "
                               user-login-name
                               "@"
                               (car (split-string system-name
                                                  "\\."))
                               " "
                               (format-time-string "%Y/%m/%d %T %z")
                               " ")))

;; (run-with-timer
;;  0.1
;;  1
;;  'my-set-terminal-header)

(provide '10sr-extras)
;;; 10sr-extras.el ends here
