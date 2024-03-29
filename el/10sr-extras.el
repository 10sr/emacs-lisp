;;; 10sr-extras.el --- 10sr extra utilities

;; Author: 10sr <>
;; Version: 0.0.1
;; URL: https://github.com/10sr/emacs-lisp/blob/master/10sr-extras.el

;;; Commentary:

;;; Code:

(require 'dired)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; download library from web

(defvar fetch-library-enabled-p t
  "Set nil to skip downloading with `fetch-library'.")
(defun fetch-library (url &optional byte-compile-p force-download-p)
  "Download a library from URL and locate it in \"~/emacs.d/lisp/\".
Return nil if library unfound and failed to download,
otherwise the path where the library installed.
If BYTE-COMPILE-P is t byte compile the file after downloading.
If FORCE-DOWNLOAD-P it t ignore exisiting library and always download.

This function also checks the value of `fetch-library-enabled-p' and do not
fetch libraries if this value is nil.  In this case all arguments (including
FORCE-DOWNLOAD-P) will be ignored."
  (let* ((dir (expand-file-name (concat user-emacs-directory "lisp/")))
         (lib (file-name-sans-extension (file-name-nondirectory url)))
         (lpath (concat dir lib ".el"))
         (locate-p (locate-library lib)))
    (if (and fetch-library-enabled-p
             (or force-download-p
                 (not locate-p)))
        (if (progn (message "Downloading %s..."
                            url)
                   (download-file url
                                  lpath
                                  t))
            (progn (message "Downloading %s...done"
                            url)
                   (when (and byte-compile-p
                              (require 'bytecomp nil t))
                     (and (file-exists-p (byte-compile-dest-file lpath))
                          (delete-file (byte-compile-dest-file lpath)))
                     (message "Byte-compiling %s..."
                              lpath)
                     (byte-compile-file lpath)
                     (message "Byte-compiling %s...done"
                              lpath)))
          (progn (and (file-writable-p lpath)
                      (delete-file lpath))
                 (message "Downloading %s...failed"
                          url))))
    (locate-library lib)))

(defun download-file (url path &optional ok-if-already-exists)
  "Download file from URL and output to PATH.
IF OK-IF-ALREADY-EXISTS is true force download."
  (let ((curl (executable-find "curl"))
        (wget (executable-find "wget")))
    (cond (wget
           (if (and (not ok-if-already-exists)
                    (file-exists-p path))
               nil
             (and (eq 0
                      (call-process wget
                                    nil
                                    nil
                                    nil
                                    "-O"
                                    path
                                    url
                                    ))
                  path)))
          (curl
           (if (and (not ok-if-already-exists)
                    (file-exists-p path))
               nil
             (and (eq 0
                      (call-process curl
                                    nil
                                    nil
                                    nil
                                    "--output"
                                    path
                                    "-L"
                                    url
                                    ))
                  path)))
          (t
           (ignore-errors
             (require 'url)
             (url-copy-file url
                            path
                            ok-if-already-exists)
             path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sdic

(when (autoload-eval-lazily 'sdic '(sdic-describe-word-at-point))
  ;; (define-key my-prefix-map "\C-w" 'sdic-describe-word)
  (defvar sdic-buffer-name)
  ;;(define-key my-prefix-map "\C-t" 'sdic-describe-word-at-point-echo)
  (defun sdic-describe-word-at-point-echo ()
    ""
    (interactive)
    (save-window-excursion
      (sdic-describe-word-at-point))
    (with-current-buffer sdic-buffer-name
      (message (buffer-substring (point-min)
                                 (progn (goto-char (point-min))
                                        (or (and (re-search-forward "^\\w"
                                                                    nil
                                                                    t
                                                                    4)
                                                 (progn (forward-line -1) t)
                                                 (point-at-eol))
                                            (point-max)))))))

  (set-variable 'sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/gene.sdic")))
  (set-variable ' sdic-waei-dictionary-list
                  '((sdicf-client "/usr/share/dict/jedict.sdic" (add-keys-to-headword t))))
  (set-variable 'sdic-disable-select-window t)
  (set-variable ' sdic-window-height 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;; http://uguisu.skr.jp/Windows/gtags.html
;; http://eigyr.dip.jp/gtags.html
;; http://cha.la.coocan.jp/doc/gnu_global.html

(let ((d "/opt/local/share/gtags/"))
  (and (file-directory-p d)
       (add-to-list 'load-path
                    d)))

(when (autoload-eval-lazily 'gtags '(gtags-mode)
        ;; (local-set-key "\M-t" 'gtags-find-tag)
        ;; (local-set-key "\M-r" 'gtags-find-rtag)
        ;; (local-set-key "\M-s" 'gtags-find-symbol)
        ;; (local-set-key "\C-t" 'gtags-pop-stack)
        (defvar gtags-mode-map)
        (define-key gtags-mode-map (kbd "C-x t h")
          'gtags-find-tag-from-here)
        (define-key gtags-mode-map (kbd "C-x t t") 'gtags-find-tag)
        (define-key gtags-mode-map (kbd "C-x t r") 'gtags-find-rtag)
        (define-key gtags-mode-map (kbd "C-x t s") 'gtags-find-symbol)
        (define-key gtags-mode-map (kbd "C-x t p") 'gtags-find-pattern)
        (define-key gtags-mode-map (kbd "C-x t f") 'gtags-find-file)
        (define-key gtags-mode-map (kbd "C-x t b") 'gtags-pop-stack) ;back

        (defvar gtags-select-mode-map)
        (define-key gtags-select-mode-map (kbd "C-m") 'gtags-select-tag)
        )
  (add-hook 'gtags-mode-hook
            (lambda ()
              (view-mode 1)
              (set-variable 'gtags-select-buffer-single t)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some modes and hooks

;; (when (require 'ensime nil t)
;;   (set-variable 'ensime-ac-case-sensitive t)
;;   (set-variable 'ensime-company-case-sensitive t)
;;   (add-hook 'scala-mode-hook
;;             'ensime-scala-mode-hook)
;;   (add-hook 'ensime-scala-mode-hook
;;             'ac-stop))

;; (defun my-view-mode-search-word (word)
;;   "Search for word current directory and subdirectories.
;; If called intearctively, find word at point."
;;   (interactive (list (thing-at-point 'symbol)))
;;   (if word
;;       (if (and (require 'gtags nil t)
;;                (gtags-get-rootpath))
;;           (gtags-goto-tag word "s")
;;         (my-rgrep word))
;;     (message "No word at point.")
;;     nil))




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
  (interactive)
  ;; Load scim-bridge.
  (set-variable 'quail-japanese-use-double-n t)
  (when (require 'scim-bridge nil t)
    ;; Turn on scim-mode automatically after loading .emacs
    (scim-mode-on)
    (set-variable 'scim-cursor-color "red")
    (scim-define-preedit-key ?\^h t)
    (scim-define-common-key ?\* nil)
    (scim-define-common-key ?\^/ nil)))

(defun 10sr-load-anthy ()
  "Use anthy.el as japanese im."
  (interactive)
  ;; anthy
  (set-variable 'quail-japanese-use-double-n t)
  ;; http://cosmos.ge.ce.nihon-u.ac.jp/diary/20120817.html
  (unless (boundp 'last-command-char)
    (define-obsolete-variable-alias 'last-command-char
      'last-command-event
      "at least 19.34"))
  (define-key anthy-mode-map (kbd "SPC") 'self-insert-command)
  (set-variable 'default-input-method "japanese-anthy")
  ;; (when (require 'anthy nil t)
  ;;   (global-set-key
  ;;    (kbd "<muhenkan>") (lambda () (interactive) (anthy-mode-off)))
  ;;   (global-set-key (kbd "<henkan>") (lambda () (interactive) (anthy-mode-on))))
  (when (>= emacs-major-version 23)
    (set-variable 'anthy-accept-timeout 1)))

;; quail
;; aproposs input-method for some information
;; (setq default-input-method "japanese")
(defun 10sr-load-mozc-el ()
  "Use mozc.el as japanese im."
  (interactive)
  (set-variable 'quail-japanese-use-double-n t)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term mode

;; (setq multi-term-program shell-file-name)
(when (autoload-eval-lazily 'multi-term)
  (set-variable 'multi-term-switch-after-close nil)
  (set-variable 'multi-term-dedicated-select-after-open-p t)
  (set-variable 'multi-term-dedicated-window-height 20))

(when (autoload-eval-lazily 'term '(term ansi-term)
        (defvar term-raw-map (make-sparse-keymap))
        ;; (define-key term-raw-map "\C-xl" 'term-line-mode)
        ;; (define-key term-mode-map "\C-xc" 'term-char-mode)
        (define-key term-raw-map (kbd "<up>") 'scroll-down-line)
        (define-key term-raw-map (kbd "<down>") 'scroll-up-line)
        (define-key term-raw-map (kbd "<right>") 'scroll-up)
        (define-key term-raw-map (kbd "<left>") 'scroll-down)
        (define-key term-raw-map (kbd "C-p") 'term-send-raw)
        (define-key term-raw-map (kbd "C-n") 'term-send-raw)
        (define-key term-raw-map "q" 'my-term-quit-or-send-raw)
        ;; (define-key term-raw-map (kbd "ESC") 'term-send-raw)
        (define-key term-raw-map [delete] 'term-send-raw)
        (define-key term-raw-map (kbd "DEL") 'term-send-backspace)
        (define-key term-raw-map "\C-y" 'term-paste)
        (define-key term-raw-map
          "\C-c" 'term-send-raw) ;; 'term-interrupt-subjob)
        '(define-key term-mode-map (kbd "C-x C-q") 'term-pager-toggle)
        ;; (dolist (key '("<up>" "<down>" "<right>" "<left>"))
        ;;   (define-key term-raw-map (read-kbd-macro key) 'term-send-raw))
        ;; (define-key term-raw-map "\C-d" 'delete-char)
        ;; (define-key term-raw-map "\C-q" 'move-beginning-of-line)
        ;; (define-key term-raw-map "\C-r" 'term-send-raw)
        ;; (define-key term-raw-map "\C-s" 'term-send-raw)
        ;; (define-key term-raw-map "\C-f" 'forward-char)
        ;; (define-key term-raw-map "\C-b" 'backward-char)
        ;; (define-key term-raw-map "\C-t" 'set-mark-command)
        )
  (defun my-term-quit-or-send-raw ()
    ""
    (interactive)
    (if (get-buffer-process (current-buffer))
        (call-interactively 'term-send-raw)
      (kill-buffer)))

  ;; http://d.hatena.ne.jp/goinger/20100416/1271399150
  ;; (setq term-ansi-default-program shell-file-name)
  (add-hook 'term-setup-hook
            (lambda ()
              (set-variable 'term-display-table (make-display-table))))
  (add-hook 'term-mode-hook
            (lambda ()
              (defvar term-raw-map (make-sparse-keymap))
              ;; (unless (memq (current-buffer)
              ;;               (and (featurep 'multi-term)
              ;;                    (defvar multi-term-buffer-list)
              ;;                    ;; current buffer is not multi-term buffer
              ;;                    multi-term-buffer-list))
              ;;   )
              (set (make-local-variable 'scroll-margin) 0)
              ;; (set (make-local-variable 'cua-enable-cua-keys) nil)
              ;; (cua-mode 0)
              ;; (and cua-mode
              ;;      (local-unset-key (kbd "C-c")))
              ;; (define-key cua--prefix-override-keymap
              ;;"\C-c" 'term-interrupt-subjob)
              (set (make-local-variable (defvar hl-line-range-function))
                   (lambda ()
                     '(0 . 0)))
              (define-key term-raw-map
                "\C-x" (lookup-key (current-global-map) "\C-x"))
              (define-key term-raw-map
                "\C-z" (lookup-key (current-global-map) "\C-z"))
              ))
  ;; (add-hook 'term-exec-hook 'forward-char)
  )

;; my-term

(defvar my-term nil
  "My terminal buffer.")
(defvar my-term-function nil
  "Function to create terminal buffer.
This function accept no argument and return newly created buffer of terminal.")

(defun my-term (&optional arg)
  "Open terminal buffer and return that buffer.

If ARG is given or called with prefix argument, create new buffer."
  (interactive "P")
  (if (and (not arg)
           my-term
           (buffer-name my-term))
      (pop-to-buffer my-term)
    (setq my-term
          (save-window-excursion
            (funcall my-term-function)))
    (and my-term
         (my-term))))


;; (setq my-term-function
;;       (lambda ()
;;         (if (eq system-type 'windows-nt)
;;             (eshell)
;;           (if (require 'multi-term nil t)
;;               (multi-term)
;;             (ansi-term shell-file-name)))))

(setq my-term-function (lambda () (eshell t)))
;;(define-key my-prefix-map (kbd "C-s") 'my-term)
(define-key ctl-x-map "i" 'my-term)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gauche-mode
;; http://d.hatena.ne.jp/kobapan/20090305/1236261804
;; http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el

;; NOTE: This gauche-mode returns 404.
;; There is another gosh-mode, so for now I submitted a recipe for that into
;; github.com/10sr/emacs-lisp/p.  I'll add setup for that later.

(when nil (and '(fetch-library
            "http://www.katch.ne.jp/~leque/software/repos/gauche-mode/gauche-mode.el"
            t)
           (autoload-eval-lazily 'gauche-mode '(gauche-mode run-scheme)
             (defvar gauche-mode-map (make-sparse-keymap))
             (defvar scheme-mode-map (make-sparse-keymap))
             (define-key gauche-mode-map
               (kbd "C-c C-z") 'run-gauche-other-window)
             (define-key scheme-mode-map
               (kbd "C-c C-c") 'scheme-send-buffer)
             (define-key scheme-mode-map
               (kbd "C-c C-b") 'my-scheme-display-scheme-buffer)))
  (let ((s (executable-find "gosh")))
    (set-variable 'scheme-program-name s)
    (set-variable 'gauche-program-name s))

  (defvar gauche-program-name nil)
  (defvar scheme-buffer nil)

  (defun run-gauche-other-window ()
    "Run gauche on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-gauche))

  (defun run-gauche ()
    "run gauche"
    (interactive)
    (run-scheme gauche-program-name)
    )

  (defun scheme-send-buffer ()
    ""
    (interactive)
    (scheme-send-region (point-min) (point-max))
    (my-scheme-display-scheme-buffer)
    )

  (defun my-scheme-display-scheme-buffer ()
    ""
    (interactive)
    (set-window-text-height (display-buffer scheme-buffer
                                            t)
                            7))

  (add-hook 'scheme-mode-hook
            (lambda ()
              nil))

  (add-hook 'inferior-scheme-mode-hook
            (lambda ()
              ;; (my-scheme-display-scheme-buffer)
              ))
  (setq auto-mode-alist
        (cons '("\.gosh\\'" . gauche-mode) auto-mode-alist))
  (setq auto-mode-alist
        (cons '("\.gaucherc\\'" . gauche-mode) auto-mode-alist))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(when (autoload-eval-lazily 'python '(python-mode)
        (defvar python-mode-map (make-sparse-keymap))
        (define-key python-mode-map (kbd "C-c C-e") 'my-python-run-as-command)
        (define-key python-mode-map (kbd "C-c C-b") 'my-python-display-python-buffer)
        (define-key python-mode-map (kbd "C-m") 'newline-and-indent)

        (defvar inferior-python-mode-map (make-sparse-keymap))
        (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
        (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)
        )
  (set-variable 'python-python-command (or (executable-find "python3")
                                           (executable-find "python")))
  ;; (defun my-python-run-as-command ()
  ;;   ""
  ;;   (interactive)
  ;;   (shell-command (concat python-python-command " " buffer-file-name)))
  (defun my-python-display-python-buffer ()
    ""
    (interactive)
    (defvar python-buffer nil)
    (set-window-text-height (display-buffer python-buffer
                                            t)
                            7))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (my-python-display-python-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; ;; current directory
;; (let ((ls (member 'mode-line-buffer-identification
;;                   mode-line-format)))
;;   (setcdr ls
;;           (cons '(:eval (concat " ("
;;                                 (abbreviate-file-name default-directory)
;;                                 ")"))
;;                 (cdr ls))))

;; ;; display last modified time
;; (let ((ls (member 'mode-line-buffer-identification
;;                   mode-line-format)))
;;   (setcdr ls
;;           (cons '(:eval (concat " "
;;                                 my-buffer-file-last-modified-time))
;;                 (cdr ls))))

;; display date

(when (safe-require-or-eval 'time)
  (setq display-time-interval 29)
  (setq display-time-day-and-date t)
  ;; (if window-system
  ;;     (display-time-mode 0)
  ;;   (display-time-mode 1))
  (when display-time-mode
    (display-time-update)))

(defun buffer-list-not-start-with-space ()
  "Return a list of buffers that not start with whitespaces."
  (let ((bl (buffer-list))
        b nbl)
    (while bl
      (setq b (pop bl))
      (unless (string-equal " "
                            (substring (buffer-name b)
                                       0
                                       1))
        (add-to-list 'nbl b)))
    nbl))

;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;; (add-to-list 'minor-mode-alist
;;              '(global-whitespace-mode ""))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if given function is a built-in one
(defun my-real-function-subr-p (function)
  "Return t if FUNCTION is a built-in function even if it is advised."
  (let* ((advised (and (symbolp function)
                       (featurep 'advice)
                       (ad-get-advice-info function)))
         (real-function
          (or (and advised (let ((origname (cdr (assq 'origname advised))))
                             (and (fboundp origname)
                                  origname)))
              function))
         (def (if (symbolp real-function)
                  (symbol-function real-function)
                function)))
    (subrp def)))

;; (my-real-function-subr-p 'my-real-function-subr-p)
;; (defadvice read-from-minibuffer (before info-in-prompt activate)
;;   "Show system info when use `read-from-minibuffer'."
;;   (ad-set-arg 0
;;               (concat my-system-info
;;                       (ad-get-arg 0))))

;; (defadvice read-string (before info-in-prompt activate)
;;   "Show system info when use `read-string'."
;;   (ad-set-arg 0
;;               (concat my-system-info
;;                       (ad-get-arg 0))))

;; (when (< emacs-major-version 24)
;;   (defadvice completing-read (before info-in-prompt activate)
;;     "Show system info when use `completing-read'."
;;     (ad-set-arg 0
;;                 (concat my-system-info
;;                         (ad-get-arg 0)))))

(when (autoload-eval-lazily 'google-translate '(google-translate-translate
                                                google-translate-at-point))
  (set-variable 'google-translate-default-source-language "auto")
  (set-variable 'google-translate-default-target-language "ja"))
(when (and (safe-require-or-eval 'google-translate)
           (safe-require-or-eval 'google-translate-smooth-ui))
  (add-to-list 'google-translate-translation-directions-alist
               '("en" . "ja"))
  (defun translate-echo-at-point ()
    "Translate popup at point."
    (interactive)
    (let ((google-translate-output-destination 'echo-area))
      (google-translate-translate "auto" "ja" (current-word t t))))
  (define-minor-mode auto-translate-mode
    "Translate word at point automatically."
    :global nil
    :lighter "ATranslate"))



;;;;;;;;;;;;;;;;;;;;;;;;
;; ilookup

(with-eval-after-load 'ilookup
  (set-variable 'ilookup-dict-alist
                '(
                  ("sdcv" . (lambda (word)
                              (shell-command-to-string
                               (format "sdcv -n '%s'"
                                       word))))
                  ("en" . (lambda (word)
                            (shell-command-to-string
                             (format "sdcv -n -u dictd_www.dict.org_gcide '%s'"
                                     word))))
                  ("ja" . (lambda (word)
                            (shell-command-to-string
                             (format "sdcv -n -u EJ-GENE95 -u jmdict-en-ja '%s'"
                                     word))))
                  ("jaj" . (lambda (word)
                             (shell-command-to-string
                              (format "sdcv -n -u jmdict-en-ja '%s'"
                                      word))))
                  ("jag" .
                   (lambda (word)
                     (with-temp-buffer
                       (insert (shell-command-to-string
                                (format "sdcv -n -u 'Genius English-Japanese' '%s'"
                                        word)))
                       (html2text)
                       (buffer-substring (point-min)
                                         (point-max)))))
                  ("alc" . (lambda (word)
                             (shell-command-to-string
                              (format "alc '%s' | head -n 20"
                                      word))))
                  ("app" . (lambda (word)
                             (shell-command-to-string
                              (format "dict_app '%s'"
                                      word))))
                  ;; letters broken
                  ("ms" .
                   (lambda (word)
                     (let ((url (concat
                                 "http://api.microsofttranslator.com/V2/Ajax.svc/"
                                 "Translate?appId=%s&text=%s&to=%s"))
                           (apikey "3C9778666C5BA4B406FFCBEE64EF478963039C51")
                           (target "ja")
                           (eword (url-hexify-string word)))
                       (with-current-buffer (url-retrieve-synchronously
                                             (format url
                                                     apikey
                                                     eword
                                                     target))
                         (message "")
                         (goto-char (point-min))
                         (search-forward-regexp "^$"
                                                nil
                                                t)
                         (url-unhex-string (buffer-substring-no-properties
                                            (point)
                                            (point-max)))))))
                  ))
  ;; (funcall (cdr (assoc "ms"
  ;;                      ilookup-alist))
  ;;          "dictionary")

  ;; (switch-to-buffer (url-retrieve-synchronously "http://api.microsofttranslator.com/V2/Ajax.svc/Translate?appId=3C9778666C5BA4B406FFCBEE64EF478963039C51&text=dictionary&to=ja"))

  ;; (switch-to-buffer (url-retrieve-synchronously "http://google.com"))

  (set-variable 'ilookup-default "ja")
  (when (locate-library "google-translate")
    (defvar ilookup-dict-alist nil)
    (add-to-list 'ilookup-dict-alist
                 '("gt" .
                   (lambda (word)
                     (save-excursion
                       (google-translate-translate "auto"
                                                   "ja"
                                                   word))
                     (with-current-buffer "*Google Translate*"
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))))))
  )


;;;;;;;;;;;;;;;;;;;;;;;
;; adoc-simple-mode

(when (safe-require-or-eval 'adoc-mode)
  (defvar adoc-simple-font-lock-keywords
    nil)
  (define-derived-mode adoc-simple-mode adoc-mode
    "Adoc-Simple"
    "Major mode for editing AsciiDoc text files.
This mode is a simplified version of `adoc-mode'."
    '(set (make-local-variable 'font-lock-defaults)
          '(adoc-simple-font-lock-keywords
            nil nil nil nil
            (font-lock-multiline . t)
            (font-lock-mark-block-function . adoc-font-lock-mark-block-function))))
  (add-to-list 'auto-mode-alist
               '("\\.adoc\\'" . adoc-simple-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; j2-mode jinja2-mmm-mode?

(define-derived-mode jinja2-mmm-mode prog-mode
  "Jinja2 MMM"
  "Major mode to setup `mmm-mode' with mmm-jinja2.
This assumes that file name should be in a format like BASE.EXT.j2 ."
  (require 'mmm-mode)
  (require 'mmm-jinja2)
  ;; Sometimes buffer-file-name is set to nil... Why?
  (when buffer-file-name
    (let ((withoutj2 (replace-regexp-in-string "\\.j2\\'"
                                               ""
                                               buffer-file-name)))
      (let ((mode (assoc-default withoutj2
                                 auto-mode-alist
                                 'string-match)))
        (when mode
          (funcall mode)))
      (add-to-list 'mmm-classes
                   'jinja2)
      (mmm-mode-on))))
;; (add-to-list 'auto-mode-alist
;;              '("\\.j2\\'" . jinja2-mmm-mode))



;;;;;;;;;;;;;;;;;;;;;
;; git-bug

(defconst git-bug-ls-regexp
  (eval-when-compile
    (rx bol
        (submatch (one-or-more alphanumeric))  ; id
        ;; (one-or-more any)
        (one-or-more space)
        (submatch (or "open" "close"))  ; status
        (one-or-more space)
        (submatch (maximal-match (zero-or-more print)))  ; title
        "\t"
        (submatch (one-or-more alphanumeric))  ; user
        (one-or-more space)
        "C:"
        (submatch (one-or-more digit))  ; Comment num
        (one-or-more space)
        "L:"
        (submatch (one-or-more digit))  ; Label num
        eol
        ))
  "Regexp to parse line of output of git-bug ls.
Used by `git-bug-ls'.")

(declare-function string-trim "subr-x")
(defun git-bug-bugs ()
  "Get list of git-bug bugs."
  (with-temp-buffer
    (git-bug--call-process "bug" "ls")
    (goto-char (point-min))
    (let ((bugs nil))
      (while (not (eq (point) (point-max)))
        (save-match-data
          (when (re-search-forward git-bug-ls-regexp (point-at-eol) t)
            (setq bugs `(,@bugs
                         ,(list
                           :id (match-string 1)
                           :status (match-string 2)
                           :title (string-trim (match-string 3))
                           :user (match-string 4)
                           :comment-num (match-string 5)
                           :label-num (match-string 6)
                           )))))
        (forward-line 1)
        (goto-char (point-at-bol)))
      bugs)))

(defun git-bug-ls ()
  "Open and select git bug list buffer."
  (interactive)
  (pop-to-buffer (git-bug-ls-noselect)))

(defun git-bug-ls--set-tabulated-list-mode-variables ()
  "Not implemented.")

(defun git-bug-ls-mode ()
  "Not implemented.")

(defun git-bug-ls-noselect (&optional directory)
  "Open git bug list buffer.

If optional arg DIRECTORY is given change current directory to there before
initializing."
  (setq directory (expand-file-name (or directory
                                        default-directory)))
  (cl-assert (file-directory-p directory))
  (let* ((root (git-bug--get-repository-root directory))
         (name (file-name-nondirectory root))
         (bname (format "*GitBug<%s>*" name)))
    (with-current-buffer (get-buffer-create bname)
      (cd root)
      (git-bug-ls--set-tabulated-list-mode-variables)
      (git-bug-ls-mode)
      (current-buffer))))

(defun git-bug--get-repository-root (dir)
  "Resolve repository root of DIR.

If DIR is not inside of any git repository, signal an error."
  (cl-assert (file-directory-p dir))
  (with-temp-buffer
    (cd dir)
    (git-bug--call-process "rev-parse" "--show-toplevel")
    (goto-char (point-min))
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun git-bug--call-process (&rest args)
  "Start git process synchronously with ARGS.

Raise error when git process ends with non-zero status.
Any output will be written to current buffer."
  (let ((status (apply 'call-process
                       "git"
                       nil
                       t
                       nil
                       args)))
    (cl-assert (eq status 0)
               nil
               (buffer-substring-no-properties (point-min) (point-max)))))


;;;;;;;;;;;;;;
;; mmv
;; https://www.emacswiki.org/emacs/MakingMarkVisible

;;;; Make the mark visible, and the visibility toggleable. ('mmv' means 'make
;;;; mark visible'.) By Patrick Gundlach, Teemu Leisti, and Stefan.

(defgroup mmv nil
  "Make mark visible."
  :group 'tools)

(defvar mmv-face-foreground
  (face-foreground 'hi-yellow)
  "Foreground color for `mmv-face'.")

(defvar mmv-face-background
  (face-background 'hi-yellow)
  "Background color for `mmv-face'.")

(defface mmv-face
  `((t :background ,mmv-face-background :foreground ,mmv-face-foreground))
  "Face used for showing the mark's position."
  :group 'mmv)

(defvar-local mmv-mark-overlay nil
  "The overlay for showing the mark's position.")

(defvar-local mmv-is-mark-visible t
  "The overlay is visible only when this variable's value is t.")

(defun mmv-draw-mark (&rest _)
  "Make the mark's position stand out by means of a one-character-long overlay.
   If the value of variable `mmv-is-mark-visible' is nil, the mark will be
   invisible."
  (unless mmv-mark-overlay
    (setq mmv-mark-overlay (make-overlay 0 0 nil t))
    (overlay-put mmv-mark-overlay 'face 'mmv-face)
    (overlay-put mmv-mark-overlay 'priority 10))  ;; bigger than highlight-indentation-current-column-overlay-priority
  (let ((mark-position (mark t)))
    (cond
     ((null mark-position) (delete-overlay mmv-mark-overlay))
     ((and (< mark-position (point-max))
           (not (eq ?\n (char-after mark-position))))
      (overlay-put mmv-mark-overlay 'after-string nil)
      (move-overlay mmv-mark-overlay mark-position (1+ mark-position)))
     (t
      ;; This branch is called when the mark is at the end of a line or at the
      ;; end of the buffer. We use a bit of trickery to avoid the higlight
      ;; extending from the mark all the way to the right end of the frame.
      (overlay-put mmv-mark-overlay 'after-string
                   (propertize " " 'face (overlay-get mmv-mark-overlay 'face)))
      (move-overlay mmv-mark-overlay mark-position mark-position)))))

;; ;; Makes display very slow?
;; (add-hook 'pre-redisplay-functions #'mmv-draw-mark)

(defun mmv-toggle-mark-visibility ()
  "Toggles the mark's visiblity and redraws it (whether invisible or visible)."
  (interactive)
  (setq mmv-is-mark-visible (not mmv-is-mark-visible))
  (if mmv-is-mark-visible
      (set-face-attribute 'mmv-face nil :background mmv-face-background :foreground mmv-face-foreground)
    (set-face-attribute 'mmv-face nil :background 'unspecified :foreground 'unspecified))
  (mmv-draw-mark))




(provide '10sr-extras)
;;; 10sr-extras.el ends here
