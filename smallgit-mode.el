(setq vc-handled-backends (delq 'Git vc-handled-backends))

(require 'easy-mmode)

;; (easy-mmode-define-keymap
;;  (list ((kbd "C-x v v") . 'smallgit-add)
;;        ((kbd "C-x v i") . 'smallgit-add))
;;  'smallgit-mode-map)

(easy-mmode-define-minor-mode
 smallgit-mode
 "small minor mode to handle git"
 nil
 " SGit"
 '(("\C-xvv" . smallgit-add-current-file)
   ("\C-xvi" . smallgit-init)
   ("\C-xvu" . smallgit-commit-update)))

(defvar smallgit-mode-hook nil)

;; (defvar smallgit-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map (kbd "C-x v v") 'smallgit-add)
;;     (define-key map (kbd "C-x v i") 'smallgit-add)))

(defun load-smallgit-mode ()
  ""
  (interactive)
  (when (smallgit-repo-p)
    (smallgit-mode 1)))

(defvar smallgit-log-buffer "*smallgit-log*")

;; (defun smallgit--call-git (&rest ARGS)
;;   "not needed?"
;;   (shell-command (concat "git"
;;                          (apply 'concat ARGS))
;;                  smallgit-log-buffer))

(defun smallgit-repo-p ()
  ""
  (eq 0 (call-process "git" nil nil nil "status")))

(defun smallgit-init ()
  ""
  (interactive)
  (unless (smallgit-repo-p)
    (shell-command "git init" smallgit-log-buffer)
    (load-smallgit-mode)))

(defun smallgit-add (&optional file switches)
  ""
  (interactive)
  (smallgit-init)
  (shell-command (concat "git add "
                         (or switches "")
                         " "
                         (or file
                             ""))
                 smallgit-log-buffer)
  (message "smallgit: added"))


(defun smallgit-add-current-file ()
  (interactive)
  (smallgit-add buffer-file-name nil))

(defun smallgit-add-all ()
  ""
  (interactive)
  (smallgit-init)
  (smallgit-add nil "-A")
  ;; (shell-command "git add -A" smallgit-log-buffer)
  (message "smallgit: added all in dir"))

(defun smallgit-add-update ()
  ""
  (interactive)
  (smallgit-init)
  (smallgit-add nil "-u")
  ;; (shell-command "git add -u" smallgit-log-buffer)
  (message "smallgit: added all updated files"))

(require 'log-edit)

(defvar smallgit--wc nil)

(defun smallgit-commit ()
  ""
  ;; (interactive "sCommit massage: ")
  ;; (smallgit--commit message))
  (interactive)
  (setq smallgit--wc (current-window-configuration))
  (log-edit (lambda ()
              (interactive)
              (smallgit--commit (save-excursion
                                  (set-buffer "*smallgit commit*")
                                  (buffer-substring-no-properties (point-max)
                                                                  (point-min))))
              (set-window-configuration smallgit--wc)
              (kill-buffer "*smallgit commit*"))
            t
            nil
            (get-buffer-create "*smallgit commit*")))


(defun smallgit-commit-all ()
  ""
  (interactive)
  (smallgit-add-all)
  (smallgit-commit nil))

(defun smallgit-commit-update ()
  ""
  (interactive)
  (smallgit-add-update)
  (call-interactively 'smallgit-commit))


(defvar smallgit--last-commit-massage nil)
(defvar smallgit--commit-amend nil)

(defun smallgit-commit-amend ()
  ""
  (interactive)
  (setq smallgit--commit-amend t)
  (smallgit-commit))

(add-hook 'log-edit-hook
          (lambda ()
            (when smallgit--commit-amend
              (insert smallgit--last-commit-massage))))

(defun smallgit--commit (message)
  "call from `smallgit-commit'"
  (shell-command (concat "git commit "
                         (if smallgit--commit-amend "--amend " "")
                         "-m \""
                         message
                         "\"")
                 smallgit-log-buffer)
  (setq smallgit--last-commit-massage message)
  (setq smallgit--commit-amend nil))

(defun smallgit-log (&optional switches)
  ""
  (interactive)
  (shell-command "git log" smallgit-log-buffer))

(defun smallgit-status ()
  ""
  (interactive)
  (shell-command "git status" smallgit-log-buffer))

(defun smallgit-diff ()
  ""
  (interactive)
  (shell-command "git diff" smallgit-log-buffer))

(defun smallgit-push ()
  ""
  (interactive)
  (shell-command "git push" smallgit-log-buffer))

(defun smallgit-remote-add (url name)
  ""
  (interactive "sUrl to add: \nShortname: ")
  (shell-command (concat "git remote add " name " " url) smallgit-log-buffer))

(defun smallgit-tag (name comment)
  ""
  (interactive "sTag name: \nsComment for tag: ")
  (shell-command (concat "git tag -a " name " -m \"" comment "\"")))

(defun smallgit-clone (url)
  ""
  (interactive "sUrl to clone: ")
  (shell-command (concat "git clone " url)))

(defun smallgit-checkout-new-branch (name)
  ""
  (interactive "sNew branch name: ")
  (smallgit-checkout name "-b"))

(defun smallgit-checkout (name &optional switches)
  ""
  (interactive "sBranch name:")
  (shell-command (concat "git checkout " (or switches "") " " name)))

(defun smallgit-merge (name)
  ""
  (interactive"sBranch name to merge: ")
  (shell-command (concat "git merge " name)))

(defun smallgit-branch (name &optional switches)
  ""
  (interactive "sBranch name: ")
  (shell-command (concat "git branch " (or switches "") " " name)))

(defun smallgit-delete-branch (name)
  ""
  (interactive "sBranch name to delete: ")
  (smallgit-branch name "-d"))

(provide 'smallgit-mode)

;; (defun smallgit-update-index-add (&optional file)
;;   ""
;;   (start-process "git" nil nil nil "update-index" "-add" file))

;; (defun smallgit-commit-all ()
;;   ""
;;   (interactive)
;;   (smallgit-add-all)
;;   (log-edit (lambda ()
;;               (call-process "git" nil nil nil "commit" "-m" comment))





