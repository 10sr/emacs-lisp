
(defvar smallgit-mode-hook nil)
(defvar smallgit-branch-name nil)
(make-variable-buffer-local 'smallgit-branch-name)
(defvar smallgit-mode-line nil)
(make-variable-buffer-local 'smallgit-mode-line)
(defvar smallgit-log-buffer "*smallgit-log*")

(require 'log-edit)
(require 'easy-mmode)
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; (easy-mmode-define-keymap
;;  (list ((kbd "C-x v v") . 'smallgit-add)
;;        ((kbd "C-x v i") . 'smallgit-add))
;;  'smallgit-mode-map)

(easy-mmode-define-minor-mode
 smallgit-mode
 "small minor mode to handle git"
 nil
 nil ;; " SGit"
 '(("\C-xvv" . smallgit-add-current-file)
   ("\C-xvi" . smallgit-init)
   ("\C-xvu" . smallgit-commit-update))
 (smallgit--display-mode-line)
 (smallgit--get-branch-name)
 (setq smallgit-mode-line (list "SGit:" 'smallgit-branch-name)))

;; (defvar smallgit-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map (kbd "C-x v v") 'smallgit-add)
;;     (define-key map (kbd "C-x v i") 'smallgit-add)))

;; 内部用

(defvar smallgit--wc nil "for `smallgit-commit', store window configuration")
(defvar smallgit--last-commit-massage nil "save last commit message. used in `smallgit-commit-amend'")
(defvar smallgit--commit-amend nil "t when current commit is amend")

(defun smallgit--get-branch-name ()
  "git current branch name and set to `smallgit-branch-name'"
  (interactive)
  (when (smallgit-repo-p)
    (setq smallgit-branch-name (with-temp-buffer
                                 (shell-command "git branch" t)
                                 (goto-char (point-min))
                                 (search-forward "*")
                                 (forward-char 1)
                                 (buffer-substring-no-properties (point)
                                                                 (point-at-eol))))))

(add-hook 'window-configuration-change-hook
          'smallgit--get-branch-name)

(defun smallgit--display-mode-line ()
  ""
  ;; (let ((ls (member '(vc-mode vc-mode)
  ;;                   mode-line-format)))
  ;;   (and ls (setcar ls 'smallgit-mode-line))))
  (if (and smallgit-mode (not (member 'smallgit-mode-line mode-line-format)))
      (let ((ls (member 'mode-line-position
                        mode-line-format)))
        (setcdr ls (cons 'smallgit-mode-line (cdr ls))))))

(defun smallgit--commit (message)
  "call from `smallgit-commit', etc."
  (shell-command (concat "git commit "
                         (if smallgit--commit-amend "--amend " "")
                         "-m \""
                         message
                         "\"")
                 smallgit-log-buffer)
  (smallgit--get-branch-name)
  (setq smallgit--last-commit-massage message)
  (setq smallgit--commit-amend nil))

(defun smallgit-repo-p ()
  "t if current dir is git repository, otherwise nil."
  (eq 0 (call-process "git" nil nil nil "status")))

;; 外部用

(defun smallgit-load ()
  ""
  (interactive)
  (when (smallgit-repo-p)
    (smallgit-mode 1)
    (smallgit--get-branch-name)))

(defun smallgit-git (&rest ARGS)
  "execute git with ARGS. ignore `nil' arg."
  (interactive "sgit command options: ")
  (shell-command (concat "git "
                         (mapconcat 'identity (delq nil ARGS) " "))
                 smallgit-log-buffer))

(defun smallgit-init ()
  ""
  (interactive)
  (unless (smallgit-repo-p)
    (smallgit-git "init") ;; (shell-command "git init" smallgit-log-buffer)
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

(defun smallgit-commit-amend ()
  ""
  (interactive)
  (setq smallgit--commit-amend t)
  (smallgit-commit))

(add-hook 'log-edit-hook
          (lambda ()
            (when smallgit--commit-amend
              (insert smallgit--last-commit-massage))))


(defun smallgit-log (&optional switches)
  ""
  (interactive)
  (smallgit-git "log")) ;; (shell-command "git log" smallgit-log-buffer))


(defun smallgit-status ()
  ""
  (interactive)
  (smallgit-git "status")) ;; (shell-command "git status" smallgit-log-buffer))

(defun smallgit-diff ()
  ""
  (interactive)
  (smallgit-git "diff")) ;; (shell-command "git diff" smallgit-log-buffer))

(defun smallgit-push ()
  ""
  (interactive)
  (smallgit-git "push")) ;; (shell-command "git push" smallgit-log-buffer))

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
  (shell-command (concat "git checkout " (or switches "") " " name))
  (smallgit--get-branch-name))

(defun smallgit-merge (name)
  "merge branch NAME to CURRENT branch.
that is, first checkout the branch to leave then merge."
  (interactive"sBranch name to merge: ")
  (shell-command (concat "git merge " name))
  (smallgit--get-branch-name))

(defun smallgit-branch (name &optional switches)
  "create new branch or do another command with switches"
  (interactive "sBranch name: ")
  (shell-command (concat "git branch " (or switches "") " " name))
  (smallgit--get-branch-name))

(defun smallgit-delete-branch (name)
  ""
  (interactive "sBranch name to delete: ")
  (smallgit-branch name "-d")
  (smallgit--get-branch-name))

(provide 'smallgit-mode)


