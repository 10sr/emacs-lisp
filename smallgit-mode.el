
(defvar smallgit-mode-hook nil "hook run with smallgit-mode.")
(defvar smallgit-branch-name nil "current branch name")
(make-variable-buffer-local 'smallgit-branch-name)
(defvar smallgit-mode-line-format nil)
(make-variable-buffer-local 'smallgit-mode-line-format)
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
 '(("\C-xvv" . smallgit-commit)
   ("\C-xvi" . smallgit-add-current-file)
   ("\C-xvu" . smallgit-commit-update)
   ("\C-xvg" . smallgit-git)
   ("\C-xvb" . smallgit-checkout)
   ("\C-xv=" . smallgit-diff)
   ("\C-xvu" . smallgit-reset-hard))
 (smallgit--display-mode-line)
 (smallgit--get-branch-name)
 (setq smallgit-mode-line-format (list "SGit:" 'smallgit-branch-name)))

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
  (when (smallgit-repo-p)
    (setq smallgit-branch-name (with-temp-buffer
                                 (shell-command "git branch" t)
                                 (goto-char (point-min))
                                 (when (search-forward "*" nil t)
                                   (forward-char 1)
                                   (buffer-substring-no-properties (point)
                                                                   (point-at-eol)))))))

(defun smallgit-when-switch-branch ()
  (smallgit--get-branch-name)
  (revert-buffer)
  (run-hook 'smallgit-switch-branch-hook))

(add-hook 'window-configuration-change-hook
          'smallgit--get-branch-name)

(defun smallgit--display-mode-line ()
  ""
  ;; (let ((ls (member '(vc-mode vc-mode)
  ;;                   mode-line-format)))
  ;;   (and ls (setcar ls 'smallgit-mode-line))))
  (if (and smallgit-mode (not (member 'smallgit-mode-line-format mode-line-format)))
      (let ((ls (member 'mode-line-position
                        mode-line-format)))
        (setcdr ls (cons 'smallgit-mode-line-format (cdr ls))))))

(defun smallgit--commit (message)
  "call from `smallgit-commit', etc."
  ;; (shell-command (concat "git commit "
  ;;                        (if smallgit--commit-amend "--amend " "")
  ;;                        "-m \""
  ;;                        message
  ;;                        "\"")
  ;;                smallgit-log-buffer)
  (smallgit-git "commit -m" (shell-quote-argument message))
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

(defun smallgit-git (&rest args)
  "execute git with ARGS. ignore `nil' arg."
  (interactive "sgit command options: ")
  (shell-command (concat "git "
                         (mapconcat 'identity (delq nil args) " "))
                 (get-buffer-create smallgit-log-buffer)))

(defun smallgit-init ()
  ""
  (interactive)
  (unless (smallgit-repo-p)
    (smallgit-git "init") ;; (shell-command "git init" smallgit-log-buffer)
    (smallgit-load)))

(defun smallgit-add (&optional file switches)
  ""
  (interactive)
  (smallgit-init)
  ;; (shell-command (concat "git add "
  ;;                        (or switches "")
  ;;                        " "
  ;;                        (or file
  ;;                            ""))
  ;;                smallgit-log-buffer)
  (smallgit-git "add" switches file)
  (message "smallgit: added"))


(defun smallgit-add-current-file ()
  (interactive)
  (smallgit-add buffer-file-name nil))

(defun smallgit-add-all ()
  ""
  (interactive)
  ;; (smallgit-init)
  (smallgit-add nil "-A")
  ;; (shell-command "git add -A" smallgit-log-buffer)
  (message "smallgit: added all in dir"))

(defun smallgit-add-update ()
  ""
  (interactive)
  ;; (smallgit-init)
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
  (smallgit-commit))

(defun smallgit-commit-update ()
  ""
  (interactive)
  (smallgit-add-update)
  (smallgit-commit))

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

(defun smallgit-pull ()
  ""
  (smallgit-git "pull"))

(defun smallgit-remote-add (url name)
  ""
  (interactive "sUrl to add: \nShortname: ")
  (smallgit-git "remote" "add" name url));; (shell-command (concat "git remote add " name " " url) smallgit-log-buffer))

(defun smallgit-tag (name comment)
  ""
  (interactive "sTag name: \nsComment for tag: ")
  (smallgit-git "tag" "-a" name "-m" (shell-quote-argument comment))) ;; (shell-command (concat "git tag -a " name " -m \"" comment "\"")))

(defun smallgit-clone (url)
  ""
  (interactive "sUrl to clone: ")
  (smallgit-git "clone" url)) ;; (shell-command (concat "git clone " url)))

(defun smallgit-checkout-new-branch (name)
  ""
  (interactive "sNew branch name: ")
  (smallgit-checkout name "-b"))

(defun smallgit-checkout (name &optional switches)
  ""
  (interactive "sBranch name:")
  (smallgit-git "checkout" switches name) ;; (shell-command (concat "git checkout " (or switches "") " " name))
  (smallgit--get-branch-name))

(defun smallgit-merge (name &optional switches)
  "merge branch NAME to CURRENT branch.
that is, first checkout the branch to leave, then merge."
  (interactive"sBranch name to merge: ")
  (smallgit-git "merge" switches name) ;; (shell-command (concat "git merge " name))
  (smallgit--get-branch-name))



(defun smallgit-branch (name &optional switches)
  "create new branch or do another command with switches"
  (interactive "sBranch name: ")
  (smallgit-git "branch" switches name) ;; (shell-command (concat "git branch " (or switches "") " " name))
  (smallgit--get-branch-name))

(defun smallgit-delete-branch (name)
  ""
  (interactive "sBranch name to delete: ")
  (smallgit-branch name "-d")
  (smallgit--get-branch-name))

(defun smallgit-reset-hard ()
  "resert all tracked files to last commit state."
  (interactive)
  (smallgit-git "reset" "--hard" "HEAD"))

(provide 'smallgit-mode)


