
(defvar smallgit-mode-hook nil "hook run with smallgit-mode.")
(defvar smallgit-branch-name nil "current branch name")
(defvar smallgit-branch-list nil "branch list")
(make-variable-buffer-local 'smallgit-branch-name)
(make-variable-buffer-local 'smallgit-branch-list)
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
 ;; '(("\C-xvv" . smallgit-commit)
 ;;   ("\C-xvi" . smallgit-add-current-file)
 ;;   ("\C-xvu" . smallgit-commit-update)
 ;;   ("\C-xvg" . smallgit-git)
 ;;   ("\C-xvb" . smallgit-checkout)
 ;;   ("\C-xv=" . smallgit-diff)
 ;;   ("\C-xvr" . smallgit-reset-hard)
 ;;   ("\C-xvn" . smallgit-checkout-new-branch))
 nil
 (use-local-map smallgit-mode-map)
 (smallgit--display-mode-line)
 (smallgit-when-change-branch)
 (setq smallgit-mode-line-format (list "SGit:" 'smallgit-branch-name)))

(defvar smallgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x v u") 'smallgit-commit-update)
    (define-key map (kbd "C-x v i") 'smallgit-add-current-file)
    (define-key map (kbd "C-x v v") 'smallgit-commit)
    (define-key map (kbd "C-x v g") 'smallgit-git)
    (define-key map (kbd "C-x v b") 'smallgit-checkout)
    (define-key map (kbd "C-x v =") 'smallgit-diff)
    ;; (define-key map (kbd "C-x v r") 'smallgit-reset-hard)
    (define-key map (kbd "C-x v n") 'smallgit-checkout-new-branch)
    (define-key map (kbd "C-x v m") 'smallgit-merge)
    map))

;; non-interactive functions

(defvar smallgit--wc nil "for `smallgit-commit', store window configuration")
(defvar smallgit--last-commit-massage nil "save last commit message. used in `smallgit-commit-amend'")
(defvar smallgit--commit-amend nil "t when current commit is amend")

(defun smallgit--get-branch-name ()
  "get current branches and set to `smallgit-branch-name' and `smallgit-branch-list'"
  (when (smallgit-repo-p)
    (let (a
          b)
      (with-temp-buffer
        (shell-command "git branch" t)
        (goto-char (point-min))
        (when (search-forward "*" nil t)
          (forward-char 1)
          (setq a (buffer-substring-no-properties (point)
                                                  (point-at-eol)))
          (goto-char (point-min))
          (while (re-search-forward "^. " nil t)
            (replace-match ""))
          (setq b (delete ""
                          (split-string (buffer-substring-no-properties (point-min)
                                                                        (point-max))
                                        "\n")))))
      (setq smallgit-branch-name a)
      (setq smallgit-branch-list b))))

(defun smallgit-when-change-branch ()
  "called when create, checkout, or delete branch"
  (smallgit--get-branch-name))
  ;;(when buffer-file-name (revert-buffer nil t)) ;;これつけると無限ループ
  ;; (run-hooks 'smallgit-chenge-branch-hook))

;; (add-hook 'window-configuration-change-hook
;;           'smallgit-when-change-branch)

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
  (smallgit-git "commit" "-m" (shell-quote-argument message))
  (smallgit--get-branch-name)
  (setq smallgit--last-commit-massage message)
  (setq smallgit--commit-amend nil))

(defun smallgit-repo-p ()
  "t if current dir is git repository, otherwise nil."
  (eq 0 (shell-command "git status")))

(defun smallgit-complete-branch-name (prompt &optional require-match)
  "about arg REQUIRE-MATCH refer to `completing-read'"
  (completing-read prompt
                   smallgit-branch-list
                   nil
                   require-match))

;; interactive

(defun smallgit-load ()
  ""
  (interactive)
  (when (and buffer-file-name (smallgit-repo-p))
    (smallgit-mode 1)
    (smallgit-when-change-branch)))

(defun smallgit-git (&rest args)
  "execute git with ARGS. ignore `nil' arg."
  (interactive "sgit command options: ")
  ;; (shell-command (concat "git "
  ;;                        (mapconcat 'identity (delq nil args) " "))
  ;;                (get-buffer-create smallgit-log-buffer)))
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
  (let ((op (with-temp-buffer
              (shell-command (concat "git "
                                     (mapconcat 'identity
                                                (delq nil args)
                                                " "))
                             t)
              (buffer-substring-no-properties (point-min)
                                              (point-max)))))
    (save-excursion
      (set-buffer (get-buffer-create smallgit-log-buffer))
      (goto-char (point-max))
      (insert op))
    (message op)
    (when buffer-file-name (revert-buffer nil t))))

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
  (when buffer-file-name (save-buffer))
  (smallgit-git "add" switches file))

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

(add-hook 'log-edit-hook
          (lambda ()
            (when smallgit--commit-amend
              (insert smallgit--last-commit-massage))))

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

(defun smallgit-tag (&optional name comment)
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
  (interactive (list (smallgit-complete-branch-name "Branch name to checkout: " t)))
  (smallgit-git "checkout" switches name) ;; (shell-command (concat "git checkout " (or switches "") " " name))
  (smallgit-when-change-branch))

(defun smallgit-merge (name &optional switches)
  "merge branch NAME to CURRENT branch.
that is, first checkout the branch to leave, then merge."
  (interactive (list (smallgit-complete-branch-name "Branch name to merge: " t)))
  (smallgit-git "merge" switches name) ;; (shell-command (concat "git merge " name))
  (smallgit-when-change-branch))

(defun smallgit-branch (name &optional switches)
  "create new branch or do another command with switches"
  (interactive (list (smallgit-complete-branch-name "Branch name to create: ")))
  (smallgit-git "branch" switches name) ;; (shell-command (concat "git branch " (or switches "") " " name))
  (smallgit-when-change-branch))

(defun smallgit-delete-branch (name)
  ""
  (interactive (list (smallgit-complete-branch-name "Branch name to delete: " t)))
  (smallgit-branch name "-D"))

(defun smallgit-reset-hard ()
  "resert all tracked files to last commit state."
  (interactive)
  (smallgit-git "reset" "--hard" "HEAD"))

(defun smallgit-rebase (name)
  ""
  (interactive (list (smallgit-complete-branch-name "Branch name to rebase: " t)))
  (smallgit-git "rebase" name)
  (smallgit-when-change-branch))

(provide 'smallgit-mode)


