
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

(easy-mmode-define-minor-mode
 smallgit-mode
 "small minor mode to handle git"
 nil
 nil ;; " SGit"
 nil
 (use-local-map smallgit-mode-map)
 (smallgit--display-mode-line)
 (smallgit-when-change-branch)
 (setq smallgit-mode-line-format (list "SGit:" 'smallgit-branch-name)))

(defvar smallgit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-x v u") 'smallgit-commit-update)
    (define-key map (kbd "C-x v i") 'smallgit-add-current-file)
    (define-key map (kbd "C-x v v") 'smallgit-commit-update)
    (define-key map (kbd "C-x v g") 'smallgit-git)
    (define-key map (kbd "C-x v b") 'smallgit-checkout)
    (define-key map (kbd "C-x v =") 'smallgit-diff)
    (define-key map (kbd "C-x v u") 'smallgit-reset-hard)
    (define-key map (kbd "C-x v n") 'smallgit-checkout-new-branch)
    (define-key map (kbd "C-x v m") 'smallgit-merge)
    (define-key map (kbd "C-x v l") 'smallgit-short-log)
    map))

;; non-interactive functions?

(defvar smallgit--wc nil "for `smallgit-commit', store window configuration")
(defvar smallgit--last-commit-massage nil "save last commit message. used in `smallgit-commit-amend'")
(defvar smallgit--commit-amend nil "t when current commit is amend")

(defun smallgit--get-branch-name ()
  "get current branches and set to `smallgit-branch-name' and `smallgit-branch-list'.
do nothing if current buffer in not under git repository."
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
  "called when create, checkout, or delete branch.
it may be called even if branch does not changed."
  (smallgit--get-branch-name))

(defun smallgit-revert-changed-buffer ()
  ""
  (interactive)
  (mapcar
   (lambda (bf)
     (save-excursion
       (set-buffer bf)
       (when smallgit-mode
         (revert-buffer t t))
       (when smallgit-mode
         (smallgit-when-change-branch))))
   (buffer-list)))

(defun smallgit--display-mode-line ()
  ""
  (if (and smallgit-mode (not (member 'smallgit-mode-line-format mode-line-format)))
      (let ((ls (member 'mode-line-position
                        mode-line-format)))
        (setcdr ls (cons 'smallgit-mode-line-format (cdr ls))))))

(defun smallgit--commit (message)
  "call from `smallgit-commit', etc.
save buffer before commit."
  (smallgit-git "commit" "-m" (shell-quote-argument message))
  (setq smallgit--last-commit-massage message)
  (setq smallgit--commit-amend nil))

(defun smallgit-repo-p ()
  "t if git installed and current dir is git repository, otherwise nil."
  (condition-case nil
      (eq 0 (call-process "git" nil nil nil "status"))
    (error nil))) 

(defun smallgit-complete-branch-name (prompt &optional require-match)
  "read from minibuffer name of branch and return as string.
about arg REQUIRE-MATCH refer to `completing-read'"
  (completing-read prompt
                   smallgit-branch-list
                   nil
                   require-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive

(defun smallgit-load ()
  ""
  (interactive)
  (when (and buffer-file-name (smallgit-repo-p))
    (smallgit-mode 1)))

(defun smallgit-git (&rest args)
  "execute git with ARGS. ignore `nil' args.
it uses `shell-command', so args including whitespace must be `shell-quote-argument'ed."
  (interactive "sgit command options: ")
  (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
  (let (op                              ;gitの出力
        p)                              ;gitの終了ステータス
    (setq op (with-temp-buffer
               (setq p (shell-command (concat "git "
                                              (mapconcat 'identity
                                                         (delq nil args)
                                                         " "))
                                      t))
               (buffer-substring-no-properties (point-min)
                                               (point-max))))
    (save-excursion
      (set-buffer (get-buffer-create smallgit-log-buffer))
      (goto-char (point-max))
      (insert op))
    (message op)
    (eq 0 p)))

(defun smallgit-init ()
  ""
  (interactive)
  (unless (smallgit-repo-p)
    (smallgit-git "init")
    (smallgit-load)))

(defun smallgit-add (&optional file switches)
  ""
  (interactive)
  (smallgit-init)
  (when buffer-file-name (save-buffer))
  (smallgit-git "add" switches file))

(defun smallgit-add-current-file ()
  (interactive)
  (smallgit-add buffer-file-name nil))

(defun smallgit-add-all ()
  ""
  (interactive)
  (smallgit-add nil "-A")
  (message "smallgit: added all in dir"))

(defun smallgit-add-update ()
  ""
  (interactive)
  (smallgit-add nil "-u")
  (message "smallgit: added all updated files"))

(defun smallgit-commit ()
  ""
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
  (smallgit-git "log"))

(defun smallgit-status ()
  ""
  (interactive)
  (smallgit-git "status"))

(defun smallgit-log (&optional num &rest args)
  ""
  (interactive)
  (apply 'smallgit-git
         "log"
         (and num (format "-%d" num))
         args))

(defun smallgit-short-log (&optional num format)
  ""
  (interactive "p")
  (smallgit-log (or num
                    5)
                (shell-quote-argument (concat "--pretty=format:"
                                                         (or format
                                                             "%h - %an, %ad : %s")))
                "--graph"))

(defun smallgit-diff (&optional switches)
  ""
  (interactive)
  (smallgit-git "diff" switches))

(defun smallgit-push ()
  ""
  (interactive)
  (smallgit-git "push"))

(defun smallgit-pull ()
  ""
  (smallgit-git "pull"))

(defun smallgit-remote-add (url name)
  ""
  (interactive "sUrl to add: \nsShortname: ")
  (smallgit-git (shell-quote-argument "remote") "add" name url))

(defun smallgit-remote-add-as-origin (url)
  ""
  (interactive "sUrl to add: ")
  (smallgit-remote-add url "origin"))

(defun smallgit-tag (&optional name comment)
  ""
  (interactive "sTag name: \nsComment for tag: ")
  (smallgit-git "tag" "-a" name "-m" (shell-quote-argument comment)))

(defun smallgit-clone (url)
  ""
  (interactive "sUrl to clone: ")
  (smallgit-git "clone" url))

(defun smallgit-checkout-new-branch (name)
  ""
  (interactive "sNew branch name: ")
  (smallgit-checkout name "-b"))

(defun smallgit-checkout (name &optional switches)
  "checkout branch"
  (interactive (list (smallgit-complete-branch-name "Branch name to checkout: " t)))
  (smallgit-git "checkout" switches name)
  (smallgit-revert-changed-buffer))

(defun smallgit-merge (name &optional switches)
  "merge branch NAME to CURRENT branch.
that is, first checkout the branch to leave, then merge."
  (interactive (list (smallgit-complete-branch-name "Branch name to merge: " t)))
  (smallgit-git "merge" switches name)
  (smallgit-revert-changed-buffer))

(defun smallgit-branch (name &optional switches)
  "create new branch or do another command with switches"
  (interactive (list (smallgit-complete-branch-name "Branch name to create: ")))
  (smallgit-git "branch" switches name)
  (smallgit-revert-changed-buffer))

(defun smallgit-delete-branch (name)
  ""
  (interactive (list (smallgit-complete-branch-name "Branch name to delete: " t)))
  (smallgit-branch name "-D"))

(defun smallgit-reset-hard ()
  "resert all tracked files to last commit state."
  (interactive)
  (smallgit-git "reset" "--hard" "HEAD")
  (smallgit-revert-changed-buffer))

(defun smallgit-rebase (name)
  ""
  (interactive (list (smallgit-complete-branch-name "Branch name to rebase: " t)))
  (smallgit-git "rebase" name)
  (smallgit-revert-changed-buffer))

(defun smallgit-merge-current-branch-to-master () ;cannot revert when returned to bch 時間が速すぎてrevertできないんだよね
  "commit needed before merge."
  (interactive)
  (let ((bch smallgit-branch-name))
    (and (smallgit-checkout "master") ;これだと終了判定できない
         (smallgit-merge bch)
         (smallgit-checkout bch))))

(provide 'smallgit-mode)


