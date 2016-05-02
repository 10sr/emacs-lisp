;; todo: delete functions around branch
;; remove branch name from mode-line?

(defvar sgit-mode-hook nil "hook run with sgit-mode.")
(defvar sgit-branch-name nil "current branch name")
(defvar sgit-branch-list nil "branch list")
(defvar sgit-repository-path nil "dir path")
(make-variable-buffer-local 'sgit-branch-name)
(make-variable-buffer-local 'sgit-branch-list)
(make-variable-buffer-local 'sgit-repository-path)
(defvar sgit-mode-line-format nil)
(make-variable-buffer-local 'sgit-mode-line-format)
(defvar sgit-log-buffer "*sgit-log*")

(require 'log-edit)
(require 'easy-mmode)
(setq vc-handled-backends (delq 'Git vc-handled-backends))

(easy-mmode-define-minor-mode sgit-mode
                              "small minor mode to handle git"
                              nil
                              nil
                              nil
                              (use-local-map sgit-mode-map)
                              (sgit--display-mode-line)
                              (sgit-when-change-branch)
                              (setq sgit-repository-path (sgit--find-repository-path))
                              (setq sgit-mode-line-format (list " SGit:" 'sgit-branch-name)))

(defvar sgit-prefix-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "i") 'sgit-add-current-file)
    (define-key map (kbd "v") 'sgit-commit-update)
    (define-key map (kbd "g") 'sgit-command)
    ;; (define-key map (kbd "b") 'sgit-checkout)
    (define-key map (kbd "=") 'sgit-diff)
    (define-key map (kbd "u") 'sgit-reset-hard)
    ;; (define-key map (kbd "n") 'sgit-checkout-new-branch)
    ;; (define-key map (kbd "m") 'sgit-merge)
    (define-key map (kbd "l") 'sgit-short-log)
    map))

(defvar sgit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x v") sgit-prefix-map)
    ;; (define-key map (kbd "C-x v i") 'sgit-add-current-file)
    ;; (define-key map (kbd "C-x v v") 'sgit-commit-update)
    ;; (define-key map (kbd "C-x v g") 'sgit-git)
    ;; (define-key map (kbd "C-x v b") 'sgit-checkout)
    ;; (define-key map (kbd "C-x v =") 'sgit-diff)
    ;; (define-key map (kbd "C-x v u") 'sgit-reset-hard)
    ;; (define-key map (kbd "C-x v n") 'sgit-checkout-new-branch)
    ;; (define-key map (kbd "C-x v m") 'sgit-merge)
    ;; (define-key map (kbd "C-x v l") 'sgit-short-log)
    map))


;; internal variables and functions?

(defvar sgit--wc nil "for `sgit-commit', store window configuration")
(defvar sgit--last-commit-message "" "save last commit message. used in `sgit-commit-amend'")
(defvar sgit--commit-amend nil "t when current commit is amend")
(defvar sgit--commit-initial-message "" "")

(defun sgit--set-branch-name ()
  "get current branches and set to `sgit-branch-name' and `sgit-branch-list'.
do nothing if current buffer in not under git repository."
  ;; (interactive)
  (when (sgit-repo-p)
    (let ((l (sgit--get-branch-name)))
      (setq sgit-branch-name (car l))
      (setq sgit-branch-list l))))

(defun sgit--get-branch-name ()
  "return list of branch names, with current branch in the car."
  (with-temp-buffer
    (shell-command "git branch" t)
    (goto-char (point-min))
    (when (search-forward "*" nil t)
      (forward-char 1)
      (cons (buffer-substring-no-properties (point)
                                            (point-at-eol))
            (progn (kill-whole-line)
                   (goto-char (point-min))
                   (while (re-search-forward "^  " nil t)
                     (replace-match ""))
                   (delete ""
                           (split-string (buffer-substring-no-properties (point-min)
                                                                         (point-max))
                                         "\n")))))))

(defun sgit-when-change-branch ()
  "called when create, checkout, or delete branch.
it may be called even if branch does not changed."
  (sgit--set-branch-name))

(defun sgit--find-repository-path (&optional dir)
  ""
  (let ((path (file-name-as-directory (or dir
                                          "."))))
    (if (file-directory-p (concat path
                                  ".git"))
        (expand-file-name (concat path
                                  ".git"))
      (sgit--find-repository-path (concat path
                                          "..")))))

(defun sgit-revert-changed-buffer ()
  ""
  (interactive)
  (let ((rpath sgit-repository-path)
        (bl (buffer-list)))
    (while bl
      (with-current-buffer (pop bl)
        (when (and sgit-mode
                   (equal rpath sgit-repository-path))
          (revert-buffer t t)
          (sgit-when-change-branch))))
    t))

(defun sgit--display-mode-line ()
  ""
  (if (and sgit-mode
           (not (member 'sgit-mode-line-format mode-line-format)))
      (let ((ls (member 'mode-line-position
                        mode-line-format)))
        (setcdr ls (cons 'sgit-mode-line-format (cdr ls))))))

(defun sgit--commit (message)
  "call from `sgit-commit', etc."
  (sgit-git "commit" "-m" message)
  (setq sgit--last-commit-message message)
  (setq sgit--commit-amend nil))

(defun sgit-repo-p ()
  "t if git installed and current dir is git repository, otherwise nil."
  (and (executable-find "git")
       (eq 0 (call-process "git" nil nil nil "status"))))
;; (condition-case nil
;;     (eq 0 (call-process "git" nil nil nil "status"))
;;   (error nil))) 

(defun sgit-complete-branch-name (prompt &optional require-match)
  "read from minibuffer name of branch and return as string.
about arg REQUIRE-MATCH refer to `completing-read'"
  (completing-read prompt
                   sgit-branch-list
                   nil
                   require-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for interactive use

(defun sgit-load ()
  ""
  (interactive)
  (when (and buffer-file-name (sgit-repo-p))
    (sgit-mode 1)))

;; (defun sgit-git (&rest args)
;;   "execute git with ARGS. ignore `nil' args.
;; it uses `shell-command', so args including whitespace must be `shell-quote-argument'ed."
;;   (interactive "sgit command options: ")
;;   (when (and sgit-mode buffer-file-name (buffer-modified-p))
;;     (save-buffer))
;;   (let ((ls nil)
;;         (op nil)
;;         (p nil)
;;         (processs-environment (cons "GIT_PAGER="
;;                                     process-environment)))
;;     (setq ls (with-temp-buffer
;;                (cons (shell-command (concat "git "
;;                                             (mapconcat 'identity
;;                                                        (delq nil args)
;;                                                        " "))
;;                                     t)
;;                      (buffer-substring-no-properties (point-min)
;;                                                      (point-max)))))
;;     (setq p (car ls))
;;     (setq op (cdr ls))
;;     (with-current-buffer (get-buffer-create sgit-log-buffer)
;;       (goto-char (point-max))
;;       (insert op))
;;     (message op)
;;     (and (eq 0 p)
;;          op)))

(defun sgit-git (&rest args)
  (sgit-command (mapconcat 'shell-quote-argument
                           (delq nil args)
                           " ")))

(defun sgit-command (option)
  (interactive "sgit: ")
  (when (and sgit-mode buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((process-environment (cons "GIT_PAGER="
                                   process-environment))
        start)
    (with-current-buffer (get-buffer-create sgit-log-buffer)
      (setq start (goto-char (point-max)))
      (shell-command (concat "git "
                             option)
                     t)
      (message (buffer-substring-no-properties start
                                               (point-max))))))

(defun sgit-init ()
  ""
  (interactive)
  (unless (sgit-repo-p)
    (sgit-git "init")
    (sgit-load)))

(defun sgit-add (&optional file switches)
  ""
  (interactive)
  (sgit-init)
  (when buffer-file-name (save-buffer))
  (sgit-git "add" switches file))

(defun sgit-add-current-file ()
  (interactive)
  (sgit-add buffer-file-name nil))

(defun sgit-add-all ()
  ""
  (interactive)
  (sgit-add nil "-A")
  (message "sgit: added all in dir"))

(defun sgit-add-update ()
  ""
  (interactive)
  (sgit-add nil "-u")
  (message "sgit: added all updated files"))

(defun sgit-commit ()
  ""
  (interactive)
  (setq sgit--wc (current-window-configuration))
  (log-edit (lambda ()
              (interactive)
              (sgit--commit (save-excursion
                              (set-buffer "*sgit commit*")
                              (goto-char (point-min))
                              (while (re-search-forward "^#" nil t)
                                (kill-whole-line))
                              (buffer-substring-no-properties (point-max)
                                                              (point-min))))
              (set-window-configuration sgit--wc)
              (kill-buffer "*sgit commit*"))
            t
            nil
            (get-buffer-create "*sgit commit*"))
  (insert sgit--commit-initial-message)
  (setq sgit--commit-initial-message ""))


;; (add-hook 'log-edit-hook
;;           (lambda ()
;;             (when sgit--commit-amend
;;               (insert sgit--last-commit-message))))

(defun sgit-commit-all ()
  ""
  (interactive)
  (sgit-add-all)
  (setq sgit--commit-initial-message (sgit-status))
  (sgit-commit))

(defun sgit-commit-update ()
  ""
  (interactive) 
  (sgit-add-update)
  (setq sgit--commit-initial-message (sgit-status))
  (sgit-commit))

;; (defun sgit-commit-amend ()
;;   ""
;;   (interactive)
;;   (setq sgit--commit-amend t)
;;   (setq sgit--commit-initla-message sgit--last-commit-message)
;;   (sgit-commit))

(defun sgit-log (&optional switches)
  ""
  (interactive)
  (sgit-git "log"))

(defun sgit-status ()
  ""
  (interactive)
  (sgit-git "status"))

(defun sgit-log (&optional num &rest args)
  ""
  (interactive)
  (apply 'sgit-git
         "log"
         (and num (format "-%d" num))
         args))

(defun sgit-short-log (&optional num format)
  ""
  (interactive "p")
  (sgit-log (or num
                5)
            (concat "--pretty=format:"
                    (or format
                        "%h - %an, %ad : %s"))
            "--graph"))

(defun sgit-diff (&optional switches)
  ""
  (interactive)
  (sgit-git "diff" switches))

(defun sgit-push ()
  ""
  (interactive)
  (sgit-git "push"))

(defun sgit-pull ()
  ""
  (sgit-git "pull"))

(defun sgit-remote-add (url name)
  ""
  (interactive "sUrl to add: \nsShortname: ")
  (sgit-git "remote" "add" name url))

(defun sgit-remote-add-as-origin (url)
  ""
  (interactive "sUrl to add: ")
  (sgit-remote-add url "origin"))

(defun sgit-tag (&optional name comment)
  ""
  (interactive "sTag name: \nsComment for tag: ")
  (sgit-git "tag" "-a" name "-m" comment))

(defun sgit-clone (url)
  ""
  (interactive "sUrl to clone: ")
  (sgit-git "clone" url))

;; (defun sgit-checkout-new-branch (name)
;;   ""
;;   (interactive "sNew branch name: ")
;;   (sgit-checkout name "-b"))

;; (defun sgit-checkout (name &optional switches)
;;   "checkout branch"
;;   (interactive (list (sgit-complete-branch-name "Branch name to checkout: " t)))
;;   (and (sgit-git "checkout" switches name)
;;        (sgit-revert-changed-buffer)))

;; (defun sgit-merge (name &optional switches)
;;   "merge branch NAME to CURRENT branch.
;; that is, first checkout the branch to leave, then merge."
;;   (interactive (list (sgit-complete-branch-name "Branch name to merge: " t)))
;;   (and (sgit-git "merge" switches name)
;;        (sgit-revert-changed-buffer)))

;; (defun sgit-branch (name &optional switches)
;;   "create new branch or do another command with switches"
;;   (interactive (list (sgit-complete-branch-name "Branch name to create: ")))
;;   (and (sgit-git "branch" switches name)
;;        (sgit-revert-changed-buffer)))

;; (defun sgit-delete-branch (name)
;;   ""
;;   (interactive (list (sgit-complete-branch-name "Branch name to delete: " t)))
;;   (sgit-branch name "-D"))

(defun sgit-reset-hard ()
  "resert all tracked files to last commit state."
  (interactive)
  (and (sgit-git "reset" "--hard" "HEAD")
       (sgit-revert-changed-buffer)))

;; (defun sgit-rebase (name)
;;   ""
;;   (interactive (list (sgit-complete-branch-name "Branch name to rebase: " t)))
;;   (and (sgit-git "rebase" name)
;;        (sgit-revert-changed-buffer)))

(provide 'sgit-mode)


