(defvar git-command-modes-alist
  '(("diff" . diff-mode)
    ("di" . diff-mode))
  "Alist of modes for each git command.")

(defvar git-command-default-options
  ""
  "Options always passed to git.")

(defvar git-command-max-mini-window-height
  max-mini-window-height
  "Maximum height for resizing mini-window when showing result.
This value means nothing when `resize-mini-window' is nil.")

(defvar git-command-ps1-showdirtystate "t"
  "Set the value of GIT_PS1_SHOWDIRTYSTATE to this when running __git_ps1.")

(defvar git-command-ps1-showstashstate ""
  "Set the value of GIT_PS1_SHOWSTASHSTATE to this when running __git_ps1.")

(defvar git-command-ps1-showuntrackedfiles ""
  "Set the value of GIT_PS1_SHOWUNTRACKEDFILES to this when running __git_ps1.")

(defvar git-command-ps1-showupstream "auto"
  "Set the value of GIT_PS1_SHOWUPSTREAM to this when running __git_ps1.")

(defvar git-command-history nil
  "History list for `git-command'.")

(defun git-command-find-git-ps1 (f)
  "Return F if F exists and it contains function \"__git_ps1\"."
  (and (file-readable-p f)
       (with-temp-buffer
         (insert ". " f "; "
                 "__git_ps1 %s;")
         (eq 0 (shell-command-on-region (point-min)
                                        (point-max)
                                        "bash -s"
                                        nil
                                        t)))
       f))

(defvar git-command-prompt-file
  (or (git-command-find-git-ps1 "/usr/share/git/completion/git-prompt.sh")
      (git-command-find-git-ps1 "/opt/local/share/doc/git-core/contrib/completion/git-prompt.sh")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git")
      (git-command-find-git-ps1 "/opt/local/etc/bash_completion.d/git")))

(defun git-command-ps1 (str)
  (let ((gcmpl (or git-command-prompt-file))
        (process-environment `(,(concat "GIT_PS1_SHOWDIRTYSTATE=" git-command-ps1-showdirtystate)
                               ,(concat "GIT_PS1_SHOWSTASHSTATE=" git-command-ps1-showstashstate)
                               ,(concat "GIT_PS1_SHOWUNTRACKEDFILES=" git-command-ps1-showuntrackedfiles)
                               ,(concat "GIT_PS1_SHOWUPSTREAM=" git-command-ps1-showupstream)
                               ,@process-environment)))
    (if (and gcmpl
             (file-readable-p gcmpl))
        (with-temp-buffer
          (insert ". " gcmpl
                  "; __git_ps1 "
                  (shell-quote-argument str)
                  ";")
          (shell-command-on-region (point-min)
                                   (point-max)
                                   "bash -s"
                                   nil
                                   t)
          (buffer-substring-no-properties (point-min)
                                          (point-max)))
      "")))

(defun git-command (cmd)
  "Shell like git command interface."
  (interactive (list (read-shell-command (format "[%s]%s $ git : "
                                                 (abbreviate-file-name default-directory)
                                                 (git-command-ps1 "[GIT:%s]"))
                                         nil
                                         'git-command-history)))
  (let ((dir default-directory)
        (bf (get-buffer-create "*Git Output*"))
        (cmd1 (car (split-string cmd )))
        (max-mini-window-height git-command-max-mini-window-height))
    (delete-windows-on bf t)
    (shell-command (concat "git "
                           git-command-default-options
                           " "
                           cmd)
                   bf)
    (with-current-buffer bf
      (cd dir)
      (and (require 'ansi-color nil t)
           (ansi-color-apply-on-region (point-min)
                                       (point-max)))
      (setq buffer-read-only t)
      (view-mode 1)
      (funcall (or (cdr (assoc cmd1
                               git-command-modes-alist))
                   'fundamental-mode))
      )))

(provide 'git-command)
