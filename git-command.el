(defvar git-command-history nil
  "History list for `git-command'.")

(defvar git-command-completion-file (or (git-command-file-find "/etc/bash_completion.d/git")
                                        (git-command-file-find "/opt/local/etc/bash_completion.d/git")))

(defun git-command-file-find (f)
  "Return F if F exists, otherwise return nil."
  (and (file-readable-p f)
       f))

(defun git-command-ps1 (str)
  (let ((gcmpl git-command-completion-file))
    (if (file-readable-p gcmpl)
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
        )
    (delete-windows-on bf t)
    (shell-command (concat "git "
                           cmd)
                   bf)
    (with-current-buffer bf
      (cd dir)
      (and (require 'ansi-color nil t)
           (ansi-color-apply-on-region (point-min)
                                       (point-max)))
      (setq buffer-read-only t)
      (view-mode 1)
      )))

(provide 'git-command)
