(defvar git-command-history nil
  "History list for `git-command'.")

(defun git-command-ps1 (str)
  (let ((gcmpl "/etc/bash_completion.d/git"))
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
