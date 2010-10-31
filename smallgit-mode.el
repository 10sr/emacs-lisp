(defun smallgit-mode ()
  ""
  (interactive))

(defun smallgit--call-git (&rest ARGS)
  "not needed?"
  (apply 'call-process "git" nil nil nil ARGS))

(defun smallgit-init ()
  ""
  (interactive)
  (call-process "git" nil nil nil "init"))

(defun smallgit-add (&optional file)
  ""
  (interactive)
  (call-process "git" nil nil nil "add" (or file (buffer-file-name))))

(defun smallgit-add-all ()
  ""
  (interactive)
  (call-process "git" nil nil nil "add" "-A"))

;; (defun smallgit-update-index-add (&optional file)
;;   ""
;;   (start-process "git" nil nil nil "update-index" "-add" file))

;; (defun smallgit-commit-all ()
;;   ""
;;   (interactive)
;;   (smallgit-add-all)
;;   (log-edit (lambda ()
;;               (call-process "git" nil nil nil "commit" "-m" comment))





