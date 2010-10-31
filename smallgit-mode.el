;; (defun smallgit-mode ()
;;   ""
;;   (interactive)
;;   (use-local-map smallgit-mode-map))

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
 '(("\C-xvv" . smallgit-commit-all)
   ("\C-xvi" . smallgit-add)))


(defvar smallgit-mode-hook nil)

;; (defvar smallgit-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key map (kbd "C-x v v") 'smallgit-add)
;;     (define-key map (kbd "C-x v i") 'smallgit-add)))


(defun load-smallgit-mode ()
  ""
  (interactive)
  (when (file-directory-p "./.git")
    (smallgit-mode 1)))

(defvar smallgit-log-buffer "*smallgit-log*")

;; (defun smallgit--call-git (&rest ARGS)
;;   "not needed?"
;;   (shell-command (concat "git"
;;                          (apply 'concat ARGS))
;;                  smallgit-log-buffer))

(defun smallgit-init ()
  ""
  (interactive)
  (shell-command "git init" smallgit-log-buffer))

(defun smallgit-add (&optional file)
  ""
  (interactive)
  (shell-command (concat "git add "
                         (or file
                             (buffer-file-name)))
                 smallgit-log-buffer)
  (message "smallgit: added"))

(defun smallgit-add-all ()
  ""
  (interactive)
  (shell-command "git add -A" smallgit-log-buffer)
  (message "smallgit: added all"))

(require 'log-edit)

(defun smallgit-commit-all ()
  ""
  (interactive)
  (smallgit-add-all)
  (log-edit (lambda ()
              (interactive)
              (smallgit--commit (save-excursion
                                  (set-buffer "*smallgit commit*")
                                  (buffer-substring-no-properties (point-max)
                                                                  (point-min)))))
            t
            nil
            (get-buffer-create "*smallgit commit*")))


(defun smallgit--commit (message)
  "call from `smallgit-commit-all'"
  (shell-command (concat "git commit -m \"" message "\"")))

(defun smallgit-push ()
  ""
  (interactive)
  (shell-command "git push"))

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





