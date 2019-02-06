;;; git-worktree.el --- Manage git worktrees  -*- lexical-binding: t; -*-

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-worktree-el
;; Package-Version: 20190206.1250
;; Version: 0.0.1
;; Package-Requires: ()
;; Keywords: util git vcs

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>


;;; Commentary:

;; Manage git worktrees via Emacs buffer.

;; M-x git-worktree to open a git-worktree buffer.
;; Inside of this buffer, you can use following keybind:

;; - ENTER: Go to directory at point (git-worktree-mode-go)
;; - A:     Add new worktree to git repository (git-worktree-mode-add)
;; - R:     Move worktree path at point (git-worktree-mode-move)
;; - D:     Remove worktree at point (git-worktree-mode-remove)

;;; Code:

(defun git-worktree-get-current-trees ()
  "Get current worktree list."
  (with-temp-buffer
    (git-worktree--call-process "worktree" "list" "--porcelain")
    (goto-char (point-min))
    (let ((trees nil))
      (save-match-data
        (while (not (eq (point) (point-max)))
          (let ((worktree nil)
                (head nil)
                (branch nil)
                (bare nil))
            (while (or (re-search-forward "^\\([^ ]+\\) \\(.*\\)$" (point-at-eol) t)
                       (re-search-forward "^\\([^ ]+\\)$" (point-at-eol) t))
              (pcase (match-string 1)
                ("worktree" (setq worktree (match-string 2)))
                ("HEAD" (setq head (match-string 2)))
                ("branch" (setq branch (match-string 2)))
                ("bare" (setq bare t))
                )
              (forward-line 1)
              (goto-char (point-at-bol)))
            (setq trees `(,@trees
                          (
                           :worktree ,worktree
                           :head ,head
                           :branch ,branch
                           :bare ,bare
                           )))
            (forward-line 1)
            (goto-char (point-at-bol)))
          ))
      trees)))

(defun git-worktree--call-process (&rest args)
  "Start git process synchronously with ARGS.

Raise error when git process ends with non-zero status.
Any output will be written to current buffer."
  (let ((status (apply 'call-process
                       "git"
                       nil
                       t
                       nil
                       args)))
    (cl-assert (eq status 0)
               nil
               (buffer-substring-no-properties (point-min) (point-max)))))

(defun git-worktree--get-repository-root (dir)
  "Resolve repository root of DIR.

If DIR is not inside of any git repository, signal an error."
  (cl-assert (file-directory-p dir))
  (with-temp-buffer
    (cd dir)
    (git-worktree--call-process "rev-parse" "--show-toplevel")
    (goto-char (point-min))
    (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
;;(git-worktree--get-repository-root default-directory)

(defun git-worktree-open-noselect (&optional directory)
  "Open git worktree list buffer.

If optional arg DIRECTORY is given change current directory to there before
initializing."
  (setq directory (expand-file-name (or directory
                                        default-directory)))
  (cl-assert (file-directory-p directory))
  (let* ((root (git-worktree--get-repository-root directory))
         (name (file-name-nondirectory root))
         (bname (format "*GitWorktree<%s>*" name)))
    (with-current-buffer (get-buffer-create bname)
      (cd root)
      (git-worktree--set-tabulated-list-mode-variables)
      (git-worktree-mode)
      (current-buffer))))
;; ((:worktree "/Users/10sr/.dotfiles" :head "5e7457a8d49ef6a517cdf39d038ba5fdf98dc68e" :branch "refs/heads/master") (:worktree "/Users/10sr/.dotfiles/b1" :head "fa7d868076d807692e35f82ae23596c903fd1117" :branch "refs/heads/b1"))

(defun git-worktree--set-tabulated-list-mode-variables ()
  "Set variables for `tabulated-list-mode'."
  (let ((trees (git-worktree-get-current-trees)))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list e
                          (vector
                           (concat (file-relative-name (plist-get e :worktree))
                                   "/")
                           (or (plist-get e :branch)
                               ;; bare worktree do not have head attr
                               "N/A")
                           (or (plist-get e :head)
                               ;; bare worktree do not have head attr
                               "N/A")
                           )))
                  trees))
    (let ((branch-max-size
           (apply 'max
                  (length "Branch") ;; Header text
                  (cl-loop for e in tabulated-list-entries
                           collect (length (elt (cadr e) 1)))))
          (worktree-max-size
           (apply 'max
                  (length "Worktree") ;; Header text
                  (cl-loop for e in tabulated-list-entries
                           collect (length (elt (cadr e) 0))))))
      (setq tabulated-list-format
            `[
              ("Worktree" ,worktree-max-size t)
              ("Branch" ,branch-max-size t)
              ("Head" -1 t)
              ]))))

;;;###autoload
(defun git-worktree-open (&optional directory)
  "Open git worktree list buffer.

If optional arg DIRECTORY is given change current directory to there before
initializing."
  (interactive)
  (let ((bf (git-worktree-open-noselect directory)))
    (pop-to-buffer bf)))
;;;###autoload
(defalias 'git-worktree 'git-worktree-open)

(defun git-worktree-mode-go ()
  "Go to worktree directory at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (path (plist-get id :worktree)))
    (cl-assert path nil "No worktree info at point")
    (cl-assert (file-directory-p path) t "Directory not found")
    (dired path)))

(defun git-worktree-mode-move ()
  "Move worktree at point to a new location."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (path (plist-get id :worktree)))
    (cl-assert path nil "No worktree info at point")
    (cl-assert (file-directory-p path) t "Directory not found")
    (let ((new (read-file-name (format "New name for worktree \"%s\": "
                                       path))))
      (with-temp-buffer
        (git-worktree--call-process "worktree"
                                    "move"
                                    path
                                    (expand-file-name new)))
      (revert-buffer))))

(defun git-worktree-mode-add ()
  "Add new git worktree."
  (interactive)
  (let* ((path (read-file-name "Path of new worktree: "))
         (commitish (read-string (format "Commitish to checkout to worktree \"%s\" (Empty to use the same name): "
                                         path)))
         (args (append '("worktree" "add")
                       (if (string= "" commitish)
                           (list (expand-file-name path))
                         (list (expand-file-name path) commitish)))))
    (with-temp-buffer
      (apply 'git-worktree--call-process args))
    (revert-buffer)))

(defun git-worktree-mode-remove ()
  "Remove worktree at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (path (plist-get id :worktree)))
    (cl-assert path nil "No worktree info at point")
    (cl-assert (file-directory-p path) t "Directory not found")
    (when (yes-or-no-p (format "Remove workking directory \"%s\": "
                               path))
      (with-temp-buffer
        (git-worktree--call-process "worktree"
                                    "remove"
                                    path))
      (revert-buffer))))

(defvar git-worktree-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "A" 'git-worktree-mode-add)
    (define-key map (kbd "C-m") 'git-worktree-mode-go)
    (define-key map "R" 'git-worktree-mode-move)
    (define-key map "D" 'git-worktree-mode-remove)
    ;; (define-key map (kbd "C-g") 'git-worktree-mode-close)
    (define-key map "/" 'isearch-forward)
    map))

(define-derived-mode git-worktree-mode tabulated-list-mode "Git-Worktrees"
  "Major mode for browsing recently opened files and directories."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            'git-worktree--set-tabulated-list-mode-variables
            nil
            t)
  (tabulated-list-init-header)
  (tabulated-list-print nil nil))


(provide 'git-worktree)

;;; git-worktree.el ends here
