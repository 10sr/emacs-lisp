;;; git-command.el --- Dead simple git command interface

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/git-command.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility git

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

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Dead simple git interface.  No major-mode, only provides command-line like
;; interface using minibuffer.  You need not remember additional keybinds for
;; using git from Emacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar git-command-default-options
  nil
  "List of options always passed to git.")


;; variables for __git_ps1
(defvar git-command-ps1-showdirtystate "t"
  "Value of  GIT_PS1_SHOWDIRTYSTATE when running __git_ps1.")

(defvar git-command-ps1-showstashstate ""
  "Value of GIT_PS1_SHOWSTASHSTATE when running __git_ps1.")

(defvar git-command-ps1-showuntrackedfiles ""
  "Value of GIT_PS1_SHOWUNTRACKEDFILES when running __git_ps1.")

(defvar git-command-ps1-showupstream "auto"
  "Value of GIT_PS1_SHOWUPSTREAM when running __git_ps1.")


(defvar git-command-history nil
  "History list for `git-command'.")


(defvar git-command-view-command-list
  '("log" "show" "help")
  "List of commands that will only output something for read.")

(defvar git-command-aliases-alist
  '(("diff" . (lambda (options cmd args new-buffer-p)
                (let ((buf (if new-buffer-p
                               (generate-new-buffer "*git diff*")
                             (get-buffer-create "*git diff*"))))
                  (with-current-buffer buf
                    (erase-buffer)
                    (shell-command (concat "git "
                                           (git-command-construct-commandline
                                            `(,@options "-c" "color.diff=never")
                                            "diff"
                                            args))
                                   t)
                    (diff-mode))
                  (display-buffer buf))))
    ("grep" . (lambda (options cmd args new-buffer-p)
                (compilation-start (concat "git "
                                           (git-command-construct-commandline
                                            `(,@options "--no-pager"
                                                        "-c" "color.grep=false")
                                            cmd
                                            `("-nHe" ,@args)))
                                   'grep-mode
                                   (and new-buffer-p
                                        (lambda (s)
                                          (generate-new-buffer-name "*git grep*"))))
                )))
  "Alist of cons of command and function to run.
The function should get three argument: see `git-command-exec'.")

(defun git-command-find-git-ps1 (f)
  "Return F if F exists and it contain function \"__git_ps1\"."
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
      (git-command-find-git-ps1
       "/opt/local/share/doc/git-core/contrib/completion/git-prompt.sh")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git-prompt")
      (git-command-find-git-ps1
       "/opt/local/share/git-core/git-prompt.sh")
      (git-command-find-git-ps1 "/opt/local/etc/bash_completion.d/git")
      ))


(defvar git-command-use-emacsclient
  nil
  "If non-nil use emacsclient for editor of git.
In this case, `server-start' will be called at the first call of `git-command'
if Emacs server is not running.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defun git-command--git-dir ()
  "Execute \"git rev-parse --git-dir\" and return result string or nil."
  (with-temp-buffer
    (and (eq 0
             (call-process "git"
                           nil
                           t
                           nil
                           "rev-parse" "--git-dir"))
         (progn (goto-char (point-min))
                (buffer-substring-no-properties (point-at-bol)
                                                (point-at-eol))))))

(defun git-command-ps1 (fmt)
  "Generate git ps1 string from FMT and return that string."
  (let ((gcmpl (or git-command-prompt-file))
        (process-environment `(,(concat "GIT_PS1_SHOWDIRTYSTATE="
                                        git-command-ps1-showdirtystate)
                               ,(concat "GIT_PS1_SHOWSTASHSTATE="
                                        git-command-ps1-showstashstate)
                               ,(concat "GIT_PS1_SHOWUNTRACKEDFILES="
                                        git-command-ps1-showuntrackedfiles)
                               ,(concat "GIT_PS1_SHOWUPSTREAM="
                                        git-command-ps1-showupstream)
                               ,@process-environment)))
    (if (and (executable-find "bash")
             gcmpl
             (file-readable-p gcmpl))
        (with-temp-buffer
          (insert ". " gcmpl
                  "; __git_ps1 "
                  (shell-quote-argument fmt)
                  ";")
          (shell-command-on-region (point-min)
                                   (point-max)
                                   "bash -s"
                                   nil
                                   t)
          (buffer-substring-no-properties (point-min)
                                          (point-max)))
      "")))

(defun git-command-get-alias-function (cmd)
  "Return alias function for CMD if available in `git-command-alias-alist'."
  (cdr (assoc cmd
              git-command-aliases-alist)))

;; commandline parsing
(defun git-command-construct-commandline (options command args)
  "Construct one commandline string from OPTIONS COMMAND and ARGS.
TODO: how to do about `git-command-default-options'?
The string returned does not start with \"git\" so this should be concat-ed.

About these arguments see document of `git-command-parse-commandline'."
  (mapconcat 'shell-quote-argument
             `(,@options ,command ,@args)
             " "))

(defun git-command-parse-commandline (str)
  "Parse commandline string STR into a list like (OPTIONS COMMAND ARGUMENT)."
  (git-command-part-commands-with-subcommand
   (git-command-shell-split-string
    str)))

(defun git-command-shell-split-string (str)
  "Split string STR into strings by shell."
  (let ((emacs-bin (concat invocation-directory
                           invocation-name)))
    (cdr (read (shell-command-to-string (concat emacs-bin
                                                " -Q --batch --eval '(prin1 command-line-args-left)' -- "
                                                str
                                                " 2>/dev/null"))))))

(defun git-command-part-commands-with-subcommand (l)
  "Partition git args list L into (OPTIONS COMMAND ARGUMENTS) and return it.
Only COMMAND is string, others are lists.
OPTIONS is distinguished by if they start with hyphens.
\"-c\" option is a special case which takes one parameter."
  (let ((i (git-command-find-subcommand l)))
    ;; make list that contains from 0th to ith element
    (list
     ;; OPTIONS
     (if (eq 0
             i)
         nil
       (let ((ol (copy-sequence l)))
         (setcdr (nthcdr (- i 1)
                         ol)
                 nil)
         ol))
     ;; COMMAND
     (or (nth i
              l)
         "")
     ;; ARGUMENTS
     (nthcdr (1+ i)
             l)
     )))

(defun git-command-find-subcommand (l &optional index)
  "Find subcommand from list L and return the index number.
Options that lead subcommand are distinguished by if they start with hyphens.
\"-c\" option is a special case which take one parameter.
If no subcommand was found, returns the length of L.

INDEX is always the original index of car of L.
The value nil is equivalent to 0."
  (let ((options-w-param '("-c"))
        (first (car l))
        (rest (cdr l))
        (i (or index
               0)))
    (if l

        ;; l is not empty
        (if (member first
                    options-w-param)
            ;; if first is the command that requires a parameter, skip the parameter
            (if rest
                (git-command-find-subcommand (cdr rest)
                                             (+ 2 i))
              ;; Only -c is given: this is undesirable situation, but anyway returns the length of original L
              (1+ i)
              )

          (if (eq ?-
                  (aref first
                        0))
              ;; the first element is not in optinos-w-params but
              ;; the first letter of first element of L is '-'
              (git-command-find-subcommand rest
                                           (1+ i))
            i))

      ;; if L is empty returns the original length of L
      i)))


;; emacs client

(eval-when-compile
  (require 'server nil t))

(defun git-command--construct-emacsclient-command ()
  "Construct and return command in a string to connect to current Emacs server."
  (if server-use-tcp
      (format "%s -f \"%s/%s\""
              "emacsclient"
              (expand-file-name server-auth-dir)
              server-name)
    (format "%s -s \"%s/%s\""
            "emacsclient"
            server-socket-dir
            server-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user commands

(eval-when-compile
  (require 'ansi-color nil t))

(defun git-command (cmd &optional new-buffer-p)
  "Shell like git command interface.  CMD is the commandline strings to run.

If NEW-BUFFER-P is non-nil, generate new buffer for running command."
  (interactive (list (read-shell-command (format "[%s]%s $ git : "
                                                 (abbreviate-file-name
                                                  default-directory)
                                                 (git-command-ps1 "[GIT:%s]"))
                                         nil
                                         'git-command-history)
                     current-prefix-arg))
  (apply 'git-command-exec (append (git-command-parse-commandline cmd)
                                   (list new-buffer-p))))

(defun git-command-exec (options command args &optional new-buffer-p)
  "Execute git.

This function accept three arguments.  OPTIONS is a list of options that will be
passed to git itself.  Tipically they are appeared before the git subcommand.
COMMAND is a string of git subcommand.  ARGS is a list of arguments for git
subcommand.

These arguments are tipically constructed with `git-command-parse-commandline'.

Set optional argument NEW-BUFFER-P to non-nil to generate new buffer for the
process."
  (let ((alias (git-command-get-alias-function command)))
    (if alias
        ;; if alias is defined in git-command-get-alias-function
        (funcall alias
                 options command args new-buffer-p)
      (if (member command
                  git-command-view-command-list)
          ;; if this command is a view command
          (let* ((bname (concat "*"
                                "git "
                                command
                                "*"))
                 (bf (if new-buffer-p (generate-new-buffer bname)
                       (and (get-buffer bname)
                            (kill-buffer bname))
                       (get-buffer-create bname))))
            (display-buffer bf)
            (with-current-buffer bf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (if (fboundp 'ansi-color-apply-on-region)
                    (progn
                      (shell-command (concat "git "
                                             (git-command-construct-commandline
                                              `(,@git-command-default-options
                                                ,@options
                                                "-c"
                                                "color.ui=always")
                                              command
                                              args))
                                     t)
                      (ansi-color-apply-on-region (point-min)
                                                  (point-max)))
                  (shell-command (concat "git "
                                         (git-command-construct-commandline
                                          `(,@git-command-default-options
                                            ,@options "-c" "color.ui=never")
                                          command
                                          args))
                                 t))
                (fundamental-mode)
                (view-mode))))
        ;; if this command is not a view command
        (and git-command-use-emacsclient
             (require 'server nil t)
             (not (server-running-p))
             (server-start))
        (let ((process-environment
               (if git-command-use-emacsclient
                   `(,(concat "GIT_EDITOR="
                              (git-command--construct-emacsclient-command))
                     ,@process-environment)
                 process-environment)))
          (git-command-term-shell-command
           (concat "git "
                   (git-command-construct-commandline
                    ;; TODO: fix colorize: currently output is not colorized
                    `(,@git-command-default-options
                      ,@options "-c" "color.ui=always")
                    command
                    args))
           (if new-buffer-p
               (generate-new-buffer "*git command*")
             "*git command*")))))))

(eval-when-compile
  (require 'term nil t))
(defvar term-shell-command-history nil
  "History for term-shell-command.")
(defun git-command-term-shell-command (command &optional buffer-or-name)
  "Run COMMAND in terminal emulator.
If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in the
buffer is destroyed.  Otherwise, new buffer is generated automatically from
COMMAND."
  (interactive (list (read-shell-command "Run program: "
                                         nil
                                         'term-shell-command-history)))
  (let* ((name (car (split-string command
                                  " ")))
         (buf (if buffer-or-name
                  (get-buffer-create buffer-or-name)
                (generate-new-buffer (concat "*"
                                             name
                                             "*"))))
         (proc (get-buffer-process buf))
         (dir default-directory))
    (and proc
         (delete-process proc))
    (display-buffer buf)
    (with-current-buffer buf
      (cd dir)
      (set (make-local-variable 'term-scroll-to-bottom-on-output)
           t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert "Start executing "
                command)
        (add-text-properties (point-at-bol)
                             (point-at-eol)
                             '(face bold))
        (insert "\n\n"))
      (require 'term)
      (term-mode)
      (term-exec buf
                 (concat "term-" name)
                 shell-file-name
                 nil
                 (list shell-command-switch
                       command))
      (term-char-mode)
      (if (ignore-errors (get-buffer-process buf))
          (set-process-sentinel (get-buffer-process buf)
                                (lambda (proc change)
                                  (with-current-buffer (process-buffer proc)
                                    (term-sentinel proc change)
                                    (goto-char (point-max)))))
        ;; (goto-char (point-max))
        ))))

(provide 'git-command)

;;; git-command.el ends here
