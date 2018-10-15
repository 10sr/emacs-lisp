(defmacro string-empty-p (str)
  "Return true if STR is empty (0 length)."
  `(string-equal ,str
                 ""))


(defvar git-revisions-git-executable
  (executable-find "git")
  "Executable name of git.")

(defun git-revisions-handle-file-normally (operation &rest args)
  "Handle file for OPERATION with ARGS normally."
  (let ((inhibit-file-name-handlers
         (cons 'git-revisions-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun git-revisions--current-repository-root (&optional dir)
  "Return full-path for git repository root for DIR or nil."
  (git-revisions--command-to-string dir
                                    "rev-parse"
                                    "--show-toplevel"))

(defun git-revisions--command-to-string (dir &rest args)
  "Execute git command with ARGS in DIR and return first line of output.

This function will return nil if git command execution failed."
  (with-temp-buffer
    (and dir
         (cd dir))
    (let ((status (apply 'call-process
                         git-revisions-git-executable
                         nil
                         t
                         nil
                         args)))
      (and (eq 0
               status)
           (buffer-substring-no-properties (point-min)
                                           (progn
                                             (goto-char (point-min))
                                             (point-at-eol)))))))

(defun git-revisions-parse-path (name)
  "Parse path for git-revisions.

NAME should be a string like /path/to/repository/git@HEAD:a.txt, and this
function returns list like (\"/path/to/repository\" \"HEAD\" \"a.txt\").

The returns will be a list of strings like (FORMER REVISION LATTER), where

* revision: String represent commit (some refs or commit hash)
* former: Path to repository
+ latter: Path in repository


For example:

name: /path/to/repo/git@abcdef:abc
  * revision: abcdef
  * former: /path/to/repo
  * latter: abc

name: git@abcdef:abc
  * revision: abcdef
  * former:
  * latter: abc
"
  (let ((splitted (split-string name
                                "\\(^\\|/\\)git@[^:]*:"))
        (revision (and (string-match "\\(^\\|/\\)git@\\([^:]*\\):"
                                     name)
                       (match-string 2
                                     name))))
    (let ((former (if (string-match-p "^/git@"
                                      name)
                      "/"
                    (car splitted)))
          (latter (nth 1 splitted)))
      (list (or former
                "")
            (or revision
                "")
            (or latter
                "")))))

;; Handlers

(defun git-revisions-handler-expand-file-name (name &optional directory)
  "Convert filename NAME to absolute, and canonicalize it.
Second arg DIRECTORY is directory to start with if NAME is relative
(does not start with slash or tilde); both the directory name and
a directory's file name are accepted.  If DIRECTORY is nil or
missing, the current buffer's value of `default-directory' is used.
NAME should be a string that is a valid file name for the underlying
filesystem.



If NAME contains string like \"git@HEAD:\", it is considered to be
a file that was committed to a git repository, and expansion will be
done as folllows:


NAME: /path/to/repository/git@HEAD:a.txt

In this case, \"/path/to/repository\" MUST be a root directory of git
repository.  This function does not verify that and return as it is.


In the following exapmles, assume current directory is:
default-directory: \"/current/directory\"


NAME: ./path/git@HEAD:a.txt

Directory \"/current/directory/path\" MUST be a root directory of git
repository.  This function does not verify that.  The return value will
be \"/current/directory/path/git@HEAD:a.txt


NAME: git@HEAD:a.txt

Case1: \"/current/directory\" is a root directory of git repository
In this case, the return value will be \"/current/directory/git@HEAD:a.txt\"

Case2: \"/current\" is a root directory of git repository
The return value will be \"/current/git@HEAD:directory/a.txt\"

Case3: Current directory is not inside of git repository
This handler does \"nothing\" and fallback to normal resolution."

(let* ((directory (file-name-as-directory (or directory
                                              default-directory)))
       (parsed (git-revisions-parse-path name))
       (former (car parsed))
       (revision (nth 1 parsed))
       (latter (nth 2 parsed)))

  (if (not (string-empty-p former))
      ;; If former is not empty
      (concat (expand-file-name former)
              "/git@"
              revision
              ":"
              ;; FIXME: Strip string like "./"
              ;; For example, "./a.txt" should be "a.txt" here
              ;; FIXME: Potential bug: latter like "../a.txt" may suck
              latter)

    ;; If former is empty (NAME starts with "git@")
    (let ((former (git-revisions--current-repository-root)))
      (if former
          (let ((latter (file-relative-name (concat default-directory
                                                    latter)
                                            former)))
            (concat former
                    "/git@"
                    revision
                    ":"
                    latter))
        ;; Current directory is not under git repository
        (git-revisions-handle-file-normally 'expand-file-name
                                             directory))))))


(defun git-revisions-handler-insert-file-contents (filename &optional visit beg end replace)
  "Insert contents of file FILENAME after point.
Returns list of absolute file name and number of characters inserted.
If second argument VISIT is non-nil, the buffer's visited filename and
last save file modtime are set, and it is marked unmodified.  If
visiting and the file does not exist, visiting is completed before the
error is signaled.

The optional third and fourth arguments BEG and END specify what portion
of the file to insert.  These arguments count bytes in the file, not
characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.

If optional fifth argument REPLACE is non-nil, replace the current
buffer contents (in the accessible portion) with the file contents.
This is better than simply deleting and inserting the whole thing
because (1) it preserves some marker positions and (2) it puts less data
in the undo list.  When REPLACE is non-nil, the second return value is
the number of characters that replace previous buffer contents.
"
  (setq filename
        (expand-file-name filename))
  (let* ((buf (current-buffer))
         (parsed (git-revisions-parse-path filename))
         (gitobj (concat (nth 1
                              parsed)
                         ":"
                         (nth 2
                              parsed))))
    (if (and (string-empty-p (nth 1
                                  parsed))
             (string-empty-p (nth 2
                                  parsed)))
        ;; Both REVISION and LATTER are empty strings
        (git-revisions-handle-file-normally 'insert-file-contents
                                            filename
                                            visit
                                            beg
                                            end
                                            replace)

      (let ((type (git-revisions--command-to-string (car parsed)
                                                    "cat-file"
                                                    "-t"
                                                    gitobj)))
        (if (string-equal type
                          "blob")
            (let ((str (with-temp-buffer
                         (cd (car parsed))
                         (call-process git-revisions-git-executable
                                       nil
                                       t
                                       nil
                                       "cat-file"
                                       "-p"
                                       gitobj)
                         ;; FIXME: I think this is not exact:
                         ;; Offsets are interpreted as string position, but
                         ;; actually they should be treated as byte position!
                         (buffer-substring (or beg
                                               (point-min))
                                           (or end
                                               (point-max))))))
              (when replace
                (goto-char (point-min)))
              (insert str)
              (when replace
                (delete-region (point)
                               (point-max)))
              (when visit
                (setq buffer-file-name
                      filename)))
          (error "Object type is not valid. name: %s type: %s"
                 filename
                 type))))))

(defvar git-revisions-inhibit nil
  "Non-nil means don't try to handle git-revisions.")

;;;###autoload
(defun git-revisions-file-handler (operation &rest args)
  (save-match-data
    (let ((op (get operation 'git-revisions)))
      (if (and op
               (not git-revisions-inhibit))
          (apply op args)
        (git-revisions-run-real-handler operation args)))))

(defun git-revisions-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
         (cons 'git-revisions-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(add-to-list 'file-name-handler-alist
             '("\\(:?\\`\\|.*/\\)git@.*\\'" . git-revisions-file-handler))

(defun git-revisions-directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY."
  (message "handler called!")
  (if full
      '("a" "b")
    (list (concat directory "/a")
          (concat directory "/b"))))

(defun git-revisions-insert-file-contents (filename &optional visit beg end replace)
  "Innsert contents of file FILENAME ater point."
  (insert "insert")
  (cons filename (length "insert")))

(put 'directory-files 'git-revisions 'git-revisions-directory-files)
(put 'insert-file-contents 'git-revisions 'git-revisions-insert-file-contents)

(provide 'git-revisions)
