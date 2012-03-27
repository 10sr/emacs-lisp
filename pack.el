(require 'simple)

(declare-function dired-dwim-target-directory "dired-aux")

(defvar 7z-program-name
  (or (executable-find "7z")
      (executable-find "7za")
      (executable-find "7zr"))
  "Path to 7z program.")
(defvar pack-buffer-name "*Pack*"
  "Buffer name for `pack'")

(defvar pack-default-extension
  "7z"
  "Default suffix for packing. Filename with this suffix must matches one of `pack-program-alist'.")

(defvar pack-program-alist
  `(
    ("\\.7z\\'" ,(concat 7z-program-name " a") ,(concat 7z-program-name " x"))
    ("\\.zip\\'" "zip -r" "unzip")
    ("\\.tar\\'" "tar cf" "tar xf")
    ("\\.tgz\\'" "tar czf" "tar xzf")
    ("\\.tar\\.gz\\'" "tar czf" "tar xzf")
    )
  "Alist of filename patterns, command for pack and unpack.
Each element looks like (REGEXP PACKING-COMMAND UNPACKING-COMMAND).
PACKING-COMMAND and UNPACKING-COMMAND can be nil if the command is not available.
Alist is searched from the beginning so pattern for \".tar.gz\" should be ahead of pattern for \".gz\"")

(defun dired-do-pack-or-unpack ()
  "Pack or unpack files in dired.
If targetting one file and that is archive file defined in `pack-program-alist', unpack that.
Otherwise, pack marked files, prompting user to decide filename for archive."
  (interactive)
  (let* ((infiles (dired-get-marked-files t))
         (onefile (and (eq 1 ; filename if only one file targeted, otherwise nil.
                           (length infiles))
                       (car infiles))))
    (if (and onefile
             (pack-file-name-association onefile))
        (when (y-or-n-p (format "unpack %s? " onefile))
          (unpack onefile))
      (let* ((dir-default (if (require 'dired-aux nil t)
                              (dired-dwim-target-directory)
                            default-directory))
             (archive-default (pack-file-extension (file-name-nondirectory (car infiles))))
             (archive ;; (if (interactive-p)
              (read-file-name "Output file to pack : "
                              dir-default
                              nil
                              nil
                              archive-default)
              ;; (concat dir-default archive-default)
              ))
        (apply 'pack
               archive
               infiles))))
  (revert-buffer)
  ;; (dired-unmark-all-marks)
  )

(defun pack-file-extension (filename)
  "If FILENAME has extension and it can be used for pack, return FILENAME.
Otherwise, return FILENAME with `pack-default-extension'"
  (if (pack-file-name-association filename)
      filename
    (concat filename "." pack-default-extension)))

(defun pack-file-name-association (filename)
  "If the pattern matching FILENAME is found at car of the list in `pack-program-alist', return cdr of that list.
Otherwise, return nil."
  (let ((case-fold-search nil))
    (assoc-default filename
                   pack-program-alist
                   'string-match-p
                   nil)))

(defun unpack (archive)
  "Unpack ARCHIVE. Command for unpacking is defined in `pack-program-alist'"
  (interactive "fArchive to extract: ")
  (let* ((earchive (expand-file-name archive))
         (cmd (nth 1
                   (pack-file-name-association earchive)))
         )
    (if cmd
        (async-shell-command (concat cmd 
                               " "
                               (shell-quote-argument earchive))
                       (get-buffer-create pack-buffer-name))
      (message "this is not archive file defined in `pack-program-alist'!"))))

(defun pack (archive &rest files)
  "Pack FILES into ARCHIVE.
If ARCHIVE have extension defined in `pack-program-alist', use that command.
Otherwise, use `pack-default-extension' for pack."
  (let* ((archive-ext (pack-file-extension (expand-file-name archive)))
         (cmd (car (pack-file-name-association archive-ext)))
         )
    (if cmd
        (async-shell-command (concat cmd
                               " "
                               (shell-quote-argument archive-ext)
                               " "
                               (mapconcat 'shell-quote-argument
                                          files
                                          " "))
                       (get-buffer-create pack-buffer-name))
      (message "invalid extension for packing!"))))

(provide 'pack)
