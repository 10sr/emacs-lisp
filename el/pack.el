;;; pack.el --- Pack and unpack archive files

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/pack.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords:

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

;; pack.el provides 3 functions: `pack-pack', `pack-unpack' and
;; `pack-dired-dwim'.

;;; Code:

(eval-when-compile
  (require 'simple))

(declare-function dired-dwim-target-directory "dired-aux")
(declare-function dired-get-marked-files "dired")

(defvar pack-buffer-name "*Pack*"
  "Buffer name for `pack'.")

(defvar pack-default-extension
  "7z"
  "Default suffix for packing.
Filename with this suffix must matches one of the cars in
`pack-program-alist'.")

(defvar pack-program-alist
  `(
    ("\\.7z\\'" "7z a" "7z x")
    ("\\.zip\\'" "zip -r" "unzip")
    ("\\.tar\\'" "tar cf" "tar xf")
    ("\\.tgz\\'" "tar czf" "tar xzf")
    ("\\.tar\\.gz\\'" "tar czf" "tar xzf")
    )
  "Alist of filename patterns, and command for pack and unpack.

Each element looks like (REGEXP PACKING-COMMAND UNPACKING-COMMAND).
PACKING-COMMAND and UNPACKING-COMMAND can be nil if the command is not
available.  Alist is searched from the beginning so pattern for \".tar.gz\"
should be ahead of pattern for \".gz\"")

;;;###autoload
(defun pack-dired-dwim ()
  "Pack or unpack files in dired.

If targetting one file and that has a archive suffix defined in
`pack-program-alist', unpack that.
Otherwise, pack marked files, prompting user to decide archive filename."
  (interactive)
  (let* ((infiles (dired-get-marked-files t))
         (onefile (and (eq 1 ; filename if only one file targeted, otherwise nil
                           (length infiles))
                       (car infiles))))
    (if (and onefile
             (pack-file-name-association onefile))
        (when (y-or-n-p (format "Unpack %s? " onefile))
          (pack-unpack onefile))
      (let* ((dir-default (if (require 'dired-aux nil t)
                              (dired-dwim-target-directory)
                            default-directory))
             (archive-default (pack--ensure-archive-extension (file-name-nondirectory
                                                    (car infiles))))
             (archive ;; (if (interactive-p)
              (read-file-name "Archive file name: "
                              dir-default
                              nil
                              nil
                              archive-default)
              ;; (concat dir-default archive-default)
              ))
        (apply 'pack-pack
               archive
               infiles))))
  (revert-buffer)
  ;; (dired-unmark-all-marks)
  )

(defun pack--ensure-archive-extension (filename)
  "If FILENAME has extension and it can be used for pack, return FILENAME.
Otherwise, return FILENAME with `pack-default-extension'"
  (if (pack-file-name-association filename)
      filename
    (concat filename "." pack-default-extension)))

(defun pack-file-name-association (filename)
  "Return commands to pack and unpack FILENAME archive file.

If the pattern matching FILENAME is found at car of the list in
`pack-program-alist', return cdr of that list.  Otherwise, return nil."
  (let ((case-fold-search nil))
    (assoc-default filename
                   pack-program-alist
                   'string-match-p
                   nil)))

;;;###autoload
(defun pack-unpack (archive)
  "Unpack ARCHIVE.

Command for unpacking is defined in `pack-program-alist'."
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
      (message "Cannot find unpacking command for %s"
               archive))))

;;;###autoload
(defun pack-pack (archive &rest files)
  "Make ARCHIVE from FILES.

If ARCHIVE have extension defined in `pack-program-alist', use that command.
Otherwise, use `pack-default-extension' for pack."
  (let* ((archive-ext (pack--ensure-archive-extension (expand-file-name archive)))
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
      (message "Invalid extension for packing: %s"
               archive-ext))))

(provide 'pack)

;;; pack.el ends here
