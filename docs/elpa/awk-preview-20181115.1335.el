;;; awk-preview.el --- Preview and Apply AWK Filter

;; Copyright (C) 2018 10sr

;; Author: 10sr<8.slashes@gmail.com>
;; URL: https://github.com/10sr/awk-preview-el
;; Package-Version: 20181115.1335
;; Version: 0.0.1
;; Package-Requires: ()

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

;; awk-preview

;;; Code:

(defgroup awk-preview nil
  "AWK previewer."
  :tag "AWK Preview"
  :prefix "awk-preview-"
  :group 'tools)

(defcustom awk-preview-program
  (or (executable-find "gawk")
      (executable-find "awk")
      "awk")
  "AWK executable path or name."
  :type 'string
  :group 'awk-preview)

(defcustom awk-preview-switches
  ;; '("--sandbox")
  nil
  "String of AWK options appended when running awk-preview."
  :type '(repeat string)
  :group 'awk-preview)

(defcustom awk-preview-default-program
  "# C-c C-l: Update preview      C-c C-c: Commit and exit
# C-c C-r: Resest to original  C-c C-k: Abort
{
    print NR, $0
}
"
  "Default awk command."
  :type 'string
  :group 'awk-preview)

(defcustom awk-preview-kill-orphan-program-buffer
  'ask
  "Decides if orphan program buffer should be killed.

This variable defines whether program buffers which do not visit any
files will be killed when exiting awk-preview sessions.

When set to nil, do not kill program buffers.
When set to `ask', ask user whether to kill them.
For other values always kill them silently.

When a program buffer is visiting a file, the buffer will not be
killed for any cases regardless of this variable."
  :type '(choice (const t)
                 (const nil)
                 (const ask))
  :group 'awk-preview)

(cl-defstruct awk-preview--env
  ;; Whether awk-preview is currently running
  (running-p nil)
  ;; Point of beg in source buffer
  (point-beg nil)
  ;; Point of end in source buffer
  (point-end nil)
  ;; Point of beginning
  ;; Used by preview buffer and always same as point-beg
  (preview-point-beg nil)
  ;; Point of beginning
  ;; Used by preview buffer and may defferent from point-end
  (preview-point-end nil)
  ;; Awk preview program temporary file name
  (program-filename nil)
  ;; Source buffer
  (source-buffer nil)
  ;; Preview buffer
  (preview-buffer nil)
  ;; Program buffer
  (program-buffer nil)
  ;; Window configuration when entering awk-review
  (window-configuration nil)
  )

(defvar-local awk-preview--env nil
  "`awk-preview--env' struct object of currently running.")
(put 'awk-preview--env
     'permanent-local
     t)

(defun awk-preview--invoke-awk (buf beg end progfile output &optional delete display)
  "Invoke AWK process.

Input is given from BUF buffer with BEG and END.
Use PROGFILE as awk program file (-f option).

Output will be written to OUTPUT buffer.
When OUTPUT is t the result will be output to BUF.

Delete the text between BEG and END when DELETE is non-nil.

DISPLAY non-nil means redisplay buffer as output is inserted."
  (with-current-buffer buf
    (let ((status (apply 'call-process-region
                         beg
                         end
                         awk-preview-program
                         delete
                         output
                         display
                         `(,@awk-preview-switches "-f" ,progfile))))
      (unless (eq status
                  0)
        (error "AWK-PREVIEW: Awk program exited abnormally"))
      output)))

(defvar awk-preview-program-buffer-name
  "*AWK Preview Program<%s>*"
  "Buffer name of awk preview program.")

(defvar awk-preview-preview-buffer-name
  "*AWK Preview<%s>*"
  "Buffer name of awk preview.")

(defun awk-preview--create-program-buffer (e)
  "Create new program buffer for `awk-preview--env' object E."
  (let ((source-name (buffer-name (awk-preview--env-source-buffer e))))
    (with-current-buffer (generate-new-buffer (format awk-preview-program-buffer-name
                                                      source-name))
      (erase-buffer)
      (insert awk-preview-default-program)
      (awk-mode)
      (current-buffer))))

(defun awk-preview--setup-program-buffer (e buf)
  "Setup awk-preview program buffer for E with BUF."
  (with-current-buffer buf
    (awk-preview-program-mode 1)
    (setq awk-preview--env e))
  (setf (awk-preview--env-program-buffer e) buf)
  (setf (awk-preview--env-program-filename e) (make-temp-file "awk-preview-"
                                                              nil
                                                              ".awk"))
  buf)

(defun awk-preview--create-setup-preview-buffer (e)
  "Create and setup preview buffer for `awk-preview--env' object E."
  (with-current-buffer (awk-preview--env-source-buffer e)
    (let ((beg (awk-preview--env-point-beg e))
          (end (awk-preview--env-point-end e))
          (buf (let ((orig buffer-file-name))
                 (unwind-protect
                     (progn
                       (setq buffer-file-name nil)
                       (clone-buffer (format awk-preview-preview-buffer-name
                                             (buffer-name))))
                   (setq buffer-file-name orig)))))
      (with-current-buffer buf
        (awk-preview-program-mode 1)
        (goto-char end)
        (setq buffer-read-only t)
        (setq awk-preview--env e))
      (setf (awk-preview--env-preview-point-beg e) beg)
      (setf (awk-preview--env-preview-point-end e) end)
      (setf (awk-preview--env-preview-buffer e) buf)
      buf)))

(defun awk-preview (beg end &optional program-buffer)
  "Run AWK and preview result.

BEG and END should be points of region to filter with awk.
If called interactively without region, whole contents will be
passwd to awk process.

PROGRAM-BUFFER, if given, should be a awk buffer and it will be used
to filter input."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max))))
  (when (and awk-preview--env
             (awk-preview--env-running-p awk-preview--env))
    (error "AWK-Preview already running"))
  (let ((e (make-awk-preview--env)))
    (setf (awk-preview--env-point-beg e) beg)
    (setf (awk-preview--env-point-end e) end)

    (setf (awk-preview--env-source-buffer e) (current-buffer))

    (awk-preview--create-setup-preview-buffer e)
    (awk-preview--setup-program-buffer e
                                       (or program-buffer
                                           (awk-preview--create-program-buffer e)))

    (setf (awk-preview--env-window-configuration e)
          (current-window-configuration))

    (cl-assert (awk-preview--env-point-beg e))
    (cl-assert (awk-preview--env-point-end e))
    (cl-assert (awk-preview--env-preview-point-beg e))
    (cl-assert (awk-preview--env-preview-point-end e))
    (cl-assert (awk-preview--env-program-filename e))
    (cl-assert (awk-preview--env-source-buffer e))
    (cl-assert (awk-preview--env-preview-buffer e))
    (cl-assert (awk-preview--env-program-buffer e))
    (cl-assert (awk-preview--env-window-configuration e))
    (setq awk-preview--env e)

    (set-window-buffer (get-buffer-window (awk-preview--env-source-buffer e))
                       (awk-preview--env-preview-buffer e))
    (pop-to-buffer (awk-preview--env-program-buffer e))

    (switch-to-buffer (awk-preview--env-program-buffer e))
    (setf (awk-preview--env-running-p e) t)
    (awk-preview-update-preview)
    ))

(defun awk-preview-update-preview ()
  "Update awk-preview."
  (interactive)
  (cl-assert awk-preview--env)
  (with-current-buffer (awk-preview--env-program-buffer awk-preview--env)
    (write-region (point-min)
                  (point-max)
                  (awk-preview--env-program-filename awk-preview--env)))
  (let ((output (with-current-buffer (get-buffer-create " *awk-preview output*")
                  (erase-buffer)
                  (current-buffer)))
        (progfile (awk-preview--env-program-filename awk-preview--env)))
    (awk-preview--invoke-awk (awk-preview--env-source-buffer awk-preview--env)
                             (awk-preview--env-point-beg awk-preview--env)
                             (awk-preview--env-point-end awk-preview--env)
                             progfile
                             output)
    (with-current-buffer (awk-preview--env-preview-buffer awk-preview--env)
      (let ((inhibit-read-only t))
        (goto-char (awk-preview--env-preview-point-end awk-preview--env))
        (delete-region (awk-preview--env-preview-point-beg awk-preview--env)
                       (point))
        (insert-buffer-substring output)
        (setf (awk-preview--env-preview-point-end awk-preview--env)
              (point)))
      )))

(defun awk-preview-reset ()
  "Show original input in preview buffer."
  (interactive)
  (cl-assert awk-preview--env)
  (with-current-buffer (awk-preview--env-preview-buffer awk-preview--env)
    (let ((inhibit-read-only t))
      (goto-char (awk-preview--env-preview-point-end awk-preview--env))
      (delete-region (awk-preview--env-preview-point-beg awk-preview--env)
                     (point))
      (insert-buffer-substring (awk-preview--env-source-buffer awk-preview--env)
                               (awk-preview--env-point-beg awk-preview--env)
                               (awk-preview--env-point-end awk-preview--env))
      (setf (awk-preview--env-preview-point-end awk-preview--env)
            (point)))))

(defun awk-preview-commit ()
  "Exit awk-preview and update buffer."
  (interactive)
  (cl-assert awk-preview--env)
  (awk-preview--invoke-awk (awk-preview--env-source-buffer awk-preview--env)
                           (awk-preview--env-point-beg awk-preview--env)
                           (awk-preview--env-point-end awk-preview--env)
                           (awk-preview--env-program-filename awk-preview--env)
                           t
                           t
                           t)
  (awk-preview--cleanup))

(defun awk-preview-abort ()
  "Discard result and exit awk-preview."
  (interactive)
  (awk-preview--cleanup))

(defun awk-preview--cleanup()
  "Cleanup awk-preview buffers variables and files."
  (cl-assert awk-preview--env)
  (with-current-buffer (awk-preview--env-source-buffer awk-preview--env)
    (kill-buffer (awk-preview--env-preview-buffer awk-preview--env))
    (when (and (not (buffer-file-name (awk-preview--env-program-buffer awk-preview--env)))
               (if (eq 'ask
                       awk-preview-kill-orphan-program-buffer)
                   (yes-or-no-p "Program buffer does not visit any file. Kill? ")
                 awk-preview-kill-orphan-program-buffer))
      (kill-buffer (awk-preview--env-program-buffer awk-preview--env)))
    (delete-file (awk-preview--env-program-filename awk-preview--env))
    (setf (awk-preview--env-running-p awk-preview--env) nil))
  (set-window-configuration (awk-preview--env-window-configuration awk-preview--env)))

(defvar awk-preview-program-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'awk-preview-update-preview)
    (define-key map (kbd "C-c C-k") 'awk-preview-abort)
    (define-key map (kbd "C-c C-c") 'awk-preview-commit)
    (define-key map (kbd "C-c C-r") 'awk-preview-reset)
    map)
  "Keymap for `awk-preview-program-mode'.")

(define-minor-mode awk-preview-program-mode
  "Minor mode for awk-preview program buffer."
  :lighter " AWKPrev"
  :keymap awk-preview-program-mode-map)


(provide 'awk-preview)
;;; awk-preview.el ends here
