;;; autosave.el --- Save buffers automatically

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/autosave.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility

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

;; Save buffers automatically when emacs is idle for spedified seconds.

;; Idle timer for autosave can be set by sexps like:
;; (and (require 'autosave nil t)
;;      (autosave-set 2))

;;; Code:

(defun autosave-save-buffers (include exclude function)
  "Check all buffers and save the buffer if all of these are satisfied :

* the filename of the buffer matches with INCLUDE
* the filename of the buffer does not match with EXCLUDE
* FUNCTION return non-nil

INCLUDE and EXCLUDE should be regexp. FUNCTION is called with no argument and
the buffer to save being set as current buffer."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (and (autosave-file-test-regexp include exclude)
                 (funcall function)
                 (save-buffer))))
        (buffer-list)))

(defun autosave-file-test-regexp (include exclude)
  "Return non-nil if the filename currently visiting is matched with INCLUDE
and is not matched with EXCLUDE."
  (and buffer-file-name
       (string-match include
                     buffer-file-name)
       (not (string-match exclude
                          buffer-file-name))))

(defun autosave-test-default ()
  "Return non-nil if all of functions in `autosave-test-default-functions'
return non-nil, otherwise return nil.

This functions is used by `autosave-set' if the fourth arg of `autosave-set'
is set to be nil."
  (run-hook-with-args-until-failure 'autosave-test-default-functions))

(defvar autosave-test-default-functions nil
  "A list of functions called by `autosave-test-default'.
Each function is called with no argument. Current buffer is set to the buffer
to save while these functions are called.
You can use `add-hook' and `remove-hook' to update this list.")

(defvar autosave-timer-list nil
  "A list of autosave timer objects. When new timer is set by `autosave-set',
the timer object is added to the top of this list.")

(defun autosave-set (secs &optional include exclude function)
  "Register timer so that buffers will be saved automatically each time when
Emacs is idle for SECS. Autosave is done by calling `autosave-save-buffers'.

INCLUDE and EXCLUDE should be regexp to match with the filename of buffer to
include and exclude respectively. These args can be nil, in that case all
files are included or no files are excluded.

Fourth arg FUNCTION is function to test if the buffer should be saved. This
function is called with no argument and the buffer to save being set as
current buffer. If this function returns non-nil, the buffer is saved.
If this arg is nil, the function `autosave-test-default' is used by default.

If the timer with exactly same arg as this has already set, timer is not
created newly.

This function returns the created timer object, or nil if timer is not created.
This timer object is also added to `autosave-timer-list'. This timer can be
disabled by using `autosave-remove'."
  (interactive "nSeconds until autosaving: ")
  (let ((tm (run-with-idle-timer secs
                                 t
                                 'autosave-save-buffers
                                 (or include
                                     "")
                                 (or exclude
                                     "^$")
                                 (or function
                                     'autosave-test-default))))
    (if (member tm
                autosave-timer-list)
        (progn (cancel-timer tm)
               (message "Same timer already set (%S)."
                        tm)
               nil)
      (progn (setq autosave-timer-list
                   (cons tm
                         autosave-timer-list))
             (message "Autosave set (%S)."
                      tm)
             tm))))

(defun autosave-remove (timer)
  "Unset autosave timer object TIMER."
  (setq autosave-timer-list
        (delq timer
              autosave-timer-list))
  (cancel-timer timer))

(defun autosave-buffer-file-name ()
  "Return nil if current buffer is not visiting any file."
  buffer-file-name)

(defun autosave-file-exists-p ()
  "Return nil if the file current buffer is visiting is not exist."
  (and buffer-file-name
       (file-exists-p buffer-file-name)))

(defun autosave-buffer-writable-p ()
  "Return nil if current buffer is read only."
  (not buffer-read-only))

(defun autosave-buffer-modified-p ()
  "Return nil if current buffer is not modified yet since last save."
  (buffer-modified-p))

(defun autosave-buffer-file-writable-p ()
  "Return nil if the file current buffer is visiting is not writable."
  (and buffer-file-name
       (file-writable-p buffer-file-name)))

(mapc (lambda (f)
        (add-hook 'autosave-test-default-functions
                  f
                  t))                   ; append
      '(autosave-buffer-file-name
        autosave-file-exists-p
        autosave-buffer-writable-p
        autosave-buffer-modified-p
        autosave-buffer-file-writable-p))

(provide 'autosave)

;;; autosave.el ends here
