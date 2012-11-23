(defun autosave-save-buffers (include exclude function)
  "Check all buffers and save the buffer if :

* the filename of the buffer matches with INCLUDE
* the filename of the buffer does not match with EXCLUDE
* FUNCTION return non-nil with the buffer being set as current buffer

For details see `autosave-set' and `autosave-file-test-regexp'."
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

(defun autosave-test ()
  "Return non-nil if all of functions in `autosave-functions' return non-nil,
otherwise return nil."
  (run-hook-with-args-until-failure 'autosave-functions))

(defvar autosave-functions nil
  "A list of functions called by `autosave-test'.
Each function is called with no argument. Current buffer is set to the buffer
to save while these functions are called.")

(defvar autosave-timer-list nil
  "A list of autosave timer objects. When new timer is set by `autosave-set',
the timer object is added to the top of this list.")

(defun autosave-set (secs &optional include exclude function)
  "Register timer so that buffers will be saved automatically each time when
Emacs is idle for SECS.

INCLUDE and EXCLUDE should be regexp to match with the filename of buffer to
include and exclude respectively. These args can be nil, in that case all
files are included or no files are excluded.

Fourth arg FUNCTION is function to test if the buffer is saved. This function
is called with no argument and the buffer to save being set as current buffer.
If this arg is nil, the function `autosave-test' is used by default.

This returns the created timer object, and this timer object can be disabled
by using `autosave-remove'."
  (interactive "nSeconds until autosaving: ")
  (let ((tm (run-with-idle-timer secs
                                 t
                                 'autosave-save-buffers
                                 (or include
                                     "")
                                 (or exclude
                                     "^$")
                                 (or function
                                     'autosave-test))))
    (setq autosave-timer-list
          (cons tm
                autosave-timer-list))
    (message "Autosave set (%d seconds)." secs)
    tm))

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
        (add-hook 'autosave-functions
                  f
                  t))                   ; append
      '(autosave-buffer-file-name
        autosave-file-exists-p
        autosave-buffer-writable-p
        autosave-buffer-modified-p
        autosave-buffer-file-writable-p))

(provide 'autosave)
