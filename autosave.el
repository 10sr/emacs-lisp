(defun autosave-save-current-buffer ()
  "Save current buffer. The variable `autosave-functions' decides if
current buffer should be saved or not."
  (when (run-hook-with-args-until-failure 'autosave-functions)
    (save-buffer)))

(defvar autosave-functions nil
  "A list of functions be called before autosaving current buffer.
Each function is called with no argument. Current buffer is set to the buffer
to save while these functions are called. If any of these functions return nil,
autosaving will not happen.")

(defvar autosave-timer nil "Autosave timer object.")

(defun autosave-enable (secs)
  "Register timer so that current buffer will be saved automatically each time
when Emacs is idle for SECS. When SECS is 0 or nil, stop the timer and disable
autosaving. The variable `autosave-functions' decides if current buffer
should be saved automatically or not."
  (interactive "nSeconds until autosaving (0 to disable autosaving.): ")
  (if (and secs
           (not (eq secs
                    0)))
      (progn (autosave-disable)
             (setq autosave-timer
                   (run-with-idle-timer secs
                                        t
                                        'autosave-save-current-buffer))
             (message "Autosaving enabled (%d seconds)." secs))
    (autosave-disable)))

(defun autosave-disable ()
  "Disable autosaving current buffer."
  (interactive)
  (when autosave-timer
    (cancel-timer autosave-timer)
    (setq autosave-timer nil)
    (message "Autosaving disabled."))
  nil)

(defun autosave-buffer-file-name ()
  "Return nil if current buffer is not visiting any file."
  buffer-file-name)

(defun autosave-file-exists-p ()
  "Return nil if the file current buffer is visiting is not exist."
  (file-exists-p buffer-file-name))

(defun autosave-buffer-writable-p ()
  "Return nil if current buffer is read only."
  (not buffer-read-only))

(defun autosave-buffer-modified-p ()
  "Return nil if current buffer is not modified yet since last save."
  (buffer-modified-p))

(defun autosave-buffer-file-writable-p ()
  "Return nil if the file current buffer is visiting is not writable."
  (file-writable-p buffer-file-name))

(mapc (lambda (f)
          (add-hook 'autosave-functions
                    f))
        '(autosave-buffer-file-name
          autosave-file-exists-p
          autosave-buffer-writable-p
          autosave-buffer-modified-p
          autosave-buffer-file-writable-p))

(provide 'autosave)
