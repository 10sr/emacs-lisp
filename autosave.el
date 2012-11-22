(defun autosave-buffer-save-current-buffer ()
  "Save current buffer. The variable `autosave-buffer-functions' decides if
current buffer should be saved or not."
  (when (run-hook-with-args-until-failure 'autosave-buffer-functions)
    (save-buffer)))

(defvar autosave-buffer-functions nil
  "A list of functions be called before autosaving current buffer.
Each function is called with no argument. Current buffer is set to the buffer
to save while these functions are called. If any of these functions return nil,
autosaving will not happen.")

(defvar autosave-buffer nil "Autosave timer object.")

(defun autosave-buffer (secs)
  "Register timer so that current buffer will be saved automatically each time
when Emacs is idle for SECS. When SECS is 0 or nil, stop the timer and disable
autosaving. The variable `autosave-buffer-functions' decides if current buffer
should be saved automatically or not."
  (interactive "nSeconds until autosaving (0 to disable autosaving.): ")
  (if (and secs
           (not (eq secs
                    0)))
      (progn (when autosave-buffer
               (cancel-timer autosave-buffer)
               (setq autosave-buffer nil))
             (setq autosave-buffer
                   (run-with-idle-timer secs
                                        t
                                        'autosave-buffer-save-current-buffer)))
    (when autosave-buffer
      (cancel-timer autosave-buffer)
      (setq autosave-buffer nil))))

(defun autosave-buffer-buffer-file-name ()
  "Return nil if current buffer is not visiting any file."
  buffer-file-name)

(defun autosave-buffer-file-exists-p ()
  "Return nil if the file current buffer is visiting is not exist."
  (file-exists-p buffer-file-name))

(defun autosave-buffer-buffer-writable-p ()
  "Return nil if current buffer is read only."
  (not buffer-read-only))

(defun autosave-buffer-buffer-modified-p ()
  "Return nil if current buffer is not modified yet since last save."
  (buffer-modified-p))

(defun autosave-buffer-buffer-file-writable-p ()
  "Return nil if the file current buffer is visiting is not writable."
  (file-writable-p buffer-file-name))

(mapc (lambda (f)
          (add-hook 'autosave-buffer-functions
                    f))
        '(autosave-buffer-buffer-file-name
          autosave-buffer-file-exists-p
          autosave-buffer-buffer-writable-p
          autosave-buffer-buffer-modified-p
          autosave-buffer-buffer-file-writable-p))
