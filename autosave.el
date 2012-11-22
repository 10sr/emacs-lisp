(defvar autosave-include-regexp
  ""
  "Regexp that matches filename to be saved.")

(defvar autosave-exclude-regexp
  "^$"
  "Regexp that matches filename not to be saved.")

(defun autosave-save-buffers ()
  "Save buffers. The variable `autosave-functions' decides if each buffer
should be saved or not."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (run-hook-with-args-until-failure 'autosave-functions)
              (save-buffer))))
        (buffer-list)))

(defvar autosave-functions nil
  "A list of functions be called before autosaving buffers.
Each function is called with no argument. Current buffer is set to the buffer
to save while these functions are called. If any of these functions return nil,
the buffer will not saved.")

(defvar autosave-timer nil "Autosave timer object.")

(defun autosave-enable (secs)
  "Register timer so that buffers will be saved automatically each time
when Emacs is idle for SECS. When SECS is 0 or nil, stop the timer and disable
autosaving. The variable `autosave-functions' decides if each buffer
should be saved automatically or not."
  (interactive "nSeconds until autosaving (0 to disable autosaving.): ")
  (if (and secs
           (not (eq secs
                    0)))
      (progn (autosave-disable)
             (setq autosave-timer
                   (run-with-idle-timer secs
                                        t
                                        'autosave-save-buffers))
             (message "Autosaving enabled (%d seconds)." secs))
    (autosave-disable)))

(defun autosave-disable ()
  "Disable autosaving buffers."
  (interactive)
  (when autosave-timer
    (cancel-timer autosave-timer)
    (setq autosave-timer nil)
    (message "Autosaving disabled."))
  nil)

(defun autosave-file-test-regexp ()
  "Return nil if the filename is not matched with `autosave-include-regexp'
or matched with `autosave-exclude-regexp'"
  (and buffer-file-name
       (string-match autosave-include-regexp
                     buffer-file-name)
       (not (string-match autosave-exclude-regexp
                          buffer-file-name))))

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
        autosave-buffer-file-writable-p
        autosave-file-test-regexp))

(provide 'autosave)

