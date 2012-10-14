;; Usage :
;; (auto-save-current-buffer 2 t)

(defun save-current-buffer (silent-p)
  "save current buffer if can without asking"
  (let ((cm (if (current-message)
                (format "%s\n" (current-message))
              ""))
        (fun (symbol-function (if silent-p
                                  'ignore
                                'message))))
    (cond ((active-minibuffer-window) nil)
          ((not buffer-file-name)
           (funcall fun
                    "%ssaving... this buffer doesn't visit any file." cm)
           nil)
          ((not (file-exists-p buffer-file-name))
           (funcall fun
                    "%ssaving... file not exist. save manually first." cm)
           nil)
          (buffer-read-only
           (funcall fun
                    "%ssaving... this buffer is read-only." cm)
           nil)
          ((not (buffer-modified-p))
           (funcal fun
                   "%ssaving... not modified yet." cm)
           nil)
          ((not (file-writable-p buffer-file-name))
           (funcall fun
                    "%ssaving... you cannot change this file." cm)
           nil)
          (t (funcall fun "%ssaving..." cm)
             (save-buffer)
             (funcall fun "%ssaving... done." cm)))))
;; (if (and buffer-file-name
;;          (not buffer-read-only)
;;          (buffer-modified-p)
;;          (file-writable-p buffer-file-name))
;;     (save-buffer))) ; silent one

(defvar auto-save-current-buffer nil "auto save timer object")

(defun auto-save-current-buffer (sec &optional silent-p)
  "auto save current buffer if idle for SEC.
when SEC is nil, stop auto save if enabled."
  (if sec
      (progn (when auto-save-current-buffer
               (cancel-timer auto-save-current-buffer)
               (setq auto-save-current-buffer nil))
             (setq auto-save-current-buffer
                   (run-with-idle-timer sec t 'save-current-buffer silent-p)))
    (when auto-save-current-buffer
      (cancel-timer auto-save-current-buffer)
      (setq auto-save-current-buffer nil))))
