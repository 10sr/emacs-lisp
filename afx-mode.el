(defvar afx-mode-map
  (let ((map (make-keymap)))
    (define-key map "e" 'afx-find-file)
    map)
  "Keymap for `afx-mode'.")

(define-minor-mode afx-mode 
  "Minor mode for lovers of AFX windows filer."
  :init-value nil
  :lighter " AFX"
  :keymap nil
  nil)

(defun afx-mode-on ()
  "Turn on `afx-mode'."
  (afx-mode 1))

(defun afx-mode-off ()
  "Turn off `afx-mode'."
  (afx-mode 0))

(defun afx-always-enable-on-dired ()
  "Always enable `afx-mode' when viewing directory using `dired'."
  (interactive)
  (add-hook 'dired-mode-hook
            'afx-mode-on))

(defun afx (dirname &optional switches)
  "Open `dired' buffer with `afx-mode' enabled."
  (interactive (dired-read-dir-and-switches "with AFX keymap "))
  (switch-to-buffer (dired dirname switches))
  (afx-mode-on))

(defalias 'afx-find-file 'dired-find-file)

(provide 'afx-mode)
