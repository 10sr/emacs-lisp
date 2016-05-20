;; original is available at http://www.bookshelf.jp/soft/meadow_30.html#SEC419

(defvar save-window-size-filename (concat user-emacs-directory
                                          "framesize.el"))

(defvar window-size-apply-delay 1
  "If non-nil, time in seconds before applying window size settings after init")

(defun window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file save-window-size-filename)
         (recentf-exclude '(".+")))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; 初期値をいじるよりも modify-frame-parameters
             ;; で変えるだけの方がいい?
             "(delete 'width default-frame-alist)\n"
             "(delete 'height default-frame-alist)\n"
             "(delete 'top default-frame-alist)\n"
             "(delete 'left default-frame-alist)\n"
             "(setq default-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "default-frame-alist))\n"
             ;;"(setq default-frame-alist default-frame-alist)"
             ))
    (save-buffer)
    ))

(defun window-size-load ()
  (let* ((file save-window-size-filename))
    (if (file-exists-p file)
        (load file))))

(defun window-size-apply-with-delay ()
  (when window-size-apply-delay
    (run-with-timer window-size-apply-delay
                    nil
                    (lambda ()
                      (modify-frame-parameters (selected-frame)
                                               default-frame-alist)))))

(when window-system
  (window-size-load)
  (add-hook 'after-init-hook      ;何かがframeの大きさ勝手に変えやがる
            'window-size-apply-with-delay
            t)
  (add-hook 'kill-emacs-hook
            'window-size-save))

(provide 'save-window-size)
