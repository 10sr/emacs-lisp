;; original is available at http://www.bookshelf.jp/soft/meadow_30.html#SEC419

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
         (file "~/.emacs.d/framesize.el")
         (recentf-exclude '("\\.emacs\\.d/framesize\\.el$")))
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
  (let* ((file "~/.emacs.d/framesize.el"))
    (if (file-exists-p file)
        (load file))))

(when window-system
  (window-size-load)
  (add-hook 'after-init-hook      ;何かがframeの大きさ勝手に変えやがる
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (modify-frame-parameters (selected-frame)
                                                         default-frame-alist))))
            t)
  (add-hook 'kill-emacs-hook
            'window-size-save))
