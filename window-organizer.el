;; forked from http://d.hatena.ne.jp/khiker/20100119/window_resize

(defun window-organizer ()
  "Control window size and position."
  (interactive)
  (save-selected-window
    (select-window (window-at 0 0))
    (let ( ;; (window-obj (selected-window))
          ;; (current-width (window-width))
          ;; (current-height (window-height))
          action
          c)
      (catch 'end-flag
        (while t
          (setq action
                (read-key-sequence-vector
                 (format "size[%dx%d] 1: maximize; 2, 3: split; 0: \
delete; o: select other; j, l: enlarge; h, k: shrink; q: quit."
                         (window-width)
                         (window-height))))
          (setq c (aref action 0))
          (cond ((= c ?l)
                 (unless (eq (window-width) (frame-width))
                   (enlarge-window-horizontally 1)))
                ((= c ?h)
                 (unless (eq (window-width) (frame-width))
                   (shrink-window-horizontally 1)))
                ((= c ?j)
                 (enlarge-window 1))
                ((= c ?k)
                 (shrink-window 1))
                ((= c ?o)
                 (other-window 1))
                ((memq c '(?d ?0))
                 (unless (eq (selected-window)
                             (next-window (selected-window) 0 1))
                   (delete-window (selected-window))))
                ((= c ?1)
                 (delete-other-windows))
                ((= c ?2)
                 (split-window-vertically))
                ((= c ?3)
                 (split-window-horizontally))
                ((memq c '(?q ?\C-g))
                 (message "Quit")
                 (throw 'end-flag t))
                (t
                 (beep))))))))
;; (aref (read-key-sequence-vector "aa") 0)

(provide 'window-organizer)
