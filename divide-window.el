(defvar divide-window-default-num
  3
  "Default number of windows `divide-window-vertically' and
`divide-window-horizontally' make.")

(defun divide-window-vertically (arg &optional window)
  "Divide window equally vertically."
  (interactive "p")
  (let ((n (if (eq arg 1)
               (or divide-window-default-num
                   3)
             arg))
        (height (window-height window)))
    (divide-window--internal window
                             n
                             (/ height n)
                             nil)))

(defun divide-window-horizontally (arg &optional window)
  "Divide window equally horizontally."
  (interactive "p")
  (let ((n (if (eq arg 1)
               (or divide-window-default-num
                   3)
             arg))
        (width (window-width window)))
    (divide-window--internal window
                             n
                             (/ width n)
                             t)))

(defun divide-window--internal (window num size side)
  "Used for internal"
  (unless (eq num 1)
    (let ((nw (split-window window
                            size
                            side)))
      (divide-window--internal nw
                               (- num 1)
                               size
                               side))))

(provide 'divide-window)
