;; todo: 書く関数と制御を分ける つまり抽象化

(defun clock ()
  "make clock buffer and view clock"
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*clock*"))
    (clock--format-minute (current-time))
    (clock--initialize-sec-timer)
    (setq buffer-read-only t))
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer "*clock*")
    (read-event "tick tack tick tack..."))
  (kill-buffer "*clock*")
  (message "tick tack tick tack...it is %s now!" (clock--get-time)))

(defun clock--initialize-minute (time))

(defun clock--format-minute (time)
  "erase buffer and make minute table.
do nothing about sec."
  (let ((buffer-read-only nil)
        (h (nth 2 (decode-time time)))
        (m (nth 1 (decode-time time)))
        (i 0))
    (erase-buffer)
    (newline)
    (insert "    ")
    (setq i 1)
    (while (not (eq i 61))
      (insert (format "%2s" i))
      (setq i (1+ i)))
    (setq i 0)
    (newline)
    (while (not (eq i h))
      ;(goto-line (+ i 3))
      (insert " " (format "%2s" i) " ")
      (clock--fill-minute 60)
      (newline)
      (setq i (1+ i)))
    (while (not (eq i 24))
      (insert " " (format "%2s" i) " \n")
      (setq i (1+ i)))
    (newline 3)
    (clock--fill-minute-append-time time)))

(defun clock--fill-minute-append-time (time)
  "call `clock--fill-minute' and insert time at eol.
called by `clock--format-minute' and `clock--count-sec'"
  (let ((buffer-read-only nil)
        (h (nth 2 (decode-time time)))
        (m (nth 1 (decode-time time))))
    (goto-line (+ h 3))
    (goto-char (point-at-bol))
    (forward-char 4)
    (delete-region (point) (point-at-eol))
    (clock--fill-minute m)
    (insert " " (clock--get-time time))))

(defun clock--get-time (&optional time)
  ""
  (format-time-string "%H:%M" (or time (current-time))))

(defun clock--fill-minute (m)
  "fill current line by m with `clock-count-string'"
  (let ((i 0))
    (goto-char (point-at-eol))
    (while (not (eq i m))
      (insert " " clock-count-string)
      (setq i (1+ i)))))

(defvar clock--timer nil)
(defvar clock--sec nil)

(defun clock--initialize-sec-timer ()
  "format sec and initialize `clock--count-sec' timer"
  (let ((sec (nth 0 (decode-time (current-time))))
        (i 0))
    (if clock--timer
        (cancel-timer clock--timer))
    (clock--count-sec-insert t)
    (while (not (eq i sec))
      (clock--count-sec-insert)
      (setq i (1+ i)))
    (setq clock--sec sec))
  (setq clock--timer (run-at-time t 1 'clock--count-sec)))

(defun clock--count-sec ()
  "count sec, append `clock-count-string', and call `clock--fill-minute-append-time' if needed.
stop timer if clock buffer is not active."
  (let ((buffer-read-only nil))
    (if (not (eq (current-buffer) (get-buffer "*clock*")))
        (progn (cancel-timer clock--timer))
      (if (not (eq clock--sec 60))
          (clock--count-sec-insert)
        (let* ((time (current-time))
               (h (nth 2 (decode-time time)))
               (m (nth 1 (decode-time time)))) 
          (if (eq m 0)
              (clock--format-minute time)
            (clock--fill-minute-append-time time))
          (clock--initialize-sec-timer))))))

(defun clock--count-sec-insert (&optional INITIALIZE LINE)
  ""
  (let ((buffer-read-only nil))
    (goto-line (or LINE 28))
    (cond
     (INITIALIZE (delete-region (point-at-bol) (point-at-eol))
                 (insert "sec:")) 
     (t (goto-char (point-at-eol))
        (setq clock--sec (1+ clock--sec))
        (insert " " clock-count-string)))))

(defvar clock-count-string "@" "string to count minute and second.")


;; 勉強中
;; でもこれにモードいらないよね
(defun clock-mode ()
  ""
  (setq major-mode 'clock-mode)
  (setq clock-mode-map (make-keymap))
  (run-mode-hooks 'clock-mode-hook))
(defvar clock-mode-hook nil)

(provide 'clock)
