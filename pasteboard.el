;; http://idita.blog11.fc2.com/blog-entry-810.html

(defvar pasteboard-paste-program (executable-find "pbpaste")
  "Program to get text from osx pasteboard.")
(defvar pasteboard-copy-program (executable-find "pbcopy")
  "Program to put text to osx pasteboard.")
(defvar pasteboard-rtun-program (executable-find "reattach-to-user-namespace")
  "Program reattach-to-user-namespace. It is needed when you use pasteboard
within tmux. For details see
https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard")

(defvar pasteboard-paste-command pasteboard-paste-program
  "Command run to get text.")
(defvar pasteboard-copy-command pasteboard-copy-program
  "Command run to put text.")

(defun turn-on-pasteboard ()
  "Enable pasteboard for yank and paste."
  (interactive)
  (setq interprogram-paste-function 'pasteboard-paste)
  (setq interprogram-cut-function 'pasteboard-cut))

(defun turn-off-pasteboard ()
  "Disable pasteboard for yank and paste."
  (interactive)
  (setq interprogram-paste-function nil)
  (setq interprogram-cut-function nil))

(defun pasteboard-enable-rtun ()
  "Set to use reattach-to-user-namespace."
  (interactive)
  (if pasteboard-rtun-program
      (progn
        (setq pasteboard-paste-command
              (concat pasteboard-rtun-program
                      " "
                      pasteboard-paste-program))
        (setq pasteboard-copy-command
              (concat pasteboard-rtun-program
                      " "
                      pasteboard-copy-program)))
    (message
     "Cannot find reattach-to-user-namespace. First you must install it!")))

(defun pasteboard-disable-rtun ()
  "Set not to use reattach-to-user-namespace."
  (interactive)
  (setq pasteboard-paste-command
        pasteboard-paste-program)
  (setq pasteboard-copy-command
        pasteboard-copy-program))

(defun pasteboard-paste ()
  (shell-command-to-string pasteboard-paste-command))

(defun pasteboard-cut (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process-shell-command pasteboard-copy-program
                                             "*Messages*"
                                             pasteboard-copy-command)))
      (process-send-string proc text)
      (process-send-eof proc))))

(provide 'pasteboard)
