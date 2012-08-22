;; http://idita.blog11.fc2.com/blog-entry-810.html

(defvar pasteboard-paste-program (executable-find "pbpaste")
  "Program to get text from osx pasteboard.")
(defvar pasteboard-copy-program (executable-find "pbcopy")
  "Program to put text to osx pasteboard.")

(defun pasteboard-paste ()
  (shell-command-to-string pasteboard-paste-program))
(defun pasteboard-cut (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process pasteboard-copy-program
                               "*Messages*"
                               pasteboard-copy-program)))
      (process-send-string proc text)
      (process-send-eof proc))))

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

(provide 'pasteboard)
