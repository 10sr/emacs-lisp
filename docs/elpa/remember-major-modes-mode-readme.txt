`remember-major-modes-mode' is a global minor mode to remember major-modes
for files.

Usually major-modes for files are detected automatically using the value of
`auto-mode-alist' or shebang.  However sometimes these detection do not work
and yet updating `auto-mode-alist' is too match.
Interactive function `remember-major-modes-remember' remember current buffer
filename and major-mode so that the mode will be enabled next time the file
opened.

To use, add to your dot.emacs as below:
(when (require 'remember-major-modes-mode nil t)
   (remember-major-modes-mode 1))

You can use M-x remember-major-modes-memorize to remember the pair of current
file and major-mdoe and M-x remember-major-modes-forget to forget the
major-mode for current visiting file.
