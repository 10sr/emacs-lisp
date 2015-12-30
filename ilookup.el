;;; ilookup.el --- Incremental dictionary lookup

;; Author: 10sr <>
;; URL: https://github.com/10sr/emacs-lisp/blob/master/ilookup.el
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: utility

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Lookup dictionary in a incremental way.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables

(defvar ilookup-prompt ">>> "
  "Prompt string for ilookup input.")

(defvar ilookup-interval 0.5
  "Time in second to show result for current input.")

(defvar ilookup-default "sdcv"
  "Default command for ilookup.
This value should be a key of `ilookup-dict-alist'.")

(defvar ilookup-dict-alist
  '(
    ("sdcv" . (lambda (word)
               (shell-command-to-string (format "sdcv -n '%s'"
                                                word))))
    )
  "Alist of ilookup dictionary functions.
Each element should be in the form of (NAME . FUNCTION).
FUNCTION must accept one argument as word to search and return the string of
result for that word.")


(defvar ilookup--current-prompt-point nil
  "Point of beginning of current prompt.  Non-nil only in ilookup buffer.")
(make-variable-buffer-local 'ilookup--current-prompt-point)

;; do not use this var to get ilookup buffer: use `ilookup--get-buffer-create'
(defvar ilookup--buffer nil
  "Pointer to ilookup buffer.")

(defvar ilookup--timer nil
  "Idle timer object for ilookup.")

(defvar ilookup--last-input nil
  "Last input queried.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal utilities

(defsubst ilookup--on-prompt-p ()
  "Return true if currently on prompt line."
  (and ilookup--current-prompt-point
       (eq (line-number-at-pos)
           (line-number-at-pos ilookup--current-prompt-point))))

(defun ilookup--timer-add ()
  "Add idle timer for ilookup."
  ;; do not duplicate timer
  (unless ilookup--timer
    (setq ilookup--timer
          (run-with-idle-timer ilookup-interval
                               t
                               'ilookup--print-result-from-input))))

(defun ilookup--timer-remove ()
  "Remove idle timer for ilookup."
  ;; remote only when killing buffer is ilookup buffer
  (and (eq ilookup--buffer
           (current-buffer))
       ilookup--timer
       (progn (cancel-timer ilookup--timer)
              (setq ilookup--timer nil))))

(defun ilookup--parse-input (input)
  "Parse input.
INPUT must be \"word\" or \"dict:word\".
Return list of (word dict)"
  (let ((l (split-string input
                         ":")))
    (if (eq 2
            (length l))
        (list (nth 1
                   l)
              (car l))
      l)))

(defun ilookup--get-output-start ()
  "Return point where outputs should be inserted.
This function insert newline if required."
  (when ilookup--current-prompt-point
    (save-excursion
      (goto-char ilookup--current-prompt-point)
      (forward-line 1)
      (when (ilookup--on-prompt-p)
        (end-of-line)
        (newline))
      (point-at-bol))))

(defun ilookup--print-result-from-input ()
  "Get entry for current ilookup input and print it next to the prompt.
Any previous output will be removed."
  (let ((input (ilookup--get-input))
        (outpoint (ilookup--get-output-start)))
    (and input
         ;; do not query same word twice
         (not (equal ilookup--last-input
                     input))
         outpoint
         (let ((inputl (ilookup--parse-input input)))
           (save-excursion
             (setq ilookup--last-input input)
             (goto-char outpoint)
             (delete-region (point)
                            (point-max))
             (insert (or (apply 'ilookup--get-result
                                inputl)
                         (format "No func found for `%s'"
                                 (or (nth 1 inputl)
                                     ilookup-default)))
                     "\n")
             (let ((p (point)))
               (goto-char (point-max))
               (goto-char p))
             )))))

(defun ilookup--get-result (word &optional dict)
  "Return result string for WORD with DICT.
Return nil if no func found for DICT."
  (funcall (or (cdr (assoc (or dict
                               ilookup-default)
                           ilookup-dict-alist))
               'ignore)
           word))

(defun ilookup--input-given-p ()
  "Return non-nil if input is given."
  (let ((input (ilookup--get-input)))
    (and input
         (< 0
            (length input)))))

(defun ilookup--get-input ()
  "Get current input for ilookup buffer.
Return nil if current buffer is not ilookup buffer."
  (and ilookup--current-prompt-point
       (save-excursion
         (goto-char ilookup--current-prompt-point)
         (buffer-substring-no-properties (ilookup-bol)
                                         (point-at-eol)))))

(defun ilookup--get-buffer-create ()
  "Create ilookup buffer if not exists yet or killed and return that buffer."
  (if (and ilookup--buffer
           ;; return nil if this buffer has been killed
           (buffer-name ilookup--buffer))
      ilookup--buffer
    ;; create newly
    (with-current-buffer (setq ilookup--buffer
                               (get-buffer-create "*ilookup*"))
      (ilookup-mode)
      (font-lock-mode t)
      (setq ilookup--current-prompt-point
            (point))
      (insert ilookup-prompt)
      (ilookup--timer-add)
      (add-hook 'kill-buffer-hook
                'ilookup--timer-remove)
      (current-buffer))))

(defun ilookup--emit-next-prompt ()
  "Emit next prompt.
Print result for current input if given and not printed yet, and emit new
prompt.  Point is set to next to the prompt."
  (when (ilookup--on-prompt-p)
    (ilookup--print-result-from-input))
  (goto-char (point-max))
  (setq ilookup--current-prompt-point
        (point))
  (insert ilookup-prompt)
  (setq ilookup--last-input nil)
  (point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major-mode and interactive command for that

(defvar ilookup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ilookup-enter)
    (define-key map (kbd "C-u") 'ilookup-kill-input)
    (define-key map (kbd "C-a") 'ilookup-goto-bol)
    (define-key map (kbd "DEL") 'ilookup-delete-backward-char)
    map))

(define-derived-mode ilookup-mode fundamental-mode
  "iLookUp"
  "Major mode for incremental lookup buffer."
  (set (make-local-variable 'font-lock-function)
       'ignore))

(defun ilookup-enter ()
  "Enter function for `ilookup-mode'.
Freeze current input and show next prompt."
  (interactive)
  (if (ilookup--on-prompt-p)
      ;; if currently on prompt and input is given
      (ilookup--emit-next-prompt)
    (let ((pword (thing-at-point 'word)))
      (when (ilookup--input-given-p)
          ;; if input is already given
        (ilookup--emit-next-prompt))
      (ilookup-goto-prompt)
      (and pword
           (insert pword)))))

(defun ilookup-kill-input ()
  "Delete `ilookup-bol' to current point."
  (interactive)
  (delete-region (ilookup-bol)
                 (point)))

(defun ilookup-goto-bol ()
  "Go to ilookup bol."
  (interactive)
  (goto-char (ilookup-bol)))

(defun ilookup-goto-prompt ()
  "Goto end of prompt line."
  (interactive)
  (when ilookup--current-prompt-point
    (goto-char ilookup--current-prompt-point)
    (end-of-line)))

(defun ilookup-bol ()
  "Return point to bol ignoring prompt."
  (save-excursion
    (beginning-of-line)
    (or (search-forward ilookup-prompt
                        (point-at-eol)
                        t)
        (point))))

(defun ilookup-delete-backward-char (n)
  "Delete the previous N characters if it is not a prompt."
  (interactive "p")
  (when (< (ilookup-bol)
           (point))
    (backward-delete-char 1)
    (when (< 1 n)
      (ilookup-delete-backward-char (1- n)))))

;;;###autoload
(defun ilookup-open ()
  "Open ilookup buffer."
  (interactive)
  (pop-to-buffer (ilookup--get-buffer-create)))

;;;###autoload
(defun ilookup-open-word (word &optional dict)
  "Open ilookup buffer with WORD input.
Optional argument DICT specified dict name defined in `ilookup-dict-alist'."
  ;; TODO: implement me!
  (interactive "sIlookup Word: ")
  (with-current-buffer (pop-to-buffer (ilookup--get-buffer-create))
    (when (ilookup--input-given-p)
      (ilookup--emit-next-prompt))
    (ilookup-goto-prompt)
    (when dict
      (insert dict
              ":"))
    (insert word)))

;;;###autoload
(defun ilookup-open-at-point ()
  "Open ilookup buffer with word at point input."
  (interactive)
  (ilookup-open-word (substring-no-properties (thing-at-point 'word))))

(provide 'ilookup)

;;; ilookup.el ends here
