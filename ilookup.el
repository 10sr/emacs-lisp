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
This value should be a key of `ilookup-alist'.")

(defvar ilookup-alist
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
  "Point of beginning of current prompt.")
(make-variable-buffer-local 'ilookup--current-prompt-point)

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
  (eq (line-number-at-pos)
      (line-number-at-pos ilookup--current-prompt-point)))

(defun ilookup--timer-add ()
  "Entry idle timer for ilookup."
  (and (eq ilookup--buffer
           (current-buffer))
       ;; do not duplicate timer
       (not ilookup--timer)
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
       (cancel-timer ilookup--timer)))

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
  "Get entry for current ilookup input."
  (let ((input (ilookup--get-input))
        (outpoint (ilookup--get-output-start)))
    (and input
         ;; do not query same word twice
         (not (equal ilookup--last-input
                     input))
         outpoint
         (let* (
                ;; colon sepatated list of input
                (inputl (split-string input
                                      ":"))
                ;; funcname for `ilookup-alist'
                (fname (if (eq (length inputl)
                               2)
                           (car inputl)
                         ilookup-default))
                (func (cdr (assoc fname
                                  ilookup-alist)))
                (word (if (eq (length inputl)
                              2)
                          (nth 1
                               inputl)
                        (car inputl))))
           (save-excursion
             (setq ilookup--last-input input)
             (goto-char outpoint)
             (delete-region (point)
                            (point-max))
             (insert (if func
                         (funcall func
                                  word)
                       (format "No func found for `%s'"
                               fname))))))))

(defun ilookup--get-result (word fname)
  "Return result string for WORD with FNAME."
  nil)

(defun ilookup--get-input ()
  "Get current input for ilookup buffer.
Return nil if current position is not on prompt line."
  (and (eq ilookup--buffer
           (current-buffer))
       (ilookup--on-prompt-p)
       (buffer-substring-no-properties (ilookup-bol)
                                       (point-at-eol))))

(defun ilookup--get-buffer-create ()
  "Create ilookup buffer if not exists yet and return that buffer."
  ;; TODO: implement me!
  nil)

(defun ilookup--parse-input (input)
  "Parse input.
INPUT must be \"word\" or \"dict:word\"."
  ;; TODO: implement me!
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major-mode and interactive command for that

(defvar ilookup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ilookup-enter)
    (define-key map (kbd "C-u") 'ilookup-kill-input)
    (define-key map (kbd "C-a") 'ilookup-goto-bol)
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
  (let ((pword (and
                ;; do not get if currently on prompt
                (not (ilookup--on-prompt-p))
                (thing-at-point 'word))))
    ;; print result is done only when currently on prompt
    (ilookup--print-result-from-input)
    (goto-char (point-max))
    (unless (eq (point)
                (point-at-bol))
      (newline))
    (setq ilookup--current-prompt-point
          (point))
    (insert ilookup-prompt)
    (and pword
         (insert pword))))

(defun ilookup-kill-input ()
  "Delete `ilookup-bol' to current point."
  (interactive)
  (delete-region (ilookup-bol)
                 (point)))

(defun ilookup-goto-bol ()
  "Go to ilookup bol."
  (interactive)
  (goto-char (ilookup-bol)))


(defun ilookup-bol ()
  "Return point to bol ignoring prompt."
  (save-excursion
    (beginning-of-line)
    (or (search-forward ilookup-prompt
                        (point-at-eol)
                        t)
        (point))))

(defun ilookup-open ()
  "Open ilookup buffer."
  (interactive)
  (if ilookup--buffer
      (pop-to-buffer ilookup--buffer)
    (with-current-buffer (setq ilookup--buffer
                               (get-buffer-create "*ilookup*"))
      (ilookup-mode)
      (font-lock-mode t)
      (ilookup-enter)
      (ilookup--timer-add)
      (add-hook 'kill-buffer-hook
                'ilookup--timer-remove)
      )
    (pop-to-buffer ilookup--buffer)))

(defun ilookup-open-word (word &optional dict)
  "Open ilookup buffer with WORD input.
Optional argument DICT specified dict name defined in `ilookup-alist'."
  ;; TODO: implement me!
  nil)

(defun ilookup-open-at-point ()
  "Open ilookup buffer with word at point input."
  ;; TODO: impelment me!
  )

(provide 'ilookup)

;;; ilookup.el ends here
