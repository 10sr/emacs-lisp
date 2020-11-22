;;; flycheck-black-check.el --- Black check  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 10sr

;; Author: 10sr<>
;; Keywords: languages, tools
;; Package-Version: 20190418.518
;; Package-Commit: 8f3b235f53fa4918b3c3a2aba68e9ae21385754c
;; Version: 0.0.1
;; URL: https://github.com/10sr/flycheck-black-check
;; Package-Requires: ((flycheck "0.0.1") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flycheck black check.
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-black-check-setup))


;;; Code:

(require 'flycheck)

(flycheck-define-checker python-black-check
  "A Python style checker."
  :command ("python3" "-m" "black"
            "--check"
            (config-file "--config" flycheck-black)
            source)
  :error-parser flycheck-black-check-parse
  ;; :error-patterns
  ;; (
  ;;  (error line-start "error: cannot format " (file-name) ": " (message) ": " line ":" column ": " (one-or-more any) line-end)
  ;;  (error line-start (message) " " (file-name) line-end)
  ;;  )
  :enabled (lambda ()
             (or (not (flycheck-python-needs-module-p 'python-black-check))
                 (flycheck-python-find-module 'python-black-check "black")))
  :verify (lambda (_) (flycheck-python-verify-module 'python-black-check "black"))
  :modes python-mode)

;; (flycheck-define-checker python-black-diff
;;   "A Python style checker."
;;   :command ("python3"
;;             "-m" "black"
;;             (config-file "--config" flycheck-black)
;;             "--diff" source)
;;   :error-parser my-flycheck-parse-unified-diff
;;   :enabled (lambda ()
;;              (or (not (flycheck-python-needs-module-p 'python-black))
;;                  (flycheck-python-find-module 'python-black "black")))
;;   :verify (lambda (_) (flycheck-python-verify-module 'python-black "black"))
;;   :modes python-mode)

(flycheck-def-config-file-var flycheck-black python-black-check "pyproject.toml"
  :safe #'stringp)


(defun flycheck-black-check-parse (output checker buffer)
  "Flycheck parser to check if reformat is required.
For argument OUTPUT, CHECKER, and BUFFER refer to doc of `flycheck-define-command-checker'."
  (let ((result nil))
    (with-temp-buffer
      (insert output)
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward "^would reformat .*$" nil t)
          (push (flycheck-error-new-at
                 (point-min)
                 nil
                 'error
                 ;;(format "Black: %s" (match-string 0))
                 "Black: would reformat"
                 :buffer buffer
                 :checker checker)
                result))
        (goto-char (point-min))
        (when (re-search-forward "^error: .*$" nil t)
          (push (flycheck-error-new-at
                 (point-min)
                 nil
                 'error
                 ;; Fix not to include absolute file path
                 (format "Black: %s" (match-string 0))
                 :buffer buffer
                 :checker checker)
                result))))
    result))

;; (defun my-flycheck-parse-unified-diff (output checker buffer)
;;   "Flycheck parser to parse diff output."
;;   (let ((source-line 0)
;;         (result ())
;;         (hunk "HUNK"))
;;     (with-temp-buffer
;;       (insert output)
;;       (goto-char (point-min))
;;       (while (not (eq (point) (point-max)))
;;         ;; FIXME: Do not stop when no result
;;         (while (not (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*$" (point-at-eol) t))
;;           (forward-line 1)
;;           (goto-char (point-at-bol)))
;;         (setq source-line
;;               (string-to-number (match-string 1)))
;;         ;; TODO: Add filename support
;;         (setq hunk
;;               (match-string 0))
;;         ;;(while (not (rearch-forward "^\\(-\\|\\+\\)")))
;;         )
;;       (add-to-list 'result
;;                    (flycheck-error-new-at
;;                     0
;;                     nil
;;                     'error
;;                     "MESSAGE"
;;                     :buffer buffer
;;                     :checker checker
;;                     :group hunk)))
;;     result))

;;;###autoload
(defun flycheck-black-check-setup ()
  "Setup Flycheck black-check."
  (interactive)
  (add-to-list 'flycheck-checkers
               'python-black-check))

(provide 'flycheck-black-check)

;;; flycheck-black-check.el ends here
