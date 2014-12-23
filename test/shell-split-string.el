(require 'ert)
(require 'shell-split-string)

(ert-deftest test-shell-split-string ()
  (should (equal (shell-split-string "abc")
                 "abc"))
  )
