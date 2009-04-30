
(defpackage :rw-ut-test
  (:use :cl :rw-ut :ptester)
  (:export :!run-tests))
  
(in-package :rw-ut-test)

(defun !run-tests ()
  (warn "I haven't had time to write any tests for `rw-ut' yet"))

(!run-tests)