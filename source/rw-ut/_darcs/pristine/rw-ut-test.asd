
(defpackage :rw-ut-test-system
  (:use :cl))

(in-package :rw-ut-test-system)

(asdf:defsystem :rw-ut-test
  :description "tests for `rw-ut'"
  :depends-on (:rw-ut :ptester)
  :components
  ((:file "rw-ut")))