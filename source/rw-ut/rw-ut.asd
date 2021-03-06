
(defpackage :rw-ut-system
  (:use :cl))

(in-package :rw-ut-system)

(asdf:defsystem :rw-ut
  :description 
"R-UT and W-UT functions for reading and writing lisp universal time as strings"
  :author "Nick Allen <nallen05@gmail.com>"
  :components
  ((:file "rw-ut")))