(in-package common-lisp-user)
(defpackage #:portable-threads-system
  (:use #:asdf #:cl))
(in-package #:portable-threads-system)

(defsystem portable-threads
  :author "The GBBopen Project <gbbopen@GBBopen.org>"
  :maintainer "Dan Corkill <corkill@GBBopen.org>"
  :licence "Part of the GBBopen Project (see LICENSE for license information)."
  :description "Portable Threads."
  :components ((:static-file "COPYING")
               (:static-file "LICENSE")
               
               (:file "portable-threads")))


