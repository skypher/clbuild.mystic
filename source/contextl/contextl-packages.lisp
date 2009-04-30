(in-package :cl-user)

(defpackage #:contextl
  (:use #:closer-common-lisp #:lispworks #:portable-threads #:trivial-garbage)
  (:export
   #:*symbol-access*
   #:active-layers
   #:adjoin-layer
   #:adjoin-layer-using-class
   #:apply-with-layer-context
   #:call-next-layered-method
   #:class-layer
   #:clear-layer-caches
   #:current-layer-context
   #:define-layered-class
   #:define-layered-function
   #:define-layered-method
   #:deflayer
   #:dletf #:dletf*
   #:ensure-active-layer
   #:ensure-inactive-layer
   #:ensure-layer
   #:ensure-layered-function
   #:ensure-layered-method
   #:find-layer
   #:find-layer-class
   #:find-singleton
   #:funcall-with-layer-context
   #:make-special-symbol
   #:layer-active-p
   #:layer-makunbound
   #:layer-name
   #:layered-access-class
   #:layered-class
   #:layered-direct-slot-definition
   #:layered-effective-slot-definition
   #:layered-effective-slot-definition-in-layers
   #:layered-function
   #:layered-function-argument-precedence-order
   #:layered-function-definer
   #:layered-function-lambda-list
   #:layered-method
   #:layered-method-lambda-list
   #:layered-method-layer
   #:layered-method-specializers
   #:lfmakunbound
   #:partial-class
   #:partial-class-defining-classes
   #:partial-class-defining-metaclass
   #:partial-object
   #:remove-layer
   #:remove-layer-using-class
   #:singleton-class
   #:slot-definition-layer
   #:slot-definition-layered-readers
   #:slot-definition-layered-writers
   #:slot-definition-layeredp
   #:slot-definition-layers
   #:slot-definition-specialp
   #:slot-boundp-using-layer
   #:slot-makunbound-using-layer
   #:slot-value-using-layer
   #:safe-special-symbol-progv
   #:special-class
   #:special-direct-slot-definition
   #:special-effective-slot-definition
   #:special-effective-slot-definition-in-layers
   #:special-layered-access-class
   #:special-layered-direct-slot-definition
   #:special-layered-effective-slot-definition
   #:special-object
   #:special-symbol-p
   #:special-symbol-progv
   #:standard-class-in-layer
   #:standard-direct-slot-definition-in-layer
   #:standard-effective-slot-definition-in-layers
   #:standard-layer-class
   #:standard-layer-object
   #:with-active-layers
   #:with-active-layers*
   #:with-inactive-layers
   #:with-special-initargs
   #:with-special-initargs*
   #:with-symbol-access
   #:without-symbol-access))

(defpackage #:contextl-common-lisp
  (:nicknames #:cxcl)
  (:use #:closer-common-lisp #:contextl)
  #.`(:export
      ,@(loop for sym being the external-symbols of :closer-common-lisp
              collect sym)
      ,@(loop for sym being the external-symbols of :contextl
              collect sym)))

(defpackage #:contextl-user
  (:use #:contextl-common-lisp)
  (:nicknames #:cx-user))
