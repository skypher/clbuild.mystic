(in-package :contextl)

(defun ensure-layered-function
       (name
        &rest initargs
        &key (lambda-list () lambda-list-p)
        (argument-precedence-order (required-args lambda-list))
        (generic-function-class 'layered-function)
        &allow-other-keys)
  (declare (dynamic-extent initargs))
  (unless lambda-list-p
    (error "The layered function ~S must be initialized with a lambda list." name))
  (let ((gf (let ((layer-arg (gensym "LAYER-ARG-")))
              (apply #'ensure-generic-function
                     (lf-definer-name name)
		     #-allegro :generic-function-class
                     #-allegro generic-function-class
                     :argument-precedence-order
                     `(,@argument-precedence-order ,layer-arg)
                     :lambda-list
                     `(,layer-arg ,@lambda-list)
                     initargs))))
    (setf (fdefinition name)
          (compile nil `(lambda (&rest rest)
                          (declare (dynamic-extent rest)
                                   (optimize (speed 3) (debug 0) (safety 0)
                                             (compilation-speed 0)))
                          (apply (the function ,gf)
                                 (layer-context-prototype *active-context*)
                                 rest))))
    (bind-lf-names name)
    gf))

(defun ensure-layered-method
       (layered-function-designator
        lambda-expression
        &key
        #-(or allegro clisp cmu ecl clozure-common-lisp openmcl mcl) 
        (method-class nil method-class-p)
        (in-layer 't)
        (qualifiers ())
        (lambda-list (cadr lambda-expression))
        (specializers (required-args lambda-list (constantly (find-class 't)))))
  (let ((layered-function (if (functionp layered-function-designator)
                            layered-function-designator
                            (fdefinition (lf-definer-name layered-function-designator))))
        (layer-arg (gensym "LAYER-ARG-")))
    #-(or allegro clisp cmu ecl clozure-common-lisp openmcl mcl)
    (unless method-class-p
      (setq method-class (generic-function-method-class layered-function)))
    (destructuring-bind
        (lambda (&rest args) &body body)
        lambda-expression
      (unless (eq lambda 'lambda)
        (error "Incorrect lambda expression: ~S." lambda-expression))
      (ensure-method layered-function
                     `(lambda (,layer-arg ,@args) ,@body)
                     #-(or allegro clisp cmu ecl clozure-common-lisp openmcl mcl) :method-class
                     #-(or allegro clisp cmu ecl clozure-common-lisp openmcl mcl) method-class
                     :qualifiers qualifiers
                     :lambda-list `(,layer-arg ,@lambda-list)
                     :specializers (cons (find-layer-class in-layer) specializers)))))

(defgeneric layered-method-layer (method)
  (:method ((method layered-method)) (find-layer (first (method-specializers method)))))

(defmethod print-object ((object layered-method) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A ~S ~A"
            (when (method-generic-function object)
              (lf-caller-name
               (generic-function-name
                (method-generic-function object))))
            (layer-name (layered-method-layer object))
            (method-qualifiers object)
            (layered-method-specializers object))))
