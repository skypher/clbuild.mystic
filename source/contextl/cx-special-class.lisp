(in-package :contextl)

(defclass special-object (standard-object)
  ())

(defclass special-class (standard-class)
  (old-slot-definitions)
  (:default-initargs :direct-superclasses (list (find-class 'special-object))))

(defmethod validate-superclass
           ((class special-class)
            (superclass standard-class))
  t)

(defmethod initialize-instance :around
  ((class special-class) &rest initargs
   &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for superclass in direct-superclasses
            thereis (subclassp superclass 'special-object))
    (call-next-method)
    (apply #'call-next-method class
           :direct-superclasses
           (append direct-superclasses
                   (list (find-class 'special-object)))
           initargs)))

(defmethod reinitialize-instance :around
  ((class special-class) &rest initargs
   &key (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
    (if (loop for superclass in direct-superclasses
              thereis (subclassp superclass 'special-object))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'special-object)))
             initargs))
    (call-next-method)))

(defgeneric slot-definition-specialp (slot)
  (:method ((slot slot-definition)) nil))

(defclass special-direct-slot-definition (standard-direct-slot-definition)
  ((specialp :initarg :special
             :initform nil
             :reader slot-definition-specialp)))

(defclass special-effective-slot-definition (standard-effective-slot-definition)
  ())

(defmethod slot-definition-specialp ((slot special-effective-slot-definition))
  t)

(defmethod direct-slot-definition-class
           ((class special-class) &key &allow-other-keys)
  (load-time-value (find-class 'special-direct-slot-definition)))

(defvar *special-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
           ((class special-class) &key &allow-other-keys)
  *special-effective-slot-definition-class*)

(defmethod compute-effective-slot-definition
           ((class special-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((*special-effective-slot-definition-class*
         (if (some #'slot-definition-specialp direct-slot-definitions)
           (load-time-value (find-class 'special-effective-slot-definition))
           (load-time-value (find-class 'standard-effective-slot-definition)))))
    (call-next-method)))

(defun shift-slot (object slot-name)
  (with-symbol-access
    (let ((slot-value (slot-value object slot-name)))
      (unless (special-symbol-p slot-value)
        (slot-makunbound object slot-name)
        (without-symbol-access
          (setf (slot-value object slot-name) slot-value))))))

#|
Note on thread safety: All special slots are initialized in shared-initialize.
This means that outside of object initialization, slot-value and slot-boundp
don't have any side effects, only potentially during object (re)initialization.
|#

(defmethod shared-initialize ((object special-object) slot-names &rest all-keys)
  (declare (dynamic-extent all-keys))
  (let ((class-slots (class-slots (class-of object))))
    (loop for slot in class-slots do
          (when (and (typep slot 'special-effective-slot-definition)
                     (not (eq (slot-definition-allocation slot) :class)))
            (shift-slot object (slot-definition-name slot)))
          (when-let (slot-initargs (slot-definition-initargs slot))
            (multiple-value-bind
                (indicator value)
                (get-properties all-keys slot-initargs)
              (when indicator
                (setf (slot-value object (slot-definition-name slot)) value)))))
    (if (eq slot-names 't)
      (loop for slot in class-slots
            for slot-name = (slot-definition-name slot)
            unless (slot-boundp object slot-name) do
            (let ((slot-initfunction (slot-definition-initfunction slot)))
              (when slot-initfunction
                (setf (slot-value object slot-name) (funcall slot-initfunction)))))
      (loop for slot-name in slot-names
            for slot = (find slot-name class-slots :key #'slot-definition-name)
            unless (slot-boundp object slot-name) do
            (let ((slot-initfunction (slot-definition-initfunction slot)))
              (when slot-initfunction
                (setf (slot-value object slot-name) (funcall slot-initfunction)))))))
  object)

(defmethod slot-unbound ((class special-class) object slot-name)
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (if *symbol-access*
    (let ((slot (find slot-name (the list (class-slots class))
                      :test #'eq
                      :key #'slot-definition-name)))
      (if (typep slot 'special-effective-slot-definition)
        (setf (slot-value-using-class class object slot)
              (make-special-symbol))
        (call-next-method)))
    (call-next-method)))

(defmethod slot-value-using-class
           ((class special-class) object (slot special-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (if *symbol-access* (call-next-method)
    (let ((slot-symbol (with-symbol-access (call-next-method))))
      (declare (type symbol slot-symbol))
      (if (boundp slot-symbol)
        (symbol-value slot-symbol)
        (slot-unbound class object (slot-definition-name slot))))))

(defmethod (setf slot-value-using-class)
           (new-value (class special-class) object (slot special-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (if *symbol-access* (call-next-method)
    (let ((slot-symbol (with-symbol-access (slot-value-using-class class object slot))))
      (setf (symbol-value (the symbol slot-symbol)) new-value))))

(defmethod slot-boundp-using-class
           ((class special-class) object (slot special-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (if *symbol-access* (call-next-method)
    (let ((slot-symbol (with-symbol-access (slot-value-using-class class object slot))))
      (boundp (the symbol slot-symbol)))))

(defmethod slot-makunbound-using-class
           ((class special-class) object (slot special-effective-slot-definition))
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (if *symbol-access* (call-next-method)
    (let ((slot-symbol (with-symbol-access (slot-value-using-class class object slot))))
      (makunbound (the symbol slot-symbol))
      object)))

#+(or allegro lispworks)
(defmethod make-instances-obsolete :after ((class special-class))
  (mapc #'make-instances-obsolete (class-direct-subclasses class)))

(defmethod reinitialize-instance :before
  ((class special-class) &key)
  (when (class-finalized-p class)
    (setf (slot-value class 'old-slot-definitions)
          (class-slots class))))

#+cmu
(defmethod reinitialize-instance :after
  ((class special-class) &key)
  (finalize-inheritance class))

(defmethod finalize-inheritance :after
  ((class special-class))
  "ensure that special slots remain special after class redefinition
   (there is no protocol for collapsing multiple values in different
   dynamic scopes for the same special slot); make instances obsolete
   when non-special slots have been turned into special slots"
  (when (slot-boundp class 'old-slot-definitions)
    (assert (loop for old-slot in (slot-value class 'old-slot-definitions)
                  for new-slot = (find (slot-definition-name old-slot)
                                       (class-slots class)
                                       :test #'eq
                                       :key #'slot-definition-name)
                  always
                  #+(and allegro (not (version>= 7 0)))
                  (cond ((null new-slot) t)
                        (t (eql (typep old-slot 'special-effective-slot-definition)
                                (typep new-slot 'special-effective-slot-definition))))
                  #-(and allegro (not (version>= 7 0)))
                  (cond ((null new-slot) t)
                        ((typep old-slot 'special-effective-slot-definition)
                         (typep new-slot 'special-effective-slot-definition))
                        (t (when (typep new-slot 'special-effective-slot-definition)
                             (make-instances-obsolete class))
                           t)))
        ()
      #+(and allegro (not (version>= 7 0)))
      "The (non-)special slots in class ~S must remain (non-)special."
      #-(and allegro (not (version>= 7 0)))
      "The special slots in class ~S must remain special."
      (class-name class))
    (slot-makunbound class 'old-slot-definitions))
    
  (loop with prototype = (class-prototype class)
        for slot in (class-slots class)
        when (and (typep slot 'special-effective-slot-definition)
                  (eq (slot-definition-allocation slot) :class))
        do (shift-slot prototype (slot-definition-name slot))))

(defun funcall-with-special-initargs (bindings thunk)
  (special-symbol-progv
      (loop for (object . initargs) in bindings
            for initarg-keys = (loop for key in initargs by #'cddr collect key)
            nconc (loop for slot in (class-slots (class-of object))
                        when (and (slot-definition-specialp slot)
                                  (intersection initarg-keys (slot-definition-initargs slot)))
                        collect (with-symbol-access
                                  (slot-value object (slot-definition-name slot)))))
      '()
    (loop for (object . initargs) in bindings
          do (apply #'shared-initialize object nil :allow-other-keys t initargs))
    (funcall thunk)))

(defmacro with-special-initargs ((&rest bindings) &body body)
  `(funcall-with-special-initargs
    (list ,@(loop for binding in bindings
                  collect `(list ,@binding)))
    (lambda () ,@body)))

(defmacro with-special-initargs* ((&rest bindings) &body body)
  (if bindings
    `(with-special-initargs (,(car bindings))
       (with-special-initargs* (,@(cdr bindings))
         ,@body))
    `(progn ,@body)))
