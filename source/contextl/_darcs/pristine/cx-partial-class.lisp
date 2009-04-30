(in-package :contextl)

(defclass partial-object (standard-object)
  ()
  (:default-initargs :allow-other-keys t))

(defclass partial-class (standard-class)
  ((defining-classes :initarg defining-classes
                     :reader partial-class-defining-classes)
   (defining-metaclass :initarg :defining-metaclass
                       :reader partial-class-defining-metaclass)))

(defmethod validate-superclass
           ((class partial-class)
            (superclass standard-class))
  t)

(defmethod validate-superclass
           ((class standard-class)
            (superclass partial-class))
  t)

#+allegro
(defmethod finalize-inheritance :after ((class partial-class))
  (mapc #'finalize-inheritance (rest (class-precedence-list class))))

(defmethod initialize-instance :around
  ((class partial-class) &rest initargs
   &key name
   (in-layer 't)
   (defining-metaclass 'standard-class))
  (declare (dynamic-extent initargs))
  (let ((in-layer-name (layer-name in-layer))
        (direct-superclasses (list (find-class 'partial-object)))
        (defining-classes ()))
    (let ((defined-class
           (apply #'make-instance defining-metaclass
                  (loop for (key value) on initargs by #'cddr
                        unless (member key '(:name :defining-metaclass))
                        nconc (list key value)))))
      (push defined-class direct-superclasses)
      (setf (getf defining-classes in-layer-name) defined-class))
    (unless (eq in-layer-name 't)
      (let ((defined-class (make-instance defining-metaclass)))
        (push defined-class direct-superclasses)
        (setf (getf defining-classes 't) defined-class)))
    (call-next-method class
                      :name name
                      :direct-superclasses direct-superclasses
                      'defining-classes defining-classes
                      :defining-metaclass defining-metaclass)))

(defmethod reinitialize-instance :around
  ((class partial-class) &rest initargs
   &key (in-layer 't)
   (defining-metaclass (partial-class-defining-metaclass class) defining-metaclass-p))
  (declare (dynamic-extent initargs))
  (let ((in-layer-name (layer-name in-layer)))
    (let ((defined-class (getf (partial-class-defining-classes class) in-layer-name)))
      (if defined-class
        (progn
          (apply #'reinitialize-instance defined-class
                 (loop for (key value) on initargs by #'cddr
                       unless (member key '(:name :defining-metaclass))
                       nconc (list key value)))
          (call-next-method class))
        (let ((defined-class
               (apply #'make-instance defining-metaclass
                      (loop for (key value) on initargs by #'cddr
                            unless (member key '(:name :defining-metaclass))
                            nconc (list key value)))))
          (apply #'call-next-method class
                 :direct-superclasses
                 (append (remove (find-class 'partial-object)
                                 (class-direct-superclasses class))
                         (list defined-class)
                         (list (find-class 'partial-object)))
                 'defining-classes
                 (list* in-layer-name defined-class
                        (partial-class-defining-classes class))
                 (when defining-metaclass-p
                   (list :defining-metaclass defining-metaclass))))))))
