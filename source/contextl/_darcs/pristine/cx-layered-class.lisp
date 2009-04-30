(in-package :contextl)

(defclass special-layered-access-class
          (layered-access-class special-class standard-class-in-layer)
  ())

(defclass special-layered-direct-slot-definition
          (layered-direct-slot-definition
           special-direct-slot-definition
           standard-direct-slot-definition-in-layer)
  ())

(defclass special-effective-slot-definition-in-layers
          (special-effective-slot-definition
           standard-effective-slot-definition-in-layers)
  ())

(defclass layered-effective-slot-definition-in-layers
          (layered-effective-slot-definition
           standard-effective-slot-definition-in-layers)
  ())

(defclass special-layered-effective-slot-definition
          (layered-effective-slot-definition-in-layers
           special-effective-slot-definition-in-layers)
  ())

(defmethod direct-slot-definition-class
           ((class special-layered-access-class) &key &allow-other-keys)
  (find-class 'special-layered-direct-slot-definition))

(defvar *special-layered-effective-slot-definition-class*)

(defmethod effective-slot-definition-class
           ((class special-layered-access-class) &key &allow-other-keys)
  *special-layered-effective-slot-definition-class*)

(defmethod compute-effective-slot-definition
           ((class special-layered-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((*special-layered-effective-slot-definition-class*
         (if (some #'slot-definition-layeredp direct-slot-definitions)
             (if (some #'slot-definition-specialp direct-slot-definitions)
                 (find-class 'special-layered-effective-slot-definition)
               (find-class 'layered-effective-slot-definition-in-layers))
           (if (some #'slot-definition-specialp direct-slot-definitions)
               (find-class 'special-effective-slot-definition-in-layers)
             (find-class 'standard-effective-slot-definition-in-layers)))))
    (call-next-method)))

(defclass layered-class (partial-class special-layered-access-class)
  ()
  (:default-initargs :defining-metaclass 'special-layered-access-class))

#+sbcl
(defmethod shared-initialize :after
  ((class layered-class) slot-names &key defining-metaclass)
  (declare (ignore slot-names defining-metaclass)))

(defmacro define-layered-class (&whole form name &body options)
  (let ((layer (if (member (car options) '(:in-layer :in) :test #'eq)
                 (cadr options)
                 t))
        (options (cond ((member (car options) '(:in-layer :in) :test #'eq)
                        (cddr options))
                       ((not (listp (car options)))
                        (error "Illegal option ~S in ~S."
                               (car options) form))
                       (t options))))
    `(defclass ,name ,(car options)
       ,(mapcar #'process-layered-access-slot-specification (cadr options))
       ,@(cddr options)
       ,@(unless (assoc :metaclass options)
           '((:metaclass layered-class)))
       (:in-layer . ,layer))))
