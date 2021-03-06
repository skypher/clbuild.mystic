
(in-package :weblocks)

(export '(widget-class))

(defclass widget-class (standard-class)
  ()
  (:documentation "A metaclass used for all widget classes. A
  custom metaclass is necessary to specialize
  'slot-value-using-class'."))

;;; Necessary to allow deriving
(defmethod validate-superclass ((class widget-class)
				(superclass standard-class))
  t)

;;; Allow customization of widget slot options
(defclass widget-slot-definition-mixin ()
  ((affects-dirty-status-p :accessor widget-slot-affects-dirty-status-p
			   :initform t
			   :initarg :affects-dirty-status-p
			   :documentation "When set to true (the
			   default), the widget will be made dirty
			   when this slot is modified."))
  (:documentation "A mixin class used in
  'widget-direct-slot-definition' and
  'widget-effective-slot-definition' to allow specifying custom widget
  properties."))

(defclass widget-direct-slot-definition
    (standard-direct-slot-definition widget-slot-definition-mixin) 
  ()
  (:documentation "Allows specifying custom widget properties."))

(defmethod direct-slot-definition-class ((class widget-class) &rest initargs) 
  (declare (ignore initargs))
  (find-class 'widget-direct-slot-definition))

;;; Copy slot options over to runtime definition of the slot
(defclass widget-effective-slot-definition
    (standard-effective-slot-definition widget-slot-definition-mixin) 
  ()
  (:documentation "Allows specifying custom widget properties."))

(defmethod effective-slot-definition-class ((class widget-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'widget-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class widget-class) slot-name dslotds)
  (declare (ignore slot-name))
  (let ((result (call-next-method)))
    (loop for dsd in dslotds
	 when (typep dsd 'widget-direct-slot-definition)
	 do (setf (widget-slot-affects-dirty-status-p result) (widget-slot-affects-dirty-status-p dsd))
	 return dsd)
    result))

