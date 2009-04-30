;;; this file contains code to parse xml using cxml into CLOS nodes
;;; (hint: multiple dispatch) that have a special feature: (slot-value
;;; node 'some-attribute) returns the attribute value even if the
;;; class has no such slot (using a hashtable).  it is to be moved
;;; into the cxml repo as a plugin if accepted by the maintainer.

(defpackage :flexml
  (:use :common-lisp :cxml)
  (:export
   #:register-flexml-namespace
   #:make-flexml-builder
   #:flexml-builder
   #:flexml-node
   #:parent-of
   #:children-of
   #:attributes-of
   #:root-of
   #:class-name-for-node-name
   #:class-for-node-name
   #:local-name-of
   #:cross-referenced-node
   #:cross-referenced-nodes

   ;; some useful helpers
   #:the-only-child
   #:find-node-by-id
   #:first-child
   #:first-child-with-type
   #:first-child-with-local-name
   #:string-content-of
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "XML")
    (defpackage "XML"
      (:export
       #:id
       #:base
       #:lang
       #:space))))

(in-package :flexml)

(defparameter *xml-namespace->lisp-package* (make-hash-table :test #'equalp))

(defun register-flexml-namespace (namespace-uri lisp-package)
  (setf (gethash namespace-uri *xml-namespace->lisp-package*) (find-package lisp-package)))

(defparameter +xml-namespace-uri+ "http://www.w3.org/XML/1998/namespace")

;; for details see http://www.w3.org/XML/1998/namespace.html
(register-flexml-namespace +xml-namespace-uri+ "XML")

(defun find-lisp-package-for-xml-namespace (namespace-uri)
  (let ((package (gethash namespace-uri *xml-namespace->lisp-package*)))
    (unless package
      (error "No lisp package is mapped for XML namespace ~S. See REGISTER-XML-NAMESPACE." namespace-uri))
    package))

(defclass flexml-builder (sax:default-handler)
  ((default-package :accessor default-package-of :initarg :default-package)
   (id-attributes :initform (make-hash-table :test 'equal) :accessor id-attributes-of :documentation "Hashtable of (namespace-uri name) -> id-attribute-entry")
   (element-stack :initform nil :accessor element-stack-of)
   (root :accessor root-of)
   (default-node-class :initform nil
                       :initarg :default-node-class
                       :accessor default-node-class-of
                       :type (or symbol class)
                       :documentation "The default value for CLASS-FOR-NODE-NAME. When NIL an error is thrown when an unknown tag is encountered, otherwise its value is used to instantiate the node.")
   (include-default-values :initform t
                           :initarg :include-default-values
                           :type boolean
                           :accessor include-default-values-p)
   (id->node :initform (make-hash-table :test #'equal)
             :accessor id->node-of)
   (cross-referencing-slots :initform nil
                            :accessor cross-referencing-slots-of)))

(defclass id-attribute-entry ()
  ((id->node :initform (make-hash-table :test 'equal) :accessor id->node-of)
   (namespace-uri :accessor namespace-uri-of :initarg :namespace-uri)
   (name :accessor name-of :initarg :name)))

(defmethod initialize-instance :after ((self flexml-builder) &key id-attributes)
  (etypecase id-attributes
    (null)
    (cons
     (dolist (entry id-attributes)
       (let ((namespace-uri nil)
             (name nil))
         (if (consp entry)
             (progn
               (assert (= 2 (length entry)))
               (setf namespace-uri (first entry))
               (setf name (second entry)))
             (progn
               (assert (stringp entry))
               (setf name entry)))
         (setf (gethash (list namespace-uri name) (id-attributes-of self))
               (make-instance 'id-attribute-entry
                              :namespace-uri namespace-uri
                              :name name)))))
    (hash-table
     ;; let's assume the user knows what they want and it's fine as it is...
     (setf (id-attributes-of self) id-attributes))))

(defun make-flexml-builder (&key (default-package nil default-package-p) (include-default-values t)
                            id-attributes)
  (let ((builder (make-instance 'flexml-builder
                                :include-default-values include-default-values
                                :id-attributes id-attributes)))
    (when default-package-p
      (setf (default-package-of builder) default-package))
    builder))

(defclass flexml-node ()
  ((xml:id :initform nil :accessor id-of)
   (parent :initform nil :initarg :paren :accessor parent-of)
   (children :initform nil :initarg :children :accessor children-of)
   (attributes :initform nil :initarg :attributes :accessor attributes-of)
   (local-name :initform nil :initarg :local-name :accessor local-name-of)))

(defmethod slot-missing ((class t) (node flexml-node) slot-name operation &optional new-value)
  (let ((attributes (attributes-of node)))
    (ecase operation
      (slot-value
       (if (listp attributes)
           (cdr (assoc slot-name attributes))
           (gethash slot-name attributes)))
      (setf
       (if (listp attributes)
           (let ((entry (assoc slot-name attributes)))
             (if entry 
                 (setf (cdr entry) new-value)
                 (push (cons slot-name new-value) (attributes-of node))))
           (if new-value
               (setf (gethash slot-name attributes) new-value)
               (remhash slot-name attributes))))
      (slot-boundp
       t)
      (slot-makunbound
       (error "FLEXML-NODE's don't support SLOT-MAKUNBOUND")))))

(defun missing-cross-reference (node slot id)
  (unless (symbolp slot)
    (setf slot (closer-mop:slot-definition-name slot)))
  (error "Referenced node with id ~S in slot ~S of node ~A was not found" id slot node))

(defmethod sax:end-document ((builder flexml-builder))
  (loop
     for (node . slot) :in (cross-referencing-slots-of builder)
     for slot-value = (closer-mop:slot-value-using-class (class-of node) node slot) do
       ;; TODO this needs to be updated to follow the id-attributes stuff
       (ecase (closer-mop:slot-definition-type slot)
         (cross-referenced-node
          (assert (stringp slot-value))
          (let ((referenced-node (gethash slot-value (id->node-of builder))))
            (unless referenced-node
              (missing-cross-reference node slot slot-value))
            (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                  referenced-node)))
         (cross-referenced-nodes
          (assert (stringp slot-value))
          (let ((ids (cl-ppcre:split " " slot-value))
                (id->node (id->node-of builder))
                (referenced-nodes ()))
            (dolist (id ids)
              (let ((referenced-node (gethash id id->node)))
                (unless referenced-node
                  (missing-cross-reference node slot id))
                (push referenced-node referenced-nodes)))
            (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                  (nreverse referenced-nodes))))))
  (root-of builder))

(deftype cross-referenced-nodes ()
  t)

(deftype cross-referenced-node ()
  t)

(defun find-slot (class slot-name &optional (errorp t))
  (declare (type standard-class class)
           (type symbol slot-name)
           (optimize speed))
  (let ((slot (find slot-name (the list (closer-mop:class-slots class))
                    :key #'closer-mop:slot-definition-name
                    :test #'eq)))
    (when (and errorp
               (not slot))
      (error "Slot ~S not found in class ~S" slot-name class))
    slot))

(defgeneric class-name-for-node-name (builder namespace-uri package local-name qualified-name)
  (:method (builder namespace-uri package (local-name string) qualified-name)
    (find-symbol (string-upcase local-name) (default-package-of builder))))

(defgeneric class-for-node-name (builder namespace-uri package local-name qualified-name)
  (:method (builder namespace-uri package (local-name string) qualified-name)
    (let* ((class-name (class-name-for-node-name builder namespace-uri package local-name qualified-name))
           (class (find-class class-name nil))
           (default-node-class (default-node-class-of builder)))
      (when (and default-node-class
                 (symbolp default-node-class))
        (setf default-node-class (find-class default-node-class)))
      (or class
          default-node-class
          (let ((*package* (find-package :cl-user)))
            (error "Did not find a class named ~S to represent the flexml node ~S in XML namespace ~S"
                   class-name local-name (or namespace-uri :default)))))))

(defmethod sax:start-element ((builder flexml-builder) namespace-uri local-name qname attributes)
  (let* ((include-default-values (include-default-values-p builder))
         (package (if namespace-uri
                      (find-lisp-package-for-xml-namespace namespace-uri)
                      (default-package-of builder)))
         (class (class-for-node-name builder namespace-uri package local-name qname)))
    (assert class)
    (assert (subtypep class 'flexml-node))
    (let* ((parent (first (element-stack-of builder)))
           (node (make-instance class))
           (id-attributes (id-attributes-of builder)))
      (setf (local-name-of node) local-name)
      (loop
         for attribute in attributes
         for attribute-local-name = (sax:attribute-local-name attribute)
         for attribute-namespace-uri = (or (sax:attribute-namespace-uri attribute)
                                           (when (member attribute-local-name '("base" "id" "lang" "space")
                                                         :test #'string=)
                                             ;; KLUDGE this shouldn't be here when parsing XML 1.1
                                             +xml-namespace-uri+))
         for attribute-package = (if attribute-namespace-uri
                                     (find-lisp-package-for-xml-namespace attribute-namespace-uri)
                                     (default-package-of builder))
         for attribute-value = (sax:attribute-value attribute)
         for id-attribute-entry = (gethash (list namespace-uri attribute-local-name) id-attributes)
         when id-attribute-entry do
           (let ((id->node (id->node-of id-attribute-entry)))
             (assert (not (gethash attribute-value id->node)) () "Duplicate id found for attribute ~A:~A, value is ~S"
                     (namespace-uri-of id-attribute-entry) (name-of id-attribute-entry) attribute-value)
             (setf (gethash attribute-value id->node) node))
         when (and (or (sax:attribute-specified-p attribute)
                       include-default-values)) do
           (let* ((slot-name (intern (string-upcase attribute-local-name) attribute-package))
                  (slot (find-slot class slot-name nil)))
             (if slot
                 ;; then grab its SLOT-DEFINITION-TYPE and process the value accordingly
                 (let ((slot-type (closer-mop:slot-definition-type slot)))
                   (when (and (consp slot-type)
                              (eq (first slot-type) 'or))
                     (unless (and (= (length slot-type) 3)
                                  (eq (second slot-type) 'null))
                       (error "Error parsing slot type ~S" slot-type))
                     (setf slot-type (third slot-type)))
                   (cond ((member slot-type '(cross-referenced-node
                                              cross-referenced-nodes))
                          (push (cons node slot) (cross-referencing-slots-of builder)))
                         ((subtypep slot-type 'integer)
                          (setf attribute-value (parse-integer attribute-value)))
                         ((subtypep slot-type 'boolean)
                          (setf attribute-value (not (null (member attribute-value
                                                                   '("1" "true" "TRUE")
                                                                   :test #'string=))))))
                   (setf (closer-mop:slot-value-using-class (class-of node) node slot)
                         attribute-value))
                 (setf (slot-value node slot-name) attribute-value))))
      (if parent
          (progn
            (setf (parent-of node) parent)
            (push node (children-of parent)))
          (setf (root-of builder) node))
      (push node (element-stack-of builder))
      node)))

(defmethod sax:end-element ((builder flexml-builder) namespace-uri local-name qname)
  (let ((node (pop (element-stack-of builder))))
    (setf (children-of node)
          (loop
             with old-children = (children-of node)
             with child-count  = (length old-children)
             with new-children = (if (zerop child-count)
                                     #() ; use a single array instace to represent the no children case
                                     (make-array child-count))
             for index :from (1- child-count) :downto 0
             do (setf (aref new-children index) (pop old-children))
             finally (return new-children)))
    node))

(defmethod sax:characters ((builder flexml-builder) data)
  (let* ((parent (first (element-stack-of builder)))
         (previous (first (children-of parent))))
    (if (stringp previous)
        (setf (car (children-of parent)) (concatenate 'string previous data))
        (push data (children-of parent)))))

;;;
;;; some useful helpers
;;;
(defun string-content-of (node)
  (unless (= 1 (length (children-of node)))
    (error "Node ~A has more then one children while expecting a single string content" node))
  (let ((value (elt (children-of node) 0)))
    (unless (stringp value)
      (error "Single child ~S of node ~A is not a string" value node))
    value))

(defun first-child (node)
  (elt (children-of node) 0))

(defun first-child-with-type (node type)
  (loop for child :across (children-of node) do
       (when (typep child type)
         (return-from first-child-with-type child))))

(defun first-child-with-local-name (node name)
  (loop for child :across (children-of node) do
       (when (string= (local-name-of child) name)
         (return-from first-child-with-local-name child))))

(defun the-only-child (node)
  (let ((children (children-of node)))
    (assert (= 1 (length children)))
    (elt children 0)))

(defgeneric find-node-by-id (id builder &key otherwise)
  (:method ((id string) (builder flexml-builder) &key (otherwise :error))
    (let ((result (gethash id (id->node-of builder))))
      (unless result
        (case otherwise
          (:error (error "No XML node found with id ~S" id))
          (:warn (warn "No XML node found with id ~S" id))
          (t (if (functionp otherwise)
                 (funcall otherwise)
                 otherwise))))
      result)))

