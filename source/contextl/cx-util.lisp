(in-package :contextl)

(defstruct (symbol-mapper (:constructor make-symbol-mapper (name)))
  (name nil :read-only t)
  (map (make-weak-hash-table :test #'eq :weakness :key) :read-only t)
  (lock (make-lock :name "symbol mapper") :read-only t))

(declaim (inline atomic-ensure-symbol-mapping))

(defun atomic-ensure-symbol-mapping (symbol mapper default-thunk)
  (or (gethash symbol (symbol-mapper-map mapper))
      (with-lock-held ((symbol-mapper-lock mapper))
        (or (gethash symbol (symbol-mapper-map mapper))
            (setf (gethash symbol (symbol-mapper-map mapper))
                  (funcall default-thunk))))))

(defmacro atomic-ensure-mapping (symbol mapper default)
  `(atomic-ensure-symbol-mapping ,symbol ,mapper (lambda () ,default)))

(defgeneric map-symbol (mapper symbol)
  (:method ((mapper symbol-mapper) (symbol symbol))
   (if (symbol-package symbol)
     (intern (format nil "=~A-FOR-~A="
                     (symbol-mapper-name mapper)
                     (symbol-name symbol))
             (symbol-package symbol))
     (atomic-ensure-mapping symbol mapper (gensym)))))

#|
Layers are represented as CLOS classes. To avoid nameclashes with plain
CLOS classes, the name of a layer is actually mapped to an internal
unambiguous name which is used instead of the regular name.
|#

(defvar *layer-class-definer*
  (make-symbol-mapper 'layer-class-definer))

(defun defining-layer (name)
  "Takes the name of a layer and returns its internal name."
  (case name
    ((t) 't)
    ((nil) (error "NIL is not a valid layer name."))
    (otherwise (map-symbol *layer-class-definer* name))))

#|
Layered functions have two names: The name of the caller and the name of
the definer. The caller is just a function that adds a representation of
the active layers to the list of arguments and calls the definer. The
definer is a generic function that contains all the layered methods.

The caller has the name under which a user knows about a layered function.
The definer has an automatically generated name that can be unambiguously
determined from the caller's name. So for example, consider the following
layered function definition:

(define-layered-function foo (...))

The caller is named 'foo whereas the definer is named something like
=layered-function-definer-for-foo=. [The details of the mapping should
be considered an implementation detail, though, and not part of the
"official" API of ContextL.]
|#

(defvar *layered-function-definer*
  (make-symbol-mapper 'layered-function-definer))

(defun lf-definer-name (name)
  "Takes the name of a layered function caller
   and returns the name of the corresponding definer."
  (cond ((plain-function-name-p name)
         (map-symbol *layered-function-definer* name))
        ((setf-function-name-p name)
         `(setf ,(map-symbol *layered-function-definer* (cadr name))))
        (t (error "Illegal function name: ~S." name))))

(defun bind-lf-names (name)
  "Takes the name of a layered function caller
   and ensures that it can be retrieved again
   from the name of a corresponding definer."
  (let ((plain-function-name (plain-function-name name)))
    (setf (get (map-symbol *layered-function-definer* plain-function-name)
               'layered-function-caller)
          plain-function-name)))

(defun lf-caller-name (name)
  "Takes the name of a layered function definer
   and returns the name of the corresponding caller."
  (cond ((plain-function-name-p name)
         (get name 'layered-function-caller))
        ((setf-function-name-p name)
         `(setf ,(get (cadr name) 'layered-function-caller)))
        (t (error "Illegal function name: ~S." name))))

#|
The following are utility functions to distingush between
the two kinds of function names available in Common Lisp.
|#

(declaim (inline plain-function-name-p))

(defun plain-function-name-p (name)
  (symbolp name))

(defun setf-function-name-p (name)
  (and (consp name)
       (eq (car name) 'setf)
       (symbolp (cadr name))
       (null (cddr name))))

(defun plain-function-name (name)
  (cond ((plain-function-name-p name) name)
        ((setf-function-name-p name) (cadr name))
        (t (error "Illegal function name ~S." name))))
