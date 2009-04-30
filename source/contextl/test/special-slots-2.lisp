(asdf:oos 'asdf:load-op :contextl)

(in-package :contextl-user)

(define-layered-class person2 ()
  ((name2 :initarg :name
          :layered-accessor person-name2)))

(defparameter *p*
  (make-instance 'person2 :name "Dr. Jekyll"))

(assert (equal (person-name2 *p*) "Dr. Jekyll"))

(handler-bind
    ((error (lambda (error)
              (eval '(define-layered-class person2 ()
                       ((name2 :initarg :name
                               :special t
                               :layered-accessor person-name2))))
              (assert (equal (person-name2 *p*) "Dr. Jekyll"))
              (continue error))))
  (dletf (((person-name2 *p*) "Mr. Hide"))
    (assert (equal (person-name2 *p*) "Mr. Hide"))))

(assert (equal (person-name2 *p*) "Dr. Jekyll"))

(print :done)

#+allegro (excl:exit)
#+cmu (ext:quit)
#+(or openmcl clozure-common-lisp) (ccl:quit)
#+sbcl (sb-ext:quit)
