;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(declaim (inline trim))

(defun write-decimal-digits (stream number &key minimum-digit-count (maximum-digit-count most-positive-fixnum)
                             (padding-character #\0))
  (declare (optimize speed)
           (type (or null fixnum) minimum-digit-count)
           (type fixnum maximum-digit-count))
  (bind ((remainder number)
         (digit 0)
         (number-of-digits 0)
         (digits (list)))
    (declare (dynamic-extent digits)
             (type fixnum digit number-of-digits))
    (iter (repeat maximum-digit-count)
          (setf (values remainder digit) (truncate remainder 10))
          (push digit digits)
          (incf number-of-digits)
          (until (zerop remainder)))
    (when minimum-digit-count
      (bind ((padding-length (- minimum-digit-count number-of-digits)))
        (when (plusp padding-length)
          (iter (repeat padding-length)
                (write-char padding-character stream)))))
    (dolist (digit digits)
      (write-char (code-char (+ #x30 digit)) stream)))
  (values))

(defmacro slot-value-unless-nil (instance slot-name)
  (once-only (instance)
    `(when ,instance
       (slot-value ,instance ,slot-name))))

(defun read-key->value-text-file-into-hashtable (file)
  (with-open-file (file-stream file :element-type '(unsigned-byte 8))
    (let ((stream (flexi-streams:make-flexi-stream file-stream :external-format :utf-8)))
      (iter (with result = (make-hash-table :test #'equal))
            (for line in-stream stream :using (lambda (stream eof-error-p eof-value)
                                                (let ((result (read-line stream eof-error-p eof-value)))
                                                  (if (eq result eof-value)
                                                      eof-value
                                                      (trim result)))))
            (for line-number from 0)
            (when (or (zerop (length line))
                      (eql (aref line 0) #\;))
              (next-iteration))
            (for pieces = (cl-ppcre:split (load-time-value
                                           (cl-ppcre:create-scanner
                                            (concatenate 'string "[ |" (list #\Tab) "]+")))
                                 line))
            (for split-count = (length pieces))
            (when (> split-count 2)
              (warn "Syntax error at line ~A, too many pieces after split: ~A" line-number pieces))
            (for singular = (elt pieces 0))
            (for plural = (if (= split-count 1)
                              singular
                              (elt pieces 1)))
            (setf (gethash singular result) plural)
            (finally (return result))))))

(defun required-arg (name)
  (error "~A is a required argument" name))

(defun capitalize-first-letter (str)
  (if (and (> (length str) 0)
           (not (upper-case-p (elt str 0))))
      (capitalize-first-letter! (copy-seq str))
      str))

(defun capitalize-first-letter! (str)
  (setf (aref str 0) (char-upcase (aref str 0)))
  str)

(defun concatenate-separated-by (separator &rest args)
  (with-output-to-string (out)
    (iter (for el :in args)
          (when el
            (unless (first-time-p)
              (princ separator out))
            (princ el out)))))

(define-constant +whitespaces+ (list #\Space #\Tab)
  :test #'equal)

(defun trim (string &optional (bag +whitespaces+))
  (string-trim bag string))

(defmacro do-locales ((var locales &optional return-value) &rest body)
  "Iterate all locale in LOCALES and all their precedence lists in the locale precedence order."
  (with-unique-names (top-block locale end)
    `(block ,top-block
       (dolist (,locale ,locales)
         (dolist (,var (precedence-list-of ,locale))
           (tagbody
              (return-from ,top-block (block nil
                                        ,@body
                                        (go ,end)))
              ,end)))
       ,return-value)))

(defmacro do-current-locales (var &rest body)
  "DO-LOCALES on *LOCALE*."
  `(do-locales (,var *locale*)
     ,@body))

(defmacro do-current-locales-for-resource (name var &rest body)
  "DO-LOCALES on *LOCALE* that calls RESOURCE-MISSING unless there's a non-local exit in its body."
  `(do-locales (,var *locale* (resource-missing ,name))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some duplicates copied from various other libs to lower the number of dependencies

;; from verrazano
(defun camel-case-to-hyphened (input)
  (if (> (length input) 0)
      (string-downcase
       (with-output-to-string (*standard-output*)
         (iter (with in-uppercase? = (upper-case-p (elt input 0)))
               (for run-length :upfrom 0)
               (for hyphen-distance :upfrom 0)
               (for char :in-vector input)
               (for previous-char :previous char :initially #\ )
               (let ((new-in-uppercase? (if (alpha-char-p char)
                                            (upper-case-p char)
                                            (if (alpha-char-p previous-char)
                                                (not in-uppercase?)
                                                in-uppercase?))))
                 (unless (eq in-uppercase? new-in-uppercase?)
                   ;;(break "~A ~A ~A ~A" previous-char char run-length hyphen-distance)
                   (when (and (alphanumericp char)
                              (alphanumericp previous-char)
                              (or (> run-length 1)
                                  (> hyphen-distance 1)))
                     (write-char #\-)
                     (setf hyphen-distance 0))
                   (setf run-length 0)
                   (setf in-uppercase? new-in-uppercase?)))
               (write-char char))))
      input))

(defmacro rebinding (bindings &body body)
  "Bind each var in BINDINGS to a gensym, bind the gensym to
var's value via a let, return BODY's value wrapped in this let.

Evaluates a series of forms in the lexical environment that is
formed by adding the binding of each VAR to a fresh, uninterned
symbol, and the binding of that fresh, uninterned symbol to VAR's
original value, i.e., its value in the current lexical
environment.

The uninterned symbol is created as if by a call to GENSYM with the
string denoted by PREFIX - or, if PREFIX is not supplied, the string
denoted by VAR - as argument.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (car (if (consp binding) binding (list binding)))
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let* ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                               ,,@body))))))

(defun handle-otherwise (otherwise)
  (cond
    ((eq otherwise :error)
     (error "Otherwise assertion failed"))
    ((and (consp otherwise)
          (member (first otherwise) '(:error :warn) :test #'eq))
     (assert (not (null (rest otherwise))))
     (apply (ecase (first otherwise)
              (:error #'error)
              (:warn  #'warn))
            (rest otherwise)))
    ((functionp otherwise)
     (funcall otherwise))
    (t
     otherwise)))

(defun not-yet-implemented (&optional message)
  (cerror "try yo continue" (apply #'concatenate 'string "Not yet implemented" (when message (list ": " message)))))

(defun singlep (list)
  (and (consp list) (not (cdr list))))

(defmacro if-bind (var test &body then/else)
  "Anaphoric IF control structure.

VAR (a symbol) will be bound to the primary value of TEST. If
TEST returns a true value then THEN will be executed, otherwise
ELSE will be executed."
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  "Just like IF-BIND but the var is always IT."
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  "Just like when except VAR will be bound to the
  result of TEST in BODY."
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  "Just like when expect the symbol IT will be
  bound to the result of TEST in BODY."
  `(when-bind it ,test ,@body))

(defmacro cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
          (progn ,@(if body body (list var)))
          (cond-bind ,var ,@others)))
      nil))

(defmacro acond (&rest clauses)
  "Just like cond-bind except the var is automatically IT."
  `(cond-bind it ,@clauses))

(defun getenv (var)
  #+allegro (sys:getenv var)
  #+clisp (ext:getenv var)
  #+cmu
  (cdr (assoc var ext:*environment-list* :test #'string=))
  #+lispworks (lw:environment-variable var)
  #+openmcl (ccl::getenv var)
  #+sbcl (sb-ext:posix-getenv var)

  #-(or allegro clisp cmu lispworks openmcl openmcl sbcl)
  (error "Could not define `getenv'."))
