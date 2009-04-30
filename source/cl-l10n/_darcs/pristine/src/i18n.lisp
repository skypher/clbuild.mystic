;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(declaim (inline resource-key))

(defun resource-key (name)
  (string-downcase name))

(define-condition resource-missing (warning)
  ((locale :initform *locale* :accessor locale-of :initarg :locale)
   (name :accessor name-of :initarg :name))
  (:report
   (lambda (condition stream)
     (cl:format stream "The resource ~S is missing for ~A"
                (name-of condition) (locale-of condition)))))

(defun resource-missing (name)
  (warn 'resource-missing :name name)
  name)

(defun ensure-resource-lookup-stub (name)
  (unless (get name 'resource-lookup-stub)
    ;; define a function with this name that'll look at the *locale* list and call the first
    ;; locale specific lambda it finds while walking the locales
    (when (fboundp name)
      (simple-style-warning "Redefining function definiton of ~S while adding a functional locale specific resource" name))
    (setf (symbol-function name)
          (lambda (&rest args)
            (lookup-resource name :arguments args)))
    ;; leave a mark that it's been defined by us
    (setf (get name 'resource-lookup-stub) t)))

(defun %set-resource (locale name resource)
  "Store RESOURCE in the resource map at the given locale. When RESOURCE is functionp then define a function on NAME that will dispatch on *locale* when called and funcall the resource registered for the current locale."
  (check-type name (or string symbol))
  (check-type locale locale)
  (let ((key (if (functionp resource)
                 (progn
                   (unless (symbolp name)
                     (error "~S: Please use symbols to name functional resources." name))
                   (ensure-resource-lookup-stub name)
                   name)
                 (string-downcase name))))
    (setf (gethash key (resources-of locale)) resource))
  name)

(defun %lookup-resource (locale name args)
  (check-type name (or symbol string))
  (check-type locale locale)
  (bind ((resources (resources-of locale))
         ((:values resource foundp) (gethash name resources)))
    (unless foundp
      (setf (values resource foundp) (gethash (string-downcase name) resources)))
    (if foundp
        ;; dispatch on resource type
        (cond ((functionp resource)
               (values (apply resource args) t))
              (args
               (values (apply #'cl:format nil resource args) t))
              (t
               (values resource t)))    ; a simple literal
        (values nil nil))))

(defun lookup-resource (name &key arguments (otherwise (if arguments :error name) otherwise-provided?))
  (do-current-locales locale
    (multiple-value-bind (result foundp) (funcall '%lookup-resource locale name arguments)
      (when foundp
        (return-from lookup-resource (values result t)))))
  (cond
    ((not otherwise-provided?)
     (resource-missing name)
     name)
    ((eq otherwise :error)
     (error "LOOKUP-RESOURCE unexpectedly failed for ~S with arguments ~S and locale ~A" name arguments *locale*))
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

(defun (setf lookup-resource) (value name)
  (%set-resource *locale* name value))

(defmacro defresources (locale-designator &body resources)
  (with-unique-names (locale)
    `(progn
       (eval-when (:compile-toplevel)
         ,@(iter (for resource in resources)
                 (for name = (first resource))
                 (when (> (length resource) 2)
                   (collect `(ensure-resource-lookup-stub ',name)))))
       (eval-when (:load-toplevel :execute)
         (let ((,locale (locale ,(canonical-locale-name-from locale-designator))))
           (declare (ignorable ,locale))
           ,@(iter (for resource in resources)
                   (for name = (first resource))
                   (if (= 2 (length resource))
                       (collect `(%set-resource ,locale ',name ',(second resource)))
                       (collect `(%set-resource ,locale ',name (lambda ,(second resource)
                                                                 ,@(cddr resource)))))))))))

(defmacro lookup-first-matching-resource (&body specs)
  "Try to look up the resource keys, return the first match, fallback to the first key.
When a resource key is a list, its elements will be concatenated separated by dots and
components evaluating to NIL are excluded from the constructed key.
An example usage:
  (lookup-first-matching-resource
    ((awhen attribute (name-of it)) (name-of state))
    (when some-random-condition
      (name-of (state-machine-of state)) (name-of state))
    (\"state-name\" (name-of state))
    \"last-try\")"
  (with-unique-names (fallback key-tmp block resource foundp)
    (iter (for spec :in specs)
          (for wrapper = `(progn))
          (when (and (consp spec)
                     (member (first spec) '(when unless)))
            (setf wrapper (subseq spec 0 2))
            (setf spec (cddr spec)))
          (for key = (cond ((atom spec)
                            spec)
                           ((and (listp spec)
                                 (= 1 (length spec)))
                            (first spec))
                           (t `(concatenate-separated-by "." ,@spec))))
          (collect `(,@wrapper
                     (setf ,key-tmp ,key)
                     (multiple-value-bind (,resource ,foundp)
                         (lookup-resource ,key-tmp :otherwise nil)
                       (unless ,fallback
                         (setf ,fallback ,key-tmp))
                       (when ,foundp
                         (return-from ,block (values ,resource t)))))
            :into lookups)
          (finally
           (return `(block ,block
                      ;; the first lookup must be treated differently to avoid double evaluation of the key
                      (let ((,fallback nil)
                            (,key-tmp nil))
                        ,@lookups
                        (return-from ,block (values ,fallback nil)))))))))

(defmacro enable-sharpquote-reader ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a lookup-resource call for the string.
Be careful when using in different situations, because it modifies *readtable*."
  ;; The standard sais that *readtable* is restored after loading/compiling a file,
  ;; so we make a copy and alter that. The effect is that it will be enabled
  ;; for the rest of the file being processed.
  `(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharpquote-reader)))

(defun %enable-sharpquote-reader ()
  (set-dispatch-macro-character
   #\# #\"
   #'(lambda (s c1 c2)
       (declare (ignore c2))
       (unread-char c1 s)
       `(lookup-resource ,(read s)))))

(defun with-sharpquote-syntax ()
  "To be used with the curly reader from arnesi: {with-sharpquote-reader (foo #\"locale-specific\") }"
  (lambda (handler)
    (%enable-sharpquote-reader)
    `(progn ,@(funcall handler))))


(defgeneric localize (object)
  (:documentation "Override this generic method for various data types. Return (values result foundp)."))

(defmethod localize ((resource-name t))
  "By default we look up everything as a constant or a function with zero args."
  (lookup-resource resource-name))

;;;
;;; some custom accessors
;;;
(defun ldml-symbol-p (name)
  (or (integerp name)
      (and (symbolp name)
           (find-symbol (symbol-name name)
                        #.(find-package :cl-l10n.ldml))
           t)))

(defun ensure-ldml-symbol (name)
  (if (integerp name)
      name
      (intern (string-upcase (string name)) :cl-l10n.ldml)))

(defmacro defun-with-capitalizer (name args &body body)
  (unless (member '&key args)
    (appendf args '(&key)))
  (appendf args '(capitalize-first-letter))
  `(defun ,name ,args
     (bind (((:values str foundp)
             (progn
               ,@body)))
       (values (if capitalize-first-letter
                   (capitalize-first-letter str)
                   str)
               foundp))))

(defun-with-capitalizer cl-l10n.lang:localize-currency-symbol (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (currencies-of locale))
      (return (values (second it) t)))))

(defun-with-capitalizer cl-l10n.lang:localize-currency-name (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (currencies-of locale))
      (return (values (first it) t)))))

(defun-with-capitalizer cl-l10n.lang:localize-language-name (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (languages-of locale))
      (return (values it t)))))

(defun-with-capitalizer cl-l10n.lang:localize-script-name (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (scripts-of locale))
      (return (values it t)))))

(defun-with-capitalizer cl-l10n.lang:localize-territory-name (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (territories-of locale))
      (return (values it t)))))

(defun-with-capitalizer cl-l10n.lang:localize-variant-name (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (gethash name (variants-of locale))
      (return (values it t)))))

(defun-with-capitalizer cl-l10n.lang:localize-month-name (name &key abbreviated)
  (bind ((index name))
    (unless (integerp name)
      (setf index (position name '(cl-l10n.ldml:january cl-l10n.ldml:february cl-l10n.ldml:marc
                                   cl-l10n.ldml:april   cl-l10n.ldml:may      cl-l10n.ldml:june
                                   cl-l10n.ldml:july    cl-l10n.ldml:august   cl-l10n.ldml:september
                                   cl-l10n.ldml:october cl-l10n.ldml:november cl-l10n.ldml:december))))
    (unless (and index
                 (<= 0 index 11))
      (error "~S is not a valid month name, it should be either an integer between 0 and 11 or a symbol like 'CL-L10N.LDML:JANUARY" name))
    (do-current-locales-for-resource "<a month name>" locale
      (when-bind calendar (gregorian-calendar-of locale)
        (when-bind vector (if abbreviated
                              (abbreviated-month-names-of calendar)
                              (month-names-of calendar))
          (awhen (aref vector index)
            (return (values it t))))))))

(defun-with-capitalizer cl-l10n.lang:localize-day-name (name &key abbreviated)
  (bind ((index name))
    (unless (integerp name)
      (setf index (position name '(cl-l10n.ldml:sunday    cl-l10n.ldml:monday   cl-l10n.ldml:tuesday
                                   cl-l10n.ldml:wednesday cl-l10n.ldml:thursday cl-l10n.ldml:friday
                                   cl-l10n.ldml:saturday))))
    (unless (and index
                 (<= 0 index 6))
      (error "~S is not a valid day name, it should be either an integer between 0 and 6 (0 is Sunday) or a symbol like 'CL-L10N.LDML:SUNDAY" name))
    (do-current-locales-for-resource "<a day name>" locale
      (when-bind calendar (gregorian-calendar-of locale)
        (when-bind vector (if abbreviated
                              (abbreviated-day-names-of calendar)
                              (day-names-of calendar))
          (awhen (aref vector index)
            (return (values it t))))))))

(defun-with-capitalizer cl-l10n.lang:localize-quarter-name (name &key abbreviated)
  (bind ((index name))
    (unless (integerp name)
      (setf index (position name '(cl-l10n.ldml:first-quarter cl-l10n.ldml:second-quarter
                                   cl-l10n.ldml:third-quarter cl-l10n.ldml:fourth-quarter))))
    (unless (and index
                 (<= 0 index 3))
      (error "~S is not a valid quarter name, it should be either an integer between 0 and 3 or a symbol like 'CL-L10N.LDML:FIRST-QUARTER" name))
    (do-current-locales-for-resource "<a quarter name>" locale
      (when-bind calendar (gregorian-calendar-of locale)
        (when-bind vector (if abbreviated
                              (abbreviated-quarter-names-of calendar)
                              (quarter-names-of calendar))
          (awhen (aref vector index)
            (return (values it t))))))))

(defun-with-capitalizer cl-l10n.lang:localize-number-symbol (name)
  (assert (ldml-symbol-p name))
  (do-current-locales-for-resource name locale
    (awhen (assoc name (number-symbols-of locale) :test #'eq)
      (return (values (cdr it) t)))))

(defun localize-number-symbol-character (number-symbol-char)
  (bind ((number-symbol-name (case number-symbol-char
                               (#\. 'ldml:decimal)
                               (#\0 'ldml:native-zero-digit)
                               (#\, 'ldml:group)
                               (#\% 'ldml:percent-sign)
                               (#\‰ 'ldml:per-mille)
                               (#\∞ 'ldml:infinity)
                               (#\E 'ldml:exponential)
                               (#\+ 'ldml:plus-sign)
                               (#\- 'ldml:minus-sign)
                               (#\; 'ldml:list))))
    ;; TODO at parse time, coerce stuff like ldml:native-zero-digit to character
    ;; and update the pattern compilers, too!
    (if number-symbol-name
        (cl-l10n.lang:localize-number-symbol number-symbol-name)
        number-symbol-char)))
