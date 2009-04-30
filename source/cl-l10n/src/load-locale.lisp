;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(deftype locale-designator ()
  `(or locale string symbol))

(defun canonical-locale-name-from (locale)
  (check-type locale locale-designator)
  (cond
    ((typep locale 'locale)
     (locale-name locale))
    ((stringp locale) ;; (member locale '("root" "en_US_POSIX") :test #'string=)
     locale)
    ((and (symbolp locale)
          (equalp (symbol-name locale) "root"))
     "root")
    (t
     (bind ((name locale)
            (language nil)
            (script nil)
            (territory nil))
       (when (and (not (null name))
                  (symbolp name))
         (setf name (symbol-name name)))
       (bind ((parts (cl-ppcre:split "_" name))
              (count (length parts)))
         (unless (<= 1 count 3)
           (error "Can't parse locale name ~S" name))
         (setf language (first parts))
         (if (= count 2)
             (setf territory (second parts))
             (progn
               ;; FIXME this is broken for names like aa_ER_SAAHO, which doesn't have a script but does have a variant
               ;; it's low priority, because this entire parsing branch is mostly dead due to that stringp check above
               (setf script (second parts))
               (setf territory (third parts))))
         (unless (and (= (length language) 2)
                      (or (null territory)
                          (= (length territory) 2)))
           (error "~A is not a valid locale name (examples: en_GB, en_US, en, zh_Hans_CN)" locale))
         (setf language (string-downcase language))
         (when script
           (setf script (concatenate 'string
                                     (list (char-upcase (elt script 0)))
                                     (string-downcase (subseq script 1)))))
         (when territory
           (setf territory (string-upcase territory)))
         (let ((*print-pretty* nil))
           (with-output-to-string (*standard-output*)
             (write-string language)
             (awhen script
               (write-char #\_)
               (write-string it))
             (awhen territory
               (write-char #\_)
               (write-string it)))))))))

(define-condition locale-not-found-error (error)
  ((locale-name :initarg :locale-name :accessor locale-name-of))
  (:report (lambda (condition stream)
             (cl:format stream "Could not find locale definition for ~S among the CLDR files. (Hint: did you run 'cl-l10n/bin/update-cldr.sh' to download the CLDR files?)"
                        (locale-name-of condition)))))

(defun locale-not-found-error (locale-name)
  (error 'locale-not-found-error :locale-name locale-name))

(defun locale (locale-designator &key (use-cache t) (otherwise nil otherwise-p))
  "Find locale named by the specification LOCALE-DESIGNATOR. If USE-CACHE
is non-nil forcefully reload/reparse the cldr locale else
the locale is first looked for in *locale-cache*. If ERRORP is non-nil
signal an error that the locale file cannot be found.
If LOADER is non-nil skip everything and call loader with LOCALE-DESIGNATOR."
  (declare (type locale-designator locale-designator))
  (if (typep locale-designator 'locale)
      locale-designator
      (let ((name (canonical-locale-name-from locale-designator)))
        (awhen (and use-cache
                    (cached-locale name))
          (return-from locale it))
        (let ((file (cldr-pathname-for name)))
          (if (probe-file file)
              (let ((locale (parse-cldr-file name)))
                (setf (cached-locale name) locale)
                (dolist (listener *locale-loaded-listeners*)
                  (funcall listener name))
                locale)
              (handle-otherwise (if otherwise-p
                                    otherwise
                                    (lambda ()
                                      (locale-not-found-error locale-designator)))))))))

(register-locale-loaded-listener 'load-resource)

(defun load-resource (name)
  (let ((resource-file (project-relative-pathname
                        (make-pathname :directory
                                       '(:relative "src" "resources")
                                       :name name
                                       :type "lisp"))))
    (awhen (probe-file resource-file)
      (when (pathname-name it)
        (load it)))))

(defun reload-resources ()
  (load-resource "common")
  (iter (for (name nil) :in-hashtable *locale-cache*)
        (load-resource name)))

(defun load-all-locales (&key (ignore-errors nil) (use-cache t))
  "Load all locale found in pathname designator PATH."
  (cl-fad:walk-directory
   *cldr-root-directory*
   (lambda (file-name)
     (bind ((locale-name (pathname-name file-name)))
       (with-simple-restart (continue "Skip loading locale ~A" locale-name)
         (handler-bind ((error (lambda (error)
                                 (when ignore-errors
                                   (warn "Ignoring failure while loading locale ~S" locale-name)
                                   (invoke-restart (find-restart 'continue error))))))
           (locale locale-name :use-cache use-cache)))))
   :test (lambda (file-name)
           (equal (pathname-type file-name) "xml"))))

(declaim (inline current-locale (setf current-locale)))

(defun current-locale ()
  *locale*)

(defun (setf current-locale) (locale-name)
  (setf *locale* (loop for locale :in (ensure-list locale-name)
                       collect (locale locale))))

(defmacro with-locale (locale &body body)
  `(let ((*locale* (mapcar 'locale (ensure-list ,locale))))
     ,@body))

(defun load-default-locale ()
  (flet ((try (env-name)
           (let ((locale-name (getenv env-name)))
             (when locale-name
               (awhen (position #\. locale-name)
                 (setf locale-name (subseq locale-name 0 it)))
               (when (member locale-name '("C" "posix" "POSIX") :test #'string=)
                 (setf locale-name "en_US_POSIX"))
               (locale locale-name :otherwise nil)))))
    (let ((locale (or (try "CL_LOCALE")
                      (try "LC_CTYPE")
                      (try "LANG")
                      (locale "en_US_POSIX"))))
      (setf (current-locale) locale))))

(defun load-root-locale ()
  (setf *root-locale* (locale "root")))

(eval-when (:load-toplevel :execute)
  (load-root-locale)
  (load-default-locale))


#+nil
(defun create-number-fmt-string (locale no-ts)
  ;; TODO: Quick workaround for buggy format in openmcl which prints the
  ;; commachar even when the : modifier is not present.
  #+openmcl (when no-ts (return-from create-number-fmt-string "~A~D~{~A~}"))
  (cl:format nil "~~A~~,,'~A,~A~A~~{~~A~~}" 
             (thousands-sep-char (locale-thousands-sep locale))
             (if (minusp (locale-grouping locale)) 3 (max 1 (locale-grouping locale)))
             (if no-ts "D" ":D")))

#+nil
(defun create-money-fmt-string (locale no-ts minusp)
  (multiple-value-bind (sep-by-space prec spos sign) 
      (get-descriptors minusp locale)
    (let ((sym-sep (if (zerop sep-by-space) "" " ")))
      (with-output-to-string (stream)
        ;; sign and sign separator
        (when (or* (= spos 0 1 3))
          (princ (if (zerop spos) "(" sign) stream)
          (when (= 2 sep-by-space)
            (princ #\Space stream)))
        ;; Sym and seperator
        (princ "~A" stream)
        (when prec
          (princ sym-sep stream))
        ;; Actual number
        ;; TODO: workaround for buggy format in openmcl
        ;; (see create-number-fmt-string above)
        #+openmcl (when no-ts (write-string "~D~{~A~}" stream))
        (unless #+openmcl no-ts #-openmcl nil
                (cl:format stream "~~,,'~A,~A~A~~{~~A~~}"
                           (thousands-sep-char (locale-mon-thousands-sep locale))
                           (if (minusp (locale-mon-grouping locale)) 3 (locale-mon-grouping locale))
                           (if no-ts "D" ":D")))
        (unless prec
          (princ sym-sep stream))
        (princ "~A" stream)
        (when (or* (= spos 0 2 4))
          (when (= 2 sep-by-space)
            (princ #\Space stream))
          (princ (if (zerop spos) ")" sign) stream))))))

#+nil
(defun add-printers (locale)
  "Creates monetary and numeric format strings for locale LOCALE."
  (when (and (get-category locale "LC_MONETARY")
             (get-category locale "LC_NUMERIC"))
    ;; otherwise its an include locale (tranlit* etc)
    (setf (printers locale)
          (nconc (list :number-no-ts
                       (create-number-fmt-string locale t))
                 (list :number-ts
                       (create-number-fmt-string locale nil))
                 (list :money-p-no-ts
                       (create-money-fmt-string locale t nil))
                 (list :money-p-ts
                       (create-money-fmt-string locale nil nil))
                 (list :money-n-no-ts
                       (create-money-fmt-string locale t t))
                 (list :money-n-ts
                       (create-money-fmt-string locale nil t))
                 (printers locale)))))


