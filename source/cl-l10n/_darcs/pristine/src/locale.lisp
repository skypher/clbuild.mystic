;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n )

;; Conditions
(define-condition locale-error (simple-error)
  ())

(defun locale-error (message &rest args)
  (error 'locale-error :format-control message :format-arguments args))

;; Classes
(defclass locale ()
  ((language
    :initform (required-arg :language)
    :initarg :language
    :accessor language-of)
   (script
    :initform nil
    :initarg :script
    :accessor script-of)
   (territory
    :initform nil
    :initarg :territory
    :accessor territory-of)
   (variant
    :initform nil
    :initarg :variant
    :accessor variant-of)
   (precedence-list
    :initform nil
    :initarg :precedence-list
    :accessor precedence-list-of)
   (version-info
    :initform nil
    :initarg :version-info
    :accessor version-info-of)
   (number-symbols
    :initform (list)
    :accessor number-symbols-of)
   (currencies
    :initform (make-hash-table :test #'eq)
    :accessor currencies-of)
   (currency-formatter
    :initform nil
    :initarg :currency-formatter
    :accessor currency-formatter-of)
   (languages
    :initform (make-hash-table :test #'eq)
    :accessor languages-of)
   (scripts
    :initform (make-hash-table :test #'eq)
    :accessor scripts-of)
   (territories
    :initform (make-hash-table :test #'eq)
    :accessor territories-of)
   (variants
    :initform (make-hash-table :test #'eq)
    :accessor variants-of)
   (gregorian-calendar
    :initform nil
    :initarg :gregorian-calendar
    :accessor gregorian-calendar-of)
   (resources
    :initform (make-hash-table :test #'equal)
    :accessor resources-of)
   (initialized
    :initform nil
    :accessor initialized-p)
   (decimal-formatters
    :initform nil
    :accessor decimal-formatters-of)
   (percent-formatters
    :initform nil
    :accessor percent-formatters-of)))

(defmethod print-object ((obj locale) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (princ (locale-name obj) stream)))

(defgeneric locale-name (locale &key ignore-script ignore-territory ignore-variant)
  (:method ((locale locale) &key ignore-variant ignore-territory ignore-script)
    (let ((*print-pretty* nil))
      (with-output-to-string (*standard-output*)
        (write-string (language-of locale))
        (unless ignore-script
          (awhen (script-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-territory
          (awhen (territory-of locale)
            (write-char #\_)
            (write-string it)))
        (unless ignore-variant
          (awhen (variant-of locale)
            (write-char #\_)
            (write-string it)))))))

(defun compute-locale-precedence-list (locale)
  "Calculate the precedence list for a locale that should be searched for definitions. For example: (locale-precedence-list (locale \"en_US_POSIX\")) => (en_US_POSIX en_US en root)"
  (let ((result (list locale)))
    (flet ((try (locale-name)
             (awhen (locale locale-name :otherwise nil)
               (push it result))))
      (when (variant-of locale)
        (try (locale-name locale
                          :ignore-variant t)))
      (when (territory-of locale)
        (try (locale-name locale
                          :ignore-territory t
                          :ignore-variant t)))
      (when (script-of locale)
        (try (locale-name locale
                          :ignore-script t
                          :ignore-territory t
                          :ignore-variant t))))
    (when (boundp '*root-locale*)
      (push *root-locale* result))
    (nreverse result)))

(defun clear-locale-cache ()
  (prog1
      (hash-table-count *locale-cache*)
    (clrhash *locale-cache*)
    (makunbound '*root-locale*)
    (load-root-locale)
    (load-default-locale)))
