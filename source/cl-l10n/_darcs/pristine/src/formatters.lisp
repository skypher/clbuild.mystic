;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package :cl-l10n)

(declaim (inline keyword-to-ldml))
(defun keyword-to-ldml (symbol)
  (case symbol
    (:short  'ldml:short)
    (:medium 'ldml:medium)
    (:long   'ldml:long)
    (:full   'ldml:full)))

(defun %format-iterating-locales (stream locale-visitor fallback-fn)
  (declare (optimize speed)
           (type function locale-visitor fallback-fn))
  (bind ((to-string? nil))
    (cond
      ((null stream)
       (setf stream (make-string-output-stream))
       (setf to-string? t))
      ((eq stream t)
       (setf stream *standard-output*)))
    (block iterating-locales
      (do-current-locales locale
        (when (funcall locale-visitor stream locale)
          (return-from iterating-locales)))
      (funcall fallback-fn stream))
    (if to-string?
        (get-output-stream-string stream)
        stream)))

;; TODO this should be cleaned up and finished once local-time settles down on how to represent dates and time of day.
;; for now format-date happily format timestamps and understands time format directives when passed in a custom pattern.
(defun format-date (stream date &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (ecase calendar
    (gregorian-calendar (format-date/gregorian-calendar stream date :verbosity verbosity :pattern pattern))))

(defun format-time (stream time &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (not-yet-implemented))

(defun format-timestamp (stream timestamp &key (verbosity 'ldml:medium) pattern (calendar 'gregorian-calendar))
  (unless (symbolp calendar)
    (setf calendar (type-of calendar)))
  (not-yet-implemented))

(defun format-date/gregorian-calendar (stream date &key (verbosity 'ldml:medium) pattern)
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (if pattern
       (lambda (stream locale)
         (declare (ignore locale))
         (funcall (etypecase pattern
                    (string
                     ;; NOTE: this code path is about 5 times slower and conses about 10 times more...
                     ;; OPTIMIZATION: we could implement some per-locale caching here, but it must be
                     ;; carefully keyed (a compiled lambda captures stuff at compile time from the compile time value of *locale*)
                     ;; and the cache must be properly locked to support threading.
                     (compile-date-time-pattern/gregorian-calendar pattern))
                    (function pattern))
                  stream date)
         t)
       (named-lambda date-format-locale-visitor (stream locale)
         (when-bind gregorian-calendar (gregorian-calendar-of locale)
           (bind ((formatter-entry (getf (date-formatters-of gregorian-calendar) verbosity))
                  (formatter (getf formatter-entry :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream date)
                   t)
                 nil)))))
   (named-lambda date-format-fallback (stream)
     (warn "No Gregorian calendar date formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           verbosity (current-locale))
     (local-time:format-timestring stream date :format '((:year 4) #\- (:month 2) #\- (:day 2))))))

(defun format-time/gregorian-calendar (stream timestamp &key (verbosity 'ldml:medium) pattern)
  (declare (ignore stream timestamp))
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (not-yet-implemented))

(defun format-timestamp/gregorian-calendar (stream timestamp &key (verbosity 'ldml:medium) pattern)
  (declare (ignore stream timestamp))
  (check-type pattern (or null string function))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (not-yet-implemented))

(defun format-number/currency (stream number currency-code &key (verbosity 'ldml:medium) pattern)
  (when pattern
    (not-yet-implemented "Custom pattern support for FORMAT-NUMBER/CURRENCY"))
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (named-lambda currency-format-visitor (stream locale)
     (awhen (currency-formatter-of locale)
       (awhen (pattern-verbosity-list-of it)
         (awhen (or (getf it verbosity)
                    (getf it nil))
           (bind ((formatter (getf it :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream number currency-code)
                   t)
                 nil))))))
   (named-lambda currency-format-fallback (stream)
     (warn "No currency formatter was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           verbosity (current-locale))
     (cl:format stream "~A ~A" number currency-code))))

(defun %format-number-iterating-locales (stream number verbosity
                                         pattern pattern-compiler
                                         formatter-accessor formatter-name fallback-format-pattern)
  (setf verbosity (or (keyword-to-ldml verbosity) verbosity))
  (%format-iterating-locales
   stream
   (if pattern
       (lambda (stream locale)
             (declare (ignore locale))
             (funcall (etypecase pattern
                        (string
                         ;; NOTE: this code path is about 10 times slower and conses about 10 times more...
                         ;; OPTIMIZATION: we could implement some per-locale caching here, but it must be
                         ;; carefully keyed (a compiled lambda captures stuff at compile time from the compile time value of *locale*)
                         ;; and the cache must be properly locked to support threading.
                         (funcall pattern-compiler pattern))
                        (function pattern))
                      stream number)
             t)
       (named-lambda number-format-visitor (stream locale)
         (awhen (or (getf (funcall formatter-accessor locale) verbosity)
                    (getf (funcall formatter-accessor  locale) nil))
           (bind ((formatter (getf it :formatter)))
             (if formatter
                 (progn
                   (funcall formatter stream number)
                   t)
                 nil)))))
   (named-lambda number-format-fallback (stream)
     (warn "No ~A was found with verbosity ~S for locale ~A. Ignoring the locale and printing in a fixed simple format."
           formatter-name verbosity (current-locale))
     (cl:format stream fallback-format-pattern number))))

(defun format-number/decimal (stream number &key (verbosity 'ldml:medium) pattern)
  (%format-number-iterating-locales stream number verbosity
                                    pattern 'compile-number-pattern/decimal
                                    #'decimal-formatters-of "decimal number formatter" "~A"))

(defun format-number/percent (stream number &key (verbosity 'ldml:medium) pattern)
  (%format-number-iterating-locales stream number verbosity
                                    pattern 'compile-number-pattern/percent
                                    #'percent-formatters-of "percent number formatter" "~A%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customized format directives

(define-constant +directive-replacements+ '((#\N . "/cl-l10n:%format-number.decimal/")
                                            (#\Y . "/cl-l10n:%format-number.percent/")
                                            (#\L . "/cl-l10n:%format-date/")
                                            (#\M . "/cl-l10n:%format-time/")
                                            (#\U . "/cl-l10n:%format-timestamp/")
                                            ;; currency support is pretty hopeless here because it needs the currency name as an argument,
                                            ;; but the format syntax only allows numeric or character arguments...
                                            )
  :test 'equal)

(define-compiler-macro format (&whole form destination format-control &rest format-arguments)
  "Compiler macro to remove unnecessary calls to parse-format-string."
  (if (stringp format-control)
      `(cl:format ,destination ,(parse-format-string format-control) ,@format-arguments)
      form))

(defmacro formatter (format-string)
  (etypecase format-string
    (string `(cl:formatter ,(parse-format-string format-string)))))

(defun format (stream format-control &rest format-arguments)
  (apply #'cl:format stream
         (etypecase format-control
           (function format-control)
           (string (parse-format-string format-control)))
         format-arguments))

(defun shadow-format (&optional (package *package*))
  "Shadowing import the CL-L10N:FORMAT symbol into PACKAGE."
  (shadowing-import '(cl-l10n::format cl-l10n::formatter) package))

(defun %format-number.decimal (stream number colon-modifier? at-modifier?)
  (bind ((print-decimal-point? (not colon-modifier?))
         (print-thousand-separator? (not at-modifier?)))
    (unless print-decimal-point?
      (not-yet-implemented "Turning off the decimal point"))
    (unless print-thousand-separator?
      (not-yet-implemented "Turning off thousand separators"))
    (format-number/decimal stream number))
  (values))

(defun %format-number.percent (stream number colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  (format-number/percent stream number)
  (values))

(defun %format-date (stream date colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  (format-date/gregorian-calendar stream date)
  (values))

(defun %format-time (stream time colon-modifier? at-modifier?)
  (declare (ignore colon-modifier? at-modifier?))
  (format-time/gregorian-calendar stream time)
  (values))

(defun %format-timestamp (stream timestamp colon-modifier? at-modifier?)
  (bind ((show-timezone? (not colon-modifier?))
         (in-utc-zone? at-modifier?)
         (local-time:*default-timezone* (if in-utc-zone? local-time:+utc-zone+ local-time:*default-timezone*)))
    (declare (ignore show-timezone?))
    ;; TODO implement show-timezone?, or maybe just use colon-modifier? as a minimal control on verbosity...
    (format-timestamp/gregorian-calendar stream timestamp))
  (values))

(defun parse-format-string (string)
  (declare (optimize speed)
           (type string string))
  (flet ((needs-parsing? (string)
           (cl-ppcre:scan (load-time-value (cl-ppcre:create-scanner
                                            (cl:format nil "~~[@V,:\\d]*[~{~A~^|~}]" (mapcar 'first +directive-replacements+))))
                          (string-upcase string)))
         (really-parse-format-string (string)
           (declare (optimize speed)
                    (type simple-string string))
           (flet ((get-replacement (char)
                    (or (when (typep char 'base-char)
                          (cdr (assoc (char-upcase (the base-char char))
                                      +directive-replacements+)))
                        char)))
             (declare (inline get-replacement))
             (bind ((*print-pretty* nil)
                    (*print-circle* nil))
               (with-output-to-string (result)
                 (loop
                    :for char :across string
                    :with tilde = nil
                    :do (case char
                          ((#\@ #\v #\, #\:)
                           (princ char result))
                          (#\~
                           (princ char result)
                           (if tilde
                               (setf tilde nil)
                               (setf tilde t)))
                          (t
                           (if tilde
                               (if (or (digit-char-p char)
                                       (member char '(#\' #\,)))
                                   (princ char result)
                                   (progn
                                     (setf tilde nil)
                                     (princ (get-replacement char) result)))
                               (princ char result))))))))))
    (if (needs-parsing? string)
        (really-parse-format-string (coerce string 'simple-string))
        string)))
