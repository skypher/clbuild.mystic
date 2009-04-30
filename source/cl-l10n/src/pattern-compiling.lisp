;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n)

;;; http://www.unicode.org/reports/tr35/tr35-11.html#Date_Format_Patterns
(define-constant +date-pattern-characters/gregorian-calendar+ "GyYuQqMLlwWdDFgEec" :test #'string=)
(define-constant +time-pattern-characters/gregorian-calendar+ "ahHKkjmsSAzZvV" :test #'string=)

(define-constant +number-pattern-characters+ "@#.-,E+;%‰¤*()0123456789" :test #'string=)

(defun create-scanner-from-character-list (scanner-characters &optional appended-regexp)
  (cl-ppcre:create-scanner (coerce (append (iter (for char :in-sequence scanner-characters)
                                                 (unless (first-time-p)
                                                   (collect #\|))
                                                 (nconcing (list #\( char #\+ #\))))
                                           appended-regexp)
                                   'simple-string)))

(defparameter +date-time-pattern-scanner/gregorian-calendar+
  (create-scanner-from-character-list (concatenate 'string
                                                   +date-pattern-characters/gregorian-calendar+
                                                   +time-pattern-characters/gregorian-calendar+)))

;;; at the time these functions are called *locale* is bound the the locale for which the pattern should be compiled for

(defun tokenize-format-pattern (pattern scanner)
  (bind ((quote-pieces (remove-if #'zerop
                                  (cl-ppcre:split "(')" pattern :with-registers-p t :omit-unmatched-p t)
                                  :key #'length)))
    (iter outer
          (generate (piece following-piece) :on quote-pieces)
          (next piece)
          (when (string= piece "")
            (next-iteration))
          (cond
            ((and (string= piece "'")
                  (string= following-piece "'"))
             (next piece)
             (collect "'"))
            ((string= piece "'")
             (collect (apply #'concatenate 'string
                             (iter inner
                                   (in outer (next piece))
                                   (until (and (string= piece "'")
                                               (not (string= following-piece "'"))))
                                   (if (and (string= piece "'")
                                            (string= following-piece "'"))
                                       (progn
                                         (in outer (next piece))
                                         (collect "'"))
                                       (collect piece))))))
            ((not (zerop (length piece)))
             (collect (cl-ppcre:split scanner piece :with-registers-p t :omit-unmatched-p t)))))))

(defun parse-pattern-prefix (a-pattern &optional terminating-characters invalid-characters)
  (bind ((next-char nil)
         (quoting? nil)
         (prefix nil)
         (split-at nil))
    (iter (generate i from 0 below (- (length a-pattern) 0))
          (next i)
          (setf next-char (elt a-pattern i))
          (if (and (char= next-char #\')
                   (> (- (length a-pattern) 1) i)
                   (char= (elt a-pattern (+ i 1)) #\'))
              (progn
                (push next-char prefix)
                (next i))
              (if quoting?
                  (if (char= next-char #\')
                      (setf quoting? nil)
                      (push next-char prefix))
                  (if (char= next-char #\')
                      (setf quoting? t)
                      (if (find next-char terminating-characters :test #'char=)
                          (finish)
                          ;; end of prefix
                          (if (find next-char invalid-characters)
                              (error "The character ~A inside a prefix/suffix is invalid, try enclosing it in 'quotes'." next-char)
                              (push next-char prefix))))))
          (finally
           (unless (null prefix)
             (setf prefix
                   (coerce
                    (nreconc prefix ())
                    'string)))
           (setf split-at i)))
    (values-list (list prefix (subseq a-pattern split-at)))))

(defun parse-pattern-padding (a-pattern)
  (bind ((pad nil))
    (cl-ppcre:register-groups-bind (nil the-quote quoted-directive ordinary-char) ("(^\\*'(')|^\\*'([^'])'|^\\*([^']))" a-pattern)
      (cond
        (the-quote
         (setf pad the-quote)
         (setf a-pattern (subseq a-pattern 3)))
        (quoted-directive
         (setf pad quoted-directive)
         (setf a-pattern (subseq a-pattern 4)))
        (ordinary-char
         (if (find ordinary-char +number-pattern-characters+)
             (error "Invalid padding character: '~A'. The specified padding character is special, try enclosing it in 'quotes'." ordinary-char)
             (progn
               (setf pad ordinary-char)
               (setf a-pattern (subseq a-pattern 2)))))
        (t (error "No padding character specified after * pad escape directive."))))
    (values pad a-pattern)))

;; TODO scientific and significant digit count format not implemented yet
(defun compile-number-absolute-value-pattern/decimal (number-format)
  (bind ((integer-fraction-with-dot-? (find #\. number-format :test #'char=))
         (significant-digit-count-? (find #\@ number-format :test #'char=)))
    (when (and integer-fraction-with-dot-?
               significant-digit-count-?)
      (error "Significant digit count number format (@) and integer/fraction digit format (.) cannot be used simultaneously."))
    (if significant-digit-count-?
        (error "Not implemented yet!")
        ;;integer/fraction format
        (cl-ppcre:register-groups-bind (integer-part fraction-part nil) ("^([^\\.]*)\\.?(.*)$|(.?)" number-format)
          (cl-ppcre:register-groups-bind (rounding-integer-number-part nil) ("(\\d*)$(.?)" (cl-ppcre:regex-replace-all "\\D" integer-part ""))
            (cl-ppcre:register-groups-bind (rounding-fraction-number-part nil) ("^(\\d*)(.?)" (cl-ppcre:regex-replace-all "\\D" fraction-part ""))
              (cl-ppcre:register-groups-bind (head (#'length primary-grouping-size) nil) ("(.*),([^,]*)$|(.?)" integer-part)
                (cl-ppcre:register-groups-bind ((#'length secondary-grouping-size) nil) (".*,([^,]*)$|(.?)" head)
                  (flet ((nil-if-zero (value) (if (zerop value) nil value)))
                    (bind (((:values rounding-increment rounding-fraction-length)
                            (aif (nil-if-zero (bind ((integer-part (if rounding-integer-number-part
                                                                       (parse-integer rounding-integer-number-part)
                                                                       0))
                                                     (fraction-part (unless (zerop (length rounding-fraction-number-part))
                                                                      (parse-integer rounding-fraction-number-part))))
                                                (if fraction-part
                                                    (coerce (+ integer-part (/ fraction-part
                                                                               (expt 10 (length rounding-fraction-number-part))))
                                                            'long-float)
                                                    integer-part)))
                                 (values it (length rounding-fraction-number-part))
                                 (aif (nil-if-zero (length fraction-part))
                                      (values (expt 10 (* -1 it)) it)
                                      (values 1 0))))
                           (minimum-digits (bind ((integer-part-without-grouping (remove #\, integer-part)))
                                             (aif (position-if #'digit-char-p integer-part-without-grouping)
                                                  (- (length integer-part-without-grouping) it)
                                                  0))))
                      (lambda (number)
                        (declare (inline digit-char)
                                 (optimize speed))
                        (setf number (abs number))
                        (bind ((formatted-digits (list))
                               ;; the next two lookups could be moved to compile time if all formatters
                               ;; were compiled for all locales, IOW doing the inheritance at
                               ;; compile time. TODO?
                               (localized-decimal-separator (localize-number-symbol-character #\.))
                               (localized-thousand-separator (localize-number-symbol-character #\,))
                               ;; caution: there are rounding errors with floating point arithmetics
                               (rounded-integer-part
                                (truncate (* rounding-increment (round (/ number rounding-increment)))))
                               (rounded-fraction-part
                                (* rounding-increment (round (/ (- number (truncate number)) rounding-increment)))))
                          ;; fraction part
                          (macrolet ((emit (stuff)
                                       (once-only (stuff)
                                         `(etypecase ,stuff
                                            (character
                                             (push ,stuff formatted-digits))
                                            (sequence
                                             (setf formatted-digits (nconc (nreverse (coerce ,stuff 'list))
                                                                           formatted-digits)))))))
                            ;; fraction part
                            (bind ((fraction-digits (list)))
                              ;; collect a reverse list of fraction digits
                              (iter (with rest = rounded-fraction-part)
                                    (with digit)
                                    (repeat rounding-fraction-length)
                                    (setf (values digit rest) (truncate (* rest 10)))
                                    (push digit fraction-digits))
                              ;; pop leading zeros
                              (iter (while fraction-digits)
                                    (for digit = (first fraction-digits))
                                    (while (zerop digit))
                                    (pop fraction-digits))
                              (when fraction-digits
                                (setf formatted-digits
                                      (iter (for digit :in fraction-digits)
                                            (for localized-digit = (localize-number-symbol-character (digit-char digit)))
                                            (etypecase localized-digit
                                              (character (collect localized-digit :at beginning))
                                              (string (nconcing (coerce localized-digit 'list) :at beginning)))))
                                (emit localized-decimal-separator)))
                            ;; integer part
                            (iter
                              (with grouping-size = (or primary-grouping-size 0))
                              (with remainder = rounded-integer-part)
                              (with number-of-digits = 0)
                              (with group)
                              (until (and (zerop remainder) (>= number-of-digits minimum-digits)))
                              (if (zerop grouping-size)
                                  (progn
                                    (setf group remainder)
                                    (setf remainder 0))
                                  (setf (values remainder group) (truncate remainder (expt 10 grouping-size))))
                              (iter
                                (with digit)
                                (with count = 0)
                                (until (or (and (plusp grouping-size)
                                                (or (>= count grouping-size)
                                                    (and (zerop remainder)
                                                         (zerop group)
                                                         (>= number-of-digits minimum-digits))))
                                           (and (zerop grouping-size)
                                                (zerop group)
                                                (>= number-of-digits minimum-digits))))
                                (setf (values group digit) (truncate group 10))
                                (emit (localize-number-symbol-character (digit-char digit)))
                                (incf number-of-digits)
                                (incf count))
                              (when (and (> grouping-size 0)
                                         (or (not (zerop remainder))
                                             (< number-of-digits minimum-digits)))
                                (emit localized-thousand-separator))
                              (when (and (first-time-p)
                                         (not (or (null secondary-grouping-size)
                                                  (zerop secondary-grouping-size))))
                                (setf grouping-size secondary-grouping-size))))
                          (coerce formatted-digits 'string)))))))))))))

(defun compile-number-pattern/decimal (pattern)
  (bind ((pos-subpat-prefix nil)
         (pos-subpat-suffix nil)
         (neg-subpat-prefix nil)
         (neg-subpat-suffix nil)
         (pad-char nil)
         (pad-pos nil)
         (number-format-size nil)
         (number-formatter nil))
    (macrolet ((handle-padding-if-applicable (position)
                 `(bind (((:values padding tail) (parse-pattern-padding pattern)))
                    (when padding
                      (when pad-char
                        (error "Padding cannot be specified more than once."))
                      (setf pad-pos ,position)
                      (setf pad-char (elt padding 0)))
                    (setf pattern tail))))
      ;; pad before prefix
      (handle-padding-if-applicable 'before-prefix)

      ;; prefix
      (setf (values pos-subpat-prefix pattern) (parse-pattern-prefix pattern "*@#0123456789" ".,;"))

      ;; pad after prefix
      (handle-padding-if-applicable 'after-prefix)

      (if (zerop (length pattern))
          (error "No number format could be found."))

      ;;number formatting
      (cl-ppcre:register-groups-bind (number-format tail) ("^([@#,.0123456789]*)(.*)$" pattern)
        (setf number-formatter (compile-number-absolute-value-pattern/decimal number-format) )
        (setf number-format-size (length number-format))
        (setf pattern tail))

      ;; pad before suffix
      (handle-padding-if-applicable 'before-suffix)

      ;;positive subpattern suffix
      (setf (values pos-subpat-suffix pattern) (parse-pattern-prefix pattern ";*" ".,"))

      ;; pad after suffix
      (handle-padding-if-applicable 'after-suffix)

      ;; negative subpattern
      (setf pattern
            (string-right-trim ")" (string-left-trim ";(" pattern)))

      ;;negative subpattern prefix
      (setf (values neg-subpat-prefix pattern) (parse-pattern-prefix pattern "@#0123456789" ",."))

      (setf pattern (string-left-trim "@#,.0123456789" pattern))

      ;;negative subpattern suffix
      (setf (values neg-subpat-suffix pattern) (parse-pattern-prefix pattern))

      (when (and
             (or (null neg-subpat-suffix)
                 (zerop (length neg-subpat-suffix)))
             (or (null neg-subpat-prefix)
                 (zerop (length neg-subpat-prefix))))
          (setf neg-subpat-prefix (concatenate 'string pos-subpat-prefix "-"))
          (setf neg-subpat-suffix pos-subpat-suffix))

      (lambda (stream number)
        (bind ((prefix (if (minusp number) neg-subpat-prefix pos-subpat-prefix))
               (suffix (if (minusp number) neg-subpat-suffix pos-subpat-suffix))
               (formatted-number (funcall number-formatter number))
               (padding (when pad-pos
                          (coerce (iter (repeat (- number-format-size (+ (length formatted-number) (length prefix) (length suffix))))
                                        (collect pad-char)) 'string))))
          (when (eq pad-pos 'before-prefix)
            (write-string padding stream))
          (when prefix
            (write-string prefix stream))
          (when (eq pad-pos 'after-prefix)
            (write-string padding stream))
          (when formatted-number
            (write-string formatted-number stream))
          (when (eq pad-pos 'before-suffix)
            (write-string padding stream))
          (when suffix
            (write-string suffix stream))
          (when (eq pad-pos 'after-suffix)
            (write-string padding stream)))))))

(defun compile-number-pattern/percent (pattern)
  ;; TODO localize percent
  (bind ((pattern (replace-percent-considering-quotes pattern "%"))
         (formatter (compile-number-pattern/decimal pattern)))
    (lambda (stream number)
      (funcall formatter stream (* number 100)))))


;;;;;;
;;; date and time formatter compilers. NOTE: i (attila) did not invest enough time in reading
;;; the CLDR spec, so some of the code down here may be off a little bit on naming convention
;;; and stuff like that. i've tried to mark every place where i know there's spece for improvement...

(defun compile-date-time-pattern/gregorian-calendar (pattern)
  (first (compile-date-time-patterns/gregorian-calendar (list pattern))))

(defun compile-date-time-patterns/gregorian-calendar (patterns)
  (declare (optimize speed))
  (macrolet ((piece-formatter (&body body)
               `(lambda (stream date year month day day-of-week hour minute second nano-second)
                  (declare (ignorable date year month day day-of-week hour minute second nano-second)
                           (type non-negative-fixnum year month day day-of-week hour minute second nano-second))
                  (bind ((month-1 (1- month))
                         (day-1 (1- day)))
                    (declare (ignorable date month-1 day-1 day-of-week)
                             (type non-negative-fixnum month-1 day-1 day-of-week))
                    ,@body)))
             (era-formatter (vector)
               `(piece-formatter
                 (bind ((era (if (< year 0) 0 1)))
                   (write-string (aref ,vector era) stream))))
             (collect (part)
               `(push ,part piece-formatters))
             (invalid-number-of-directives ()
               `(error "Invalid number of consecutive '~A' directives in Gregorian calendar date format: \"~A\", piece \"~A\""
                       directive-character pattern piece)))
    ;; TODO these effective vectors should be cached on the locale instance because they don't use much memory, but speed up pattern compilation
    ;; TODO implement the more fine-grained inheritance rules described in http://www.unicode.org/reports/tr35/tr35-11.html#Date_Format_Patterns
    (bind ((day-names               (effective-date-related-names/gregorian-calendar 'day-names-of #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")))
           (abbreviated-day-names   (effective-date-related-names/gregorian-calendar 'abbreviated-day-names-of #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
           (narrow-day-names        (effective-date-related-names/gregorian-calendar 'narrow-day-names-of #("S" "M" "T" "W" "T" "F" "S")))
           (month-names             (effective-date-related-names/gregorian-calendar 'month-names-of #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")))
           (abbreviated-month-names (effective-date-related-names/gregorian-calendar 'abbreviated-month-names-of #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
           (narrow-month-names      (effective-date-related-names/gregorian-calendar 'narrow-month-names-of #("J" "F" "M" "A" "M" "J" "J" "A" "S" "O" "N" "D")))
           (era-names               (effective-date-related-names/gregorian-calendar 'era-names-of #("BC" "AD")))
           (abbreviated-era-names   (effective-date-related-names/gregorian-calendar 'abbreviated-era-names-of #("BC" "AD")))
           (narrow-era-names        (effective-date-related-names/gregorian-calendar 'narrow-era-names-of #("BC" "AD")))
           (formatters (list)))
      (dolist (pattern patterns)
        (bind ((piece-formatters (list)))
          (dolist (outer-piece (tokenize-format-pattern pattern +date-time-pattern-scanner/gregorian-calendar+))
            (unless (zerop (length outer-piece))
              ;;(format *debug-io* "processing outer piece ~S~%" outer-piece)
              (if (consp outer-piece)
                  (dolist (piece outer-piece)
                    (bind ((length (length piece)))
                      (unless (zerop length)
                        ;;(format *debug-io* "  processing inner piece ~S~%" piece)
                        (bind ((directive-character (char piece 0)))
                          (flet ((process-hour-directive (&key offset modulo)
                                   (if (or (= length 1)
                                           (= length 2))
                                       (collect (piece-formatter (bind ((hour (if modulo
                                                                                  (mod hour modulo)
                                                                                  hour)))
                                                                   (when offset
                                                                     (incf hour offset))
                                                                   (write-decimal-digits stream hour
                                                                                         :minimum-digit-count length))))
                                       (invalid-number-of-directives))))
                            (declare (inline process-hour-directive))
                            (switch (directive-character :test #'char=)
                              (#\y (cond
                                     ((= length 1)
                                      (collect (piece-formatter (write-decimal-digits stream year))))
                                     ((= length 2)
                                      (collect (piece-formatter (write-decimal-digits stream year :maximum-digit-count 2))))
                                     (t (collect (piece-formatter (write-decimal-digits stream year :minimum-digit-count length))))))
                              (#\M (cond
                                     ((= length 3)
                                      (collect (piece-formatter (write-string (aref abbreviated-month-names month-1) stream))))
                                     ((= length 4)
                                      (collect (piece-formatter (write-string (aref month-names month-1) stream))))
                                     ((= length 5)
                                      (collect (piece-formatter (write-string (aref narrow-month-names month-1) stream))))
                                     ((<= length 2)
                                      (collect (piece-formatter (write-decimal-digits stream month :minimum-digit-count length))))
                                     (t
                                      (invalid-number-of-directives))))
                              (#\E (cond
                                     ((= length 4)
                                      (collect (piece-formatter (write-string (aref day-names day-of-week) stream))))
                                     ((= length 5)
                                      (collect (piece-formatter (write-string (aref narrow-day-names day-of-week) stream))))
                                     ((<= length 3)
                                      (collect (piece-formatter (write-string (aref abbreviated-day-names day-of-week) stream))))
                                     (t
                                      (invalid-number-of-directives))))
                              (#\G (cond
                                     ((= length 4)
                                      (collect (era-formatter era-names)))
                                     ((= length 5)
                                      (unless narrow-era-names
                                        (error "Locale ~A does not have narrow era names for the Gregorian calendar" *locale*))
                                      (collect (era-formatter narrow-era-names)))
                                     ((<= length 3)
                                      (collect (era-formatter abbreviated-era-names)))
                                     (t
                                      (invalid-number-of-directives))))
                              (#\d (unless (or (= 1 length)
                                               (= 2 length))
                                     (invalid-number-of-directives))
                                   (collect (piece-formatter (write-decimal-digits stream day :minimum-digit-count length))))
                              (#\h (process-hour-directive :offset 1 :modulo 12))
                              (#\H (process-hour-directive))
                              (#\K (process-hour-directive :modulo 12))
                              (#\k (process-hour-directive :offset 1))
                              (#\m (unless (or (= 1 length)
                                               (= 2 length))
                                     (invalid-number-of-directives))
                                   (collect (piece-formatter (write-decimal-digits stream minute :minimum-digit-count length))))
                              (#\s (unless (or (= 1 length)
                                               (= 2 length))
                                     (invalid-number-of-directives))
                                   (collect (piece-formatter (write-decimal-digits stream second :minimum-digit-count length))))
                              (#\S (bind ((rounding-divisor (expt 10 (- length))))
                                     (collect (piece-formatter
                                               (bind ((fraction (round (/ nano-second 1000000000d0) rounding-divisor)))
                                                 (write-decimal-digits stream fraction :minimum-digit-count length))))))
                              (otherwise
                               (when (find directive-character +date-pattern-characters/gregorian-calendar+ :test #'char=)
                                 (cerror "Print it unprocessed" "Unexpected or not yet implemented directive in Gregorian calendar date format: \"~A\", character ~A"
                                         pattern directive-character))
                               (collect (piece-formatter (write-string piece stream))))))))))
                  (collect (piece-formatter (write-string outer-piece stream))))))
          (nreversef piece-formatters)
          (push (named-lambda date-formatter (stream date)
                  ;; TODO should we compare the value of *locale* at compile/runtime?
                  ;; if yes, then check the other formatters, too!
                  (local-time:with-decoded-timestamp (:year year :month month :day day :day-of-week day-of-week
                                                      :hour hour :minute minute :sec second :nsec nano-second)
                      date
                    (dolist (formatter piece-formatters)
                      (funcall (the function formatter) stream date year month day day-of-week hour minute second nano-second))))
                formatters)))
      (nreverse formatters))))

(defclass currency-formatter ()
  ((before-currency
    :initform nil
    :accessor before-currency-of)
   (after-currency
    :initform nil
    :accessor after-currency-of)
   (unit-pattern
    :initform nil
    :accessor unit-pattern-of)
   (pattern-verbosity-list
    :initform nil
    :accessor pattern-verbosity-list-of)))

;; TODO for now, it's not implemented according to the cldr
(defun compile-number-pattern/currency (locale)
  (awhen (currency-formatter-of locale)
    (awhen (pattern-verbosity-list-of it)
      (iter (for verbosity :in it by #'cddr)
            (with pattern = (getf (getf it verbosity) :pattern))
            (with currency-specific-formatter = (make-hash-table))
            (setf (getf it verbosity)
                  (list :pattern pattern
                        :currency-specific-formatter currency-specific-formatter
                        :formatter
                        (lambda (stream number currency-code)
                          (assert (ldml-symbol-p currency-code))
                          (bind ((formatter (or
                                             (gethash currency-code currency-specific-formatter)
                                             (setf
                                              (gethash currency-code currency-specific-formatter)
                                              (compile-number-pattern/decimal
                                               (replace-currency-sign-considering-quotes
                                                pattern
                                                (do-current-locales locale
                                                  (awhen (gethash currency-code (currencies-of locale))
                                                    (awhen (second it)
                                                      (return it))))
                                                (symbol-name currency-code)
                                                (do-current-locales locale
                                                  (awhen (gethash currency-code (currencies-of locale))
                                                    (awhen (first it)
                                                      (return it))))))))))
                            (funcall formatter stream number)))))))))


(defmacro replace-sign-considering-quotes (pattern char-to-replace &body body)
  ;; TODO user once-only to rebind
  `(bind ((pattern ,pattern)
          (char-to-replace ,char-to-replace))
     (flet ((char-at-? (pattern index character)
              (if (and (<= 0 index) (< index (length pattern)))
                  (char= (elt pattern index) character)
                  nil)))
       (macrolet ((collect-string (string)
                    `(map 'list (lambda (c)
                                  (collect c)
                                  (if (char= c #\')
                                      (collect c))) ,string)))
         (coerce
          (iter (generating char :in-sequence pattern :with-index index)
                (with no-quote = t)
                (next char)
                (switch (char :test #'char=)
                  (char-to-replace (if no-quote
                                       (progn
                                         (unless
                                             (and
                                              (char-at-? pattern (- index 1) #\')
                                              (bind ((pattern (subseq pattern 0 index))
                                                     (match (mismatch pattern (make-string index :initial-element #\') :from-end t)))
                                                (and match (oddp (- index match)))))
                                           (collect #\'))
                                         ,@body
                                         (unless
                                             (and
                                              (char-at-? pattern (+ index 1) #\')
                                              (bind ((pattern (subseq pattern (+ index 1)))
                                                     (length (length pattern))
                                                     (match (mismatch pattern (make-string length :initial-element #\'))))
                                                (and match (oddp match))))
                                           (collect #\')))
                                       (collect char)))
                  (#\' (setf no-quote (not no-quote))
                       (unless
                           (or
                            (and no-quote
                                 (char-at-? pattern (+ index 1) char-to-replace)
                                 (bind ((pattern (subseq pattern 0 index))
                                        (match (mismatch pattern (make-string index :initial-element #\') :from-end t)))
                                   (and match (evenp (- index match)))))
                            (and (not no-quote)
                                 (char-at-? pattern (- index 1) char-to-replace)
                                 (bind ((pattern (subseq pattern index))
                                        (length (length pattern))
                                        (match (mismatch pattern (make-string length :initial-element #\'))))
                                   (and match (oddp match)))))
                         (collect #\')))
                  (otherwise (collect char))))
          'string)))))

(defun replace-percent-considering-quotes (pattern localized-percent-string)
  (replace-sign-considering-quotes pattern #\%
    (collect-string localized-percent-string)))

(defun replace-currency-sign-considering-quotes (pattern currency-symbol currency-code currency-long-name)
  (replace-sign-considering-quotes pattern #\¤
    (if (not (char-at-? pattern (+ index 1) char-to-replace))
        ;; currency symbol
        (collect-string currency-symbol)
        (progn
          (next char)
          (if (not (char-at-? pattern (+ index 1) char-to-replace))
              ;; international currency symbol (3 letter code)
              (collect-string currency-code)
              (progn
                ;; long form of decimal symbol
                (next char)
                (collect-string currency-long-name)))))))
