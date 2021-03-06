;;
;; OVERVIEW
;;
;; the functions R-UT and W-UT are for reading and writing lisp universal time as
;; strings
;;
;; universal times stored as strings using R-UT and W-UT have the following advantages
;; over normal universal times stored as integers:
;;
;;    1. they are human readable and thus less prone to bugs
;;    2. they are easier to use when interacting with external databases or other
;;       programs that expect times dates encoded as strings
;;    3. they are ISO-8601 compliant [by default]
;;
;; EXAMPLE
;;
;;    CL-USER> (defvar now (get-universal-time))
;;    NOW
;;
;;    CL-USER> now
;;    3425557791
;;
;;    CL-USER> (use-package :rw-ut)
;;    T
;;
;;    CL-USER> (w-ut now)
;;    "2008/07/20 15:49:51"
;;
;;    CL-USER> (r-ut *)
;;    3425557791
;;
;;    CL-USER> (eql * now)
;;    T
;;
;;    CL-USER> (w-ut now "MM-DD-YY")
;;    "07-20-08"
;;
;;    CL-USER> (r-ut * "MM-DD-YY")
;;    3425500800
;;
;;    CL-USER> (w-ut * "YYYY/MM/DD)
;;    "2008/07/20"
;;
;; PATTERNS
;;
;; R-UT and W-UT are designed to work with "patterns". patterns are strings that control
;; how R-UT and W-UT read and write universal time as strings.
;;
;; patterns look like the psuedocode described in the the ISO-8601-2004_E document in
;; section 3.4.2 "Characters used in place of digits or signs" [dont worry, it's also
;;  the same psuedocode used in the Wikipedia article on iso-8601].
;;
;; the #\? character introduces a "breakpoint". when writing [with W-UT], everything
;; after a breakpoint will be omitted if there is nothing non-boring after it. when
;; reading [with R-UT], everything after the breakpoint can be omitted and will be
;; assumed to be boring values
;;
;; "boring values" means 1 for month and day and 0 for everything else
;;
;; R-UT and W-UT currently understand the following patterns:
;;
;;    YYYY -- year, 4 digits
;;    YY   -- year, 2 digits
;;    MM   -- month, 2 digits
;;    M    -- month, 1-2 digits
;;    DD   -- day, 2 digits
;;    D    -- day, 1-2 digits
;;    hh   -- hour, 2 digits
;;    h    -- hour, 1-2 digits
;;    mm   -- minute, 2 digits
;;    m    -- minute, 1-2 digits
;;    ss   -- second, 2 digits
;;    s    -- second, 1-2 digits
;;
;; the #\\ char is the escape char, use it to read or write things that look like
;; patterns but aren't
;;
;; BREAK POINTS [the ?]
;;
;; anything after a #\? can be "omitted", since R-UT and W-UT are lazy and don't like to
;; deal with minumum values [1 for dates and months, 0 for everything else] if the don't
;; have to
;;
;;    RW-UT> (w-ut (r-ut "2008")
;; 	        "YYYY-MM-DD")
;;    "2008-01-01"
;;
;;    RW-UT> (w-ut (r-ut "2008")
;; 	        "YYYY-MM?-DD")
;;    "2008-01"
;;
;;    RW-UT> (w-ut (r-ut "2008")
;; 	        "YYYY?-MM?-DD")
;;    "2008"
;;
;; THE DEFAULT PATTERN
;;
;; The default pattern for both R-UT and W-UT is
;;
;;    YYYY?/MM?/DD? hh?:mm?:ss
;;
;; TOLERANCE
;;
;; The R-UT will accept two kinds of junk:
;;
;;    1. difference literal characters that don't effect the length of the string. for
;;       example, the default pattern can read strings that look like this:
;;
;;          YYYY-MM-DD hh:mm:ss
;;
;;       but it can also read strings that look like this:
;;
;;           YYYY-MM-DDThh:mm:ss
;;
;;       [note that numberic characters in the wrong spot will probably screw things up
;;        because it looks like part of the date]
;;      
;;     2. there can be junk at the _end_ of the string. so the default pattern can read
;;        string that look like this:
;;
;;           YYYY-MM-DD hh:mm:ss
;;
;;        but it can also read strings that look like this:
;;
;;           YYYY-MM-DD hh:mm:ss.0000Z
;;
;; API
;;
;; function
;; R-UT (string &optional (pattern "YYYY-MM-DD hh:mm:ss"))
;;
;;    -- reads `STRING' according to the pattern string `PATTERN' and returns a
;;       universal time
;;
;; function
;; W-YT (ut &optional (pattern "YYYY-MM-DD hh:mm:ss"))
;;
;;    -- write the universal `UT' as a string according to the pattern string `PATTERN'
;;
;; TODO
;;
;;    -tests
;;    -names of months and days of week
;;    -12 hour clock, a.m./p.m.
;;    -optimize

(defpackage :rw-ut
  (:use :cl)
  (:export ; using
           :*time-zone*
	   :r-ut
	   :w-ut

	   ; compiling fast readers or writers at runtime
	   :ut-pattern->src/read
	   :ut-pattern->src/write))

(in-package :rw-ut)

; special

(defvar *time-zone* 0) ; by default assumes GMT/UTC

(defconstant +default-ut-pattern+
  (if (boundp '+default-ut-pattern+) ; SBCL complains at normal DEFCONSTANT...
      (symbol-value '+default-ut-pattern+)
      "YYYY?/MM?/DD? hh?:mm?:ss"))

; util

(defun .get-match (string pattern-table)
"
   (.get-match \"01234\" '((\"0123\" . xx)))

   -> xx, 4

   (.get-match \"abcde\" '((\"0123\" . xx)))

   -> NIL
"
  (dolist (% pattern-table)
    (destructuring-bind (pattern . xx) %
      (let ((pattern-length (length pattern))
	    (mismatch (mismatch string pattern)))
	(if (or (null mismatch)
		(eql mismatch pattern-length))
	    (return-from .get-match (values xx pattern-length)))))))

(defun .parse-string (string pattern-table &optional (accum ""))
"
   (.parse-string \"jjj0123jjjjj012345abc\" '((\"01234\" . |1-4|) (\"0123\" . |1-3|) (\"abc\" . ABC))

   -> (\"jjj\" |1-3| \"jjjjj\" |1-4| \"5\" ABC)
"
  (if (zerop (length string))
      (if (not (zerop (length accum)))
	  (list accum))
      (let ((1st-char (char string 0)))
	(if (char= 1st-char #\\)
	    (.parse-string (subseq string 2)
			   pattern-table
			   (format nil "~A~A" accum (char string 1)))
	    (multiple-value-bind (fn pattern-length)
		(.get-match string pattern-table)
	      (if fn
		  (if (zerop (length accum))
		      (cons fn #1=(.parse-string (subseq string pattern-length)
						 pattern-table))
		      (list* accum fn #1#))
		  (.parse-string (subseq string 1) pattern-table (format nil "~A~A" accum (char string 0)))))))))

; R-UT

(defparameter *read-ut-pattern-table*
  `(("?"     . ?)
    ("YYYY" . year)
    ("YY"   . year)
    ("MM"   . month)
    ("M"    . month)
    ("DD"   . date)
    ("D"    . date)
    ("hh"   . hour)
    ("h"    . hour)
    ("mm"   . minute)
    ("m"    . minute)
    ("ss"   . second)
    ("s"    . second)))

(defun ut-pattern->src/read (pattern)
  `(lambda (string)
    (let ((string-length (length string))
	  (n 0)
	  (year 0)
	  (hour 0)
	  (minute 0)
	  (second 0)
	  (date 1)
	  (month 1))
      (declare (ignorable string-length))
      ,@(labels ((rfn (%s)
		   (when %s
		     (destructuring-bind (1st% . rest%) %s
		       (etypecase 1st%
			 (string (cons `(incf n ,(length 1st%)) (rfn rest%)))
			 (symbol (case 1st%
				   (? (list `(when (> string-length n)
					       ,@(rfn rest%))))
				   (otherwise (cons `(multiple-value-setq (,1st% n)
						       (parse-integer string
								      :start n
								      :junk-allowed t))
						    (rfn rest%))))))))))
		(rfn (.parse-string pattern *read-ut-pattern-table*)))
      (encode-universal-time second minute hour date month year *time-zone*))))

(defun r-ut (string &optional (pattern +default-ut-pattern+))
  "reads `STRING' according to the pattern string `PATTERN' and returns a universal time
like would be returned by ENCODE-UNIVERSAL-TIME"
  (funcall (coerce (ut-pattern->src/read pattern) 'function)
	   string))

(define-compiler-macro r-ut (&whole whole string &optional (pattern +default-ut-pattern+))
  (cond ((stringp pattern) `(funcall ,(ut-pattern->src/read pattern) ,string))
	((and (symbolp pattern)
	      (constantp pattern)) `(funcall ,(ut-pattern->src/read (symbol-value pattern)) ,string))
	(t (warn "<<<R-UT is a _lot_ faster if `PATTERN' is a constant or a string. see UT-PATTERN->SRC/READ if you need to compile a fast reader at runtime>>>")
	   whole)))

; W-UT

(defparameter *write-ut-pattern-table*
  `(("?"    . ?)
    ("YYYY" year   n)
    ("YY"   year   2)
    ("MM"   month  2)
    ("M"    month  n)
    ("DD"   date   2)
    ("D"    date   n)
    ("hh"   hour   2)
    ("h"    hour   n)
    ("mm"   minute 2)
    ("m"    minute n)
    ("ss"   second 2)
    ("s"    second n)))

(defun .parsed-string->ut-parts (parsed-pattern)
"
   (.parsed-string->ut-parts '(\"jjj\" (year n) \"jjjjj\" (date 2) ? \"5\" (month 2)))

   -> (year date month)
"
  (mapcar 'first (remove-if-not 'listp parsed-pattern)))

(defun .ut-part->minimum-value (symbol)
  (ecase symbol
    ((second minute hour year) 0)
    ((date month) 1)))

(defun ut-pattern->src/write (pattern)
  `(lambda (ut)
     (multiple-value-bind (second minute hour date month year)
	 (decode-universal-time ut *time-zone*)
       (declare (ignorable second minute hour date month year))
       (with-output-to-string (out)
	 ,@(labels ((rfn (%s)
		      (when %s
			(destructuring-bind (1st% . rest%) %s
			  (typecase 1st%
			    (string (cons `(write-string ,1st% out) (rfn rest%)))
			    (otherwise (case 1st%
					 (? (list `(unless (and ,@(mapcar (lambda (_) `(= ,_ ,(.ut-part->minimum-value _)))
									(.parsed-string->ut-parts rest%)))
						     ,@(rfn rest%))))
					 (otherwise (destructuring-bind (symbol digits) 1st%
						      (cons (ecase digits
							      (n `(princ ,symbol out))
							      (2 (case symbol
								   (year `(format out "~2,'0d" (mod year 100)))
								   (otherwise `(format out "~2,'0d" ,symbol)))))
							    (rfn rest%)))))))))))

		    (rfn (.parse-string pattern *write-ut-pattern-table*)))))))

(defun w-ut (ut &optional (pattern +default-ut-pattern+))
  "write the universal `UT' as a string according to the pattern string `PATTERN'"
  (funcall (coerce (ut-pattern->src/write pattern) 'function)
	   ut))

(define-compiler-macro w-ut (&whole whole ut &optional (pattern +default-ut-pattern+))
  (cond ((stringp pattern) `(funcall ,(ut-pattern->src/write pattern) ,ut))
	((and (symbolp pattern)
	      (constantp pattern)) `(funcall ,(ut-pattern->src/write (symbol-value pattern)) ,ut))
	(t (warn "<<<W-UT is a _lot_ faster if `PATTERN' is a constant or a string. see UT-PATTERN->SRC/WRITE if you need to compile a fast writer at runtime>>>")
	   whole)))