(in-package :json-test)
(in-suite json)

(defmacro with-objects-as-hashtables(&body body)
  ;;For testing, keys are stored as strings
  `(let ((*json-object-factory* #'(lambda ()
                                   (make-hash-table :test #'equalp )))
        (*json-object-factory-add-key-value* #'(lambda (obj key value)
                                                 (setf (gethash key obj)
                                                       value)
                                                 obj))
        (*json-object-factory-return* #'identity))
    ,@body))

(test json-string()
   (is (string= (encode-json-to-string (format nil "hello~&hello"))
                 "\"hello\\nhello\""))
   (is (string= (encode-json-to-string (format nil "\"aquote"))
                 "\"\\\"aquote\"")))

(test json-literals
  (is (string= "true" (encode-json-to-string t)))
  (is (string= "null" (encode-json-to-string nil))))

(defun is-same-number(nr)
  "If it gets decoded back ok then it was encoded ok"
  (is (= nr (decode-json-from-string (encode-json-to-string nr)))))

(test json-number
  (is (string= "0" (encode-json-to-string 0)))
  (is (string= "13" (encode-json-to-string 13)))
  (is (string= "13.02" (encode-json-to-string 13.02)))

  (is-same-number 2e10)
  (is-same-number  -1.3234e-10)
  (is-same-number -1280.12356)
  (is-same-number 1d2)
  (is-same-number 1l2)
  (is-same-number 1s2)
  (is-same-number 1f2)
  (is-same-number 1e2))

(defun decode-then-encode (json)
  (with-objects-as-hashtables
    (assert (member (elt json 0) '(#\{ #\[ #\" ))) ;must be json
    (flet ((normalize (string)
             (remove #\Newline (remove #\Space string))))
      (let* ((decoded (decode-json-from-string json))
             (encoded (encode-json-to-string decoded)))
;;        (format t "Json:~a~&" json)
;;        (format t "Encoded:~a" encoded)    
        (is (string= (normalize json)
                     (normalize encoded)))))))

(test test-encode-json-nathan-hawkins
  (let ((foo '((a . 1) (b . 2) (c . 3)))
        (*prototype-name* nil))
    (is (string= (encode-json-to-string foo)
                 "{\"a\":1,\"b\":2,\"c\":3}"))))

(test test-encode-json-alist
      (let ((alist `((:HELLO . 100)(:hi . 5)))
            (expected "{\"hello\":100,\"hi\":5}")
            (*prototype-name* nil))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-two
  (let ((alist `((HELLO . 100)(hi . 5)))
        (expected "{\"hello\":100,\"hi\":5}")
        (*prototype-name* nil))
    (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                   expected))))

(test test-encode-json-alist-string
      (let ((*prototype-name* nil)
            (alist `((:hello . "hej")(:hi . "tjena")))
            (expected "{\"hello\":\"hej\",\"hi\":\"tjena\"}"))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-camel-case
      (let ((alist `((:hello-message . "hej")(*also-starting-with-upper . "hej")))
            (expected "{\"helloMessage\":\"hej\",\"AlsoStartingWithUpper\":\"hej\"}")
            (*prototype-name* nil))
        (is (string= (with-output-to-string (s) (encode-json-alist alist s))
                     expected))))

(test test-encode-json-alist-with-prototype
  (let ((alist `((hello . 100) (hi . 5)))
        (expected "{\"hello\":100,\"hi\":5,\"prototype\":{\"lispClass\":\"cons\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}")
        (*prototype-name* 'prototype))
    (is (string= (encode-json-to-string alist) expected))))

(test test-encode-json-plist
      (let ((plist '(:foo 1 :bar "blub"))
            (expected "{\"foo\":1,\"bar\":\"blub\"}"))
        (is (string= (with-output-to-string (s) (encode-json-plist plist s))
                     expected))
        (is (string= (encode-json-plist-to-string plist)
                     expected))))

(test encode-pass-2
  (decode-then-encode "[[[[[[[[[[[[[[[[[[[\"Not too deep\"]]]]]]]]]]]]]]]]]]]"))

(test encode-pass-3
  (let ((*prototype-name* nil))
    (decode-then-encode "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\"
    }
}
")))

(defclass foo () ((bar :initarg :bar) (baz :initarg :baz)))
(defclass goo () ((quux :initarg :quux :initform 933)))
(defclass frob (foo goo) ())

(test test-encode-json-clos
  (finalize-inheritance (find-class 'goo))
  (let ((obj (make-instance 'foo
               :bar (json::make-object '((hello . 100) (hi . 5)) nil
                                       :superclasses '(goo))
               :baz (make-instance 'frob
                      :bar 'xyzzy
                      :baz (make-instance 'standard-object))))
        (expected "{\"bar\":{\"quux\":933,\"hello\":100,\"hi\":5},\"baz\":{\"quux\":933,\"bar\":\"xyzzy\",\"baz\":{}}}")
        (*prototype-name* nil))
    (is (string= (encode-json-to-string obj) expected))))

(test test-encode-json-clos-with-prototype
  (finalize-inheritance (find-class 'goo))
  (let ((obj (make-instance 'foo
               :bar (json::make-object '((hello . 100) (hi . 5)) nil
                                       :superclasses '(goo))
               :baz (make-instance 'frob :bar 'xyzzy :baz 'blub)))
        (expected "{\"bar\":{\"quux\":933,\"hello\":100,\"hi\":5,\"prototype\":{\"lispClass\":null,\"lispSuperclasses\":[\"goo\"],\"lispPackage\":\"jsonTest\"}},\"baz\":{\"quux\":933,\"bar\":\"xyzzy\",\"baz\":\"blub\",\"prototype\":{\"lispClass\":\"frob\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}},\"prototype\":{\"lispClass\":\"foo\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}")
        (*prototype-name* 'prototype))
    (is (string= (encode-json-to-string obj) expected))))

#.(export 'json::emotional (find-package '#:json))
(test test-encode-json-clos-max-package
  (let ((obj (json::make-object '((rational . 100) (emotional . 5)) nil))
        (expected "{\"rational\":100,\"emotional\":5,\"prototype\":{\"lispClass\":null,\"lispSuperclasses\":null,\"lispPackage\":\"json\"}}")
        (*prototype-name* 'prototype))
    (is (progn (with-output-to-string (s) (encode-json obj s)))   #+nil(string= 
                 expected))))

;; Test inspired by the file pass1. 
;; There are too many small differences just to decode-encode the whole pass1 file,
;; Instead the difficult parts are in separate tests below.

(test controls
   (decode-then-encode "\"\\\\b\\\\f\\\\n\\\\r\\\\\""))

(test slash
  (let* ((z "\"/ & /\"")
         (also-z "\"/ & \/\"") ;Extra quote
         (x (encode-json-to-string z))
         (also-x (encode-json-to-string also-z))
         (y (decode-json-from-string x))
         (also-y (decode-json-from-string also-x)))
    (is (string= x also-x))
    (is (string= y also-y))
    (is (string= z y))))


(test quoted
  (decode-then-encode "\"&#34; %22 0x22 034 &#x22;\""))

(test alpha-1
  (decode-then-encode "\"abcdefghijklmnopqrstuvwyz\""))

(test alpha-2
  (decode-then-encode "\"ABCDEFGHIJKLMNOPQRSTUVWYZ\""))

(test digit
  (decode-then-encode "\"0123456789\""))

(test special
  (decode-then-encode "\"`1~!@#$%^&*()_+-={':[,]}|;.</>?\""))

(test hex
  (decode-then-encode "\"\u0123\u4567\u89AB\uCDEF\uabcd\uef4A\""))

(test true
  (decode-then-encode "[ true]"))

(test false
  (is (string= (encode-json-to-string (decode-json-from-string "[false]"))
               "[null]")));;We dont separate between false and null
(test null
  (decode-then-encode "[null]"))

(test array
  (with-list-decoder-semantics
    ;;Since empty lists becomes nil in lisp, they are converted back to null
    (is (string= (encode-json-to-string (decode-json-from-string "[  ]"))
                 "null")))
  (with-clos-decoder-semantics
    ;;Since empty lists becomes #() in lisp, they are converted back to empty list
    (is (string= (encode-json-to-string (decode-json-from-string "[  ]"))
                 "[]")))
  ;;But you can use vectors
  (is (string= (encode-json-to-string (vector 1 2))
               "[1,2]")))

(test character
  ;;Characters are encoded to strings, but when decoded back to string
  (is (string= (encode-json-to-string #\a) "\"a\"")))


(test hash-table-symbol
  (let ((ht (make-hash-table))
        (*prototype-name* nil))
    (setf (gethash 'symbols-are-now-converted-to-camel-case ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"symbolsAreNowConvertedToCamelCase\":5}"))))

(test hash-table-symbol-with-prototype
  (let ((ht (make-hash-table))
        (*prototype-name* 'prototype))
    (setf (gethash 'five ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"five\":5,\"prototype\":{\"lispClass\":\"hashTable\",\"lispSuperclasses\":null,\"lispPackage\":\"jsonTest\"}}"))))

(test hash-table-string
  (let ((ht (make-hash-table :test #'equal))
        (*prototype-name* nil))
    (setf (gethash "lower x" ht) 5)
    (is (string= (encode-json-to-string ht)
                 "{\"lower x\":5}"))))


(defparameter *encode-performace-test-string*
  "{
    \"JSON Test Pattern pass3\": {
        \"The outermost value\": \"must be an object or array.\",
        \"In this test\": \"It is an object.\",
        \"Performance-1\" : 123465.578,
        \"Performance-2\" : 12e4,
        \"Performance-2\" : \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
        \"Performance-3\" : [\"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\",
                         \"asdasdsadsasdasdsdasdasdasdsaaaaaaaaaaaaasdasdasdasdasdsd\"]
    }
}
")





(test encoder-performance
  (with-objects-as-hashtables      
    (let* ((json-string *encode-performace-test-string*)
           (chars (length json-string))
           (lisp-obj (decode-json-from-string json-string))
           (count 2000))
      (format t "Encoding ~a varying chars  from memory ~a times." chars count)
      (time
       (dotimes (x count) 
         (let ((discard-soon (encode-json-to-string lisp-obj)))
           (funcall #'identity discard-soon)))))))



