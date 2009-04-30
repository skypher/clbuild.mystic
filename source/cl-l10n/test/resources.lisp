;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-l10n.test)

(defmacro def-resource-test (name (operator &rest locales) &body body)
  `(deftest ,name ()
     (flet ((one-pass ()
              ,@(iter (for (input expected) :in body)
                      (collect `(is (string= (,operator ,@(ensure-list input))
                                             ,expected))))))
       ,@(iter (for locale :in (ensure-list locales))
               (collect `(with-locale ,locale
                           (one-pass)))))))

(defsuite* (resources :in test))

;;;
;;; Hungarian
;;;
(def-resource-test test/resources/hu/indefinite-article
    (cl-l10n.lang:with-indefinite-article "hu" "hu_HU")
  ("alma"   "egy alma")
  ("barack" "egy barack")
  (("kutya" :capitalize-first-letter t) "Egy kutya"))

(def-resource-test test/resources/hu/definite-article
    (cl-l10n.lang:with-definite-article "hu" "hu_HU")
  ("alma"    "az alma")
  ("állat"   "az állat")
  ("barack"  "a barack")
  ("őr"      "az őr")
  (("kutya" :capitalize-first-letter t) "A kutya"))

(def-resource-test test/resources/hu/month-names
    (cl-l10n.lang:month "hu" "hu_HU")
  (0          "január")
  (january    "január")
  (1          "február")
  (2          "március")
  (11         "december")
  (december   "december")
  ((11 :capitalize-first-letter t) "December"))

(def-resource-test test/resources/hu/day-names
    (cl-l10n.lang:day "hu" "hu_HU")
  (0          "vasárnap")
  (1          "hétfő")
  (monday     "hétfő")
  (6          "szombat")
  ((0 :capitalize-first-letter t) "Vasárnap"))

;;;
;;; English
;;;
(def-resource-test test/resources/en/indefinite-article
    (cl-l10n.lang:with-indefinite-article "en" "en_GB" "en_US" "en_US_POSIX")
  ("table"    "a table")
  ("elephant" "an elephant")
  ("car"      "a car")
  (("dog" :capitalize-first-letter t)     "A dog")
  (("element" :capitalize-first-letter t) "An element"))

(def-resource-test test/resources/en/definite-article
    (cl-l10n.lang:with-definite-article "en" "en_GB" "en_US" "en_US_POSIX")
  ("table"    "the table")
  ("elephant" "the elephant")
  ("car"      "the car")
  (("dog" :capitalize-first-letter t)     "The dog")
  (("element" :capitalize-first-letter t) "The element"))

(def-resource-test test/resources/en/month-names
    (cl-l10n.lang:month "en" "en_GB" "en_US" "en_US_POSIX")
  (0   "January")
  (1   "February")
  (2   "March")
  (11  "December")
  ((11 :capitalize-first-letter t) "December"))
