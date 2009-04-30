(in-package #:cl-l10n.system)

(defpackage :cl-l10n.test
  (:use :common-lisp
        :cl-l10n
        :alexandria
        :iter
        :stefil
        :metabang-bind
        )
  (:shadowing-import-from :cl-l10n
   #:format
   #:formatter
   #:*locale-cache*
   #:*locale*
   #:precedence-list-of
   ))

(in-package #:cl-l10n.test)

(defsuite* (test :in root-suite))

;; import all the internal symbol of WUI
(let ((cl-l10n-package (find-package :cl-l10n))
      (cl-l10n-test-package (find-package :cl-l10n.test)))
  (iter (for symbol :in-package cl-l10n-package :external-only nil)
        (when (and (eq (symbol-package symbol) cl-l10n-package)
                   (not (find-symbol (symbol-name symbol) cl-l10n-test-package)))
          (import symbol cl-l10n-test-package))))
