;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n.system)

(defpackage #:cl-l10n
  (:use
   :common-lisp
   :metabang-bind
   :alexandria
   :iterate
   )
  (:shadow
   cl:format
   cl:formatter
   )
  (:export
   #:locale
   #:locale-name
   #:current-locale
   #:locale-error
   #:load-all-locales
   #:with-locale

   #:locale-not-found-error

   #:format-number/decimal
   #:format-number/percent
   #:format-number/currency

   #:format-date
   #:format-time
   #:format-timestamp

   #:shadow-format
   #:reload-resources
   #:register-locale-loaded-listener
   #:unregister-locale-loaded-listener

   #:capitalize-first-letter
   #:capitalize-first-letter!

   #:lookup-resource
   #:localize
   #:resource-missing
   #:defresources
   #:enable-sharpquote-reader
   #:with-sharpquote-reader
   #:lookup-first-matching-resource

   #:consonantp
   #:vowelp
   #:high-vowel-p
   #:low-vowel-p
   #:last-vowel-of
   #:starts-with-consonant-p
   #:starts-with-vowel-p

   #:english-plural-of
   #:english-indefinite-article-for
   #:hungarian-definite-article-for
   #:hungarian-plural-of
   ))

(defpackage :cl-l10n.lang
  (:use :common-lisp :cl-l10n)

  (:shadowing-import-from :cl-l10n
   #:defresources)

  (:export

   #:plural-of
   #:indefinite-article-for
   #:definite-article-for

   #:list

   #:number-symbol
   #:currency-symbol
   #:currency-name
   #:language
   #:script
   #:territory
   #:variant
   #:month
   #:day
   #:quarter

   #:with-indefinite-article
   #:with-definite-article

   #:localize-currency-symbol
   #:localize-currency-name
   #:localize-language-name
   #:localize-script-name
   #:localize-territory-name
   #:localize-variant-name
   #:localize-month-name
   #:localize-day-name
   #:localize-quarter-name
   #:localize-number-symbol
   ))

(defpackage :cl-l10n.ldml
  (:nicknames :ldml)
  (:export
   #:january
   #:february
   #:marc
   #:april
   #:may
   #:june
   #:july
   #:august
   #:september
   #:october
   #:november
   #:december

   #:first-quarter
   #:second-quarter
   #:third-quarter
   #:fourth-quarter

   #:sunday
   #:monday
   #:tuesday
   #:wednesday
   #:thursday
   #:friday
   #:saturday

   #:node

   #:ldml
   #:identity
   #:language
   #:languages
   #:script
   #:territory
   #:variant
   #:numbers
   #:symbols
   #:currencies
   #:currency
   #:currency-formats
   #:currency-format-length
   #:currency-format
   #:currency-spacing
   #:currency-match
   #:surrounding-match
   #:insert-between
   #:display-name
   #:script
   #:scripts
   #:territories
   #:territory
   #:variants
   #:variant
   #:calendars
   #:calendar
   #:month-width
   #:month
   #:day-width
   #:day
   #:quarter-width
   #:quarter
   #:am
   #:pm
   #:date-formats
   #:date-format-length
   #:time-formats
   #:time-format-length
   #:era-names
   #:era-abbr
   #:era-narrow
   #:era
   #:short
   #:medium
   #:long
   #:full
   #:decimal
   #:decimal-formats
   #:decimal-format-length
   #:decimal-format
   #:native-zero-digit
   #:group
   #:percent-sign
   #:per-mille
   #:infinity
   #:exponential
   #:plus-sign
   #:minus-sign
   #:list
   #:zero
   #:one
   #:two
   #:few
   #:many
   #:other
   #:unit-pattern
   #:percent-formats
   #:percent-format-length
   #:percent-format
   ))

(block check-lisp-source-file-encoding
  (map nil (lambda (a b)
             (unless (eql a (char-code b))
               (cerror "try it anyway"
                       "Your lisp seems to be reading .lisp files in something else then UTF-8. The source files of cl-l10n contain literal strings with unicode characters and failing to properly read them in UTF-8 will cause problems.")
               (return-from check-lisp-source-file-encoding)))
       #(233 225 250 337 243 246 369 237)
       "éáúőóöűí"))
