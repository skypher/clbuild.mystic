(in-package :cl-l10n.lang)

(defun with-indefinite-article (str &key capitalize-first-letter)
  (let ((article (indefinite-article-for str)))
    (concatenate 'string
                 (if capitalize-first-letter
                     (capitalize-first-letter article)
                     article)
                 (list #\Space)
                 str)))

(defun with-definite-article (str &key capitalize-first-letter)
  (let ((article (definite-article-for str)))
    (concatenate 'string
                 (if capitalize-first-letter
                     (capitalize-first-letter article)
                     article)
                 (list #\Space)
                 str)))

(defun ensure-ldml-symbol-or-form (thing)
  (if (and thing
           (or (symbolp thing)
               (stringp thing)))
      (cl-l10n::ensure-ldml-symbol thing)
      thing))

(defmacro number-symbol (name)
  `(cl-l10n.lang:localize-number-symbol
    ',(ensure-ldml-symbol-or-form name)))

(defmacro currency-symbol (name)
  `(cl-l10n.lang:localize-currency-symbol
    ',(ensure-ldml-symbol-or-form name)))

(defmacro currency-name (name)
  `(cl-l10n.lang:localize-currency-name
    ',(ensure-ldml-symbol-or-form name)))

(defmacro language (name)
  `(cl-l10n.lang:localize-language-name
    ',(ensure-ldml-symbol-or-form name)))

(defmacro script (name)
  `(cl-l10n.lang:localize-script-name
    ',(ensure-ldml-symbol-or-form name)))

(defmacro territory (name)
  `(cl-l10n.lang:localize-territory-name
    ',(ensure-ldml-symbol-or-form name)))

(defmacro variant (name)
  `(cl-l10n.lang:localize-variant-name
    ',(ensure-ldml-symbol-or-form name)))

(defmacro month (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-month-name
    ',(ensure-ldml-symbol-or-form name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

(defmacro day (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-day-name
    ',(ensure-ldml-symbol-or-form name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

(defmacro quarter (name &key abbreviated capitalize-first-letter)
  `(cl-l10n.lang:localize-quarter-name
    ',(ensure-ldml-symbol-or-form name)
    :abbreviated ,abbreviated
    :capitalize-first-letter ,capitalize-first-letter))

