;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-user)

(defpackage #:cl-l10n.system
  (:use #:cl #:asdf))

(in-package #:cl-l10n.system)

(defsystem cl-l10n
  :name "CL-L10N"
  :author "Sean Ross <sross@common-lisp.net>"
  :maintainer "Sean Ross <sross@common-lisp.net>"
  :version "0.4"
  :description "Portable CL Locale Support"
  :long-description "Portable CL Package to support localization"
  :licence "MIT"
  :components ((:file "flexml")
               (:module :src
                        :components ((:file "package")
                                     (:file "variables" :depends-on ("package"))
                                     (:file "utils" :depends-on ("package" "variables"))
                                     (:file "pattern-compiling" :depends-on ("utils"))
                                     (:file "cldr-parsing" :depends-on ("package" "utils" "locale" "i18n" "pattern-compiling"))
                                     (:file "locale" :depends-on ("utils" "calendar"))
                                     (:file "calendar" :depends-on ("utils"))
                                     (:file "load-locale" :depends-on ("locale" "cldr-parsing"))
                                     (:file "formatters" :depends-on ("load-locale"))
                                     (:file "i18n" :depends-on ("locale"))
                                     (:module :languages
                                              :components ((:file "common")
                                                           (:file "english" :depends-on ("common"))
                                                           (:file "hungarian" :depends-on ("common")))
                                              :depends-on ("package" "utils"))
                                     (:module :resources
                                              :components ((:file "common"))
                                              :depends-on ("package" "utils" "load-locale")))
                        :depends-on ("flexml")))
  :depends-on (:alexandria
               :iterate
               :cl-ppcre
               :metabang-bind
               :cl-fad
               :flexi-streams ; TODO replace with babel
               :cxml
               :local-time
               :closer-mop
               ))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-l10n))))
  (provide 'cl-l10n))

(defsystem :cl-l10n.test
  :depends-on (:cl-l10n
               :stefil
               :parse-number
               )
  :components
  ((:module "test"
            :components
            ((:file "package")
             (:file "cldr" :depends-on ("package"))
             (:file "resources" :depends-on ("package" "cldr"))))))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-l10n))))
  (oos 'load-op :cl-l10n.test)
  (in-package :cl-l10n.test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Issued a (declaim (optimize (debug 3))) and changed *package* for C-cC-c/REPL convenience")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-l10n.test::test)")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-l10n))))
  nil)
