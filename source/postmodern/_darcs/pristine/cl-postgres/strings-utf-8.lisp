(in-package :cl-postgres)

(defparameter *client-encoding* "UNICODE")

(declaim (inline enc-byte-length))
(defun enc-byte-length (sequence)
  (trivial-utf-8:utf-8-byte-length sequence))

(declaim (inline enc-write-string))
(defun enc-write-string (string output &key null-terminate)
  (trivial-utf-8:write-utf-8-bytes string output
                                   :null-terminate null-terminate))

(declaim (inline enc-read-string))
(declaim (ftype (function (t &key (:null-terminated t)
                                  (:byte-length unsigned-byte))
                          string)
                enc-read-string))
(defun enc-read-string (input &key null-terminated (byte-length -1))
  (trivial-utf-8:read-utf-8-string input :null-terminated null-terminated
                                   :byte-length byte-length))

(declaim (inline enc-string-bytes))
(defun enc-string-bytes (string &key null-terminate)
  (trivial-utf-8:string-to-utf-8-bytes string :null-terminate null-terminate))
