;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; encoding: utf-8 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

(defparameter *hungarian-plural-overrides*
  (read-key->value-text-file-into-hashtable
   (project-relative-pathname (make-pathname :directory '(:relative "src" "languages")
                                             :name "hungarian-plural-overrides"
                                             :type "text"))))

(defun hungarian-plural-of (word &optional (uppercase nil uppercase-provided-p))
  "Returns the hungarian plural of the given word."
  (declare (type string word)
           (optimize (speed 3) (debug 0)))
  (awhen (gethash word *hungarian-plural-overrides*)
    (return-from hungarian-plural-of
      (if uppercase
          (string-upcase it)
          it)))
  (let ((length (length word)))
    (when (< length 3)
      (setf word (concatenate 'string word "-k"))
      (return-from hungarian-plural-of
        (if uppercase
            (string-upcase word)
            word)))
    (let* ((original-last-letter (elt word (1- length)))
           (last-letter (char-downcase original-last-letter))
           (last-letter2 (char-downcase (elt word (- length 2))))
           (last-letter3 (char-downcase (elt word (- length 3)))))
      (unless uppercase-provided-p
        (setf uppercase (upper-case-p original-last-letter)))
      (macrolet ((all-but-last (&optional (count 1))
                   `(subseq word 0 (- length ,count)))
                 (emit (body &rest pieces)
                   `(return-from hungarian-plural-of
                     (concatenate 'string ,body
                      ,@(iter (for piece in pieces)
                              (collect `(if uppercase
                                         (string-upcase ,piece)
                                         ,piece)))))))
        (if (vowelp last-letter)
            (cond ((eq last-letter #\e) (emit (all-but-last) "ék"))
                  ((eq last-letter #\a) (emit (all-but-last) "ák"))
                  #+nil((and (eq last-letter #\i) (melleknev -i kepzovel)) (emit word "ek"))
                  (t (emit word "k")))
            (when-bind last-vowel (last-vowel-of word)
              ;; handle -zat,-zet,-zás,-zés
              (when (and (eq #\z last-letter3)
                         (or (and (or (eq #\a last-letter2)
                                      (eq #\e last-letter2))
                                  (eq #\t last-letter))
                             (and (or (eq #\á last-letter2)
                                      (eq #\é last-letter2))
                                  (eq #\s last-letter))))
                (emit word (if (or (eq #\a last-letter2)
                                   (eq #\á last-letter2))
                               "ok"
                               "ek")))
              (if (high-vowel-p last-vowel)
                  (cond ((eq last-vowel #\ö)
                         (emit word "ök"))
                        (t (emit word "ek")))
                  (progn
                    ;; handle -at, -um
                    (when (or (and (eq #\a last-letter2)
                                   (eq #\t last-letter))
                              (and (eq #\u last-letter2)
                                   (eq #\m last-letter)))
                      (emit word "ok"))
                    ;; handle -ott
                    (when (and (eq #\o last-letter3)
                               (eq #\t last-letter2)
                               (eq #\t last-letter))
                      (emit word "ak"))
                    (emit word "ok")))))
        (emit word "-k")))))

(defun hungarian-definite-article-for (word)
  "Returns a/az for the given word."
  (declare (type string word)
           (optimize (speed 3) (debug 0)))
  (if (> (length word) 1)
      (if (vowelp (elt word 0))
          "az"
          "a")
      word))

#+(and sbcl sb-unicode)
(progn
  (defun jelző-képző-rag-egész-számokhoz (number)
    (declare (optimize (speed 3)))
    (declare (type integer number))
    (setf number (abs number))
    (let ((number-of-leading-zeros
           (if (zerop number)
               0
               (iter (for (values i remainder) :first (floor number 10) :then (floor i 10))
                     (while (zerop remainder))
                     (counting t)))))
      (cond
        ((zerop number-of-leading-zeros)
         ;; 0-9
         (aref #.(coerce '("ás" "es" "es" "as" "es" "ös" "os" "es" "as" "es") '(simple-vector 10))
               (mod number 10)))
        ((= 1 number-of-leading-zeros)
         ;; 10-90
         (aref #.(coerce '(nil "es" "as" "as" "es" "es" "as" "es" "as" "es") '(simple-vector 10))
               (/ (mod number 100) 10)))
        ((= 2 number-of-leading-zeros)
         ;; 100
         "as")
        (t
         ;; 1000-10000-100000...
         (aref #.(coerce '(nil "es" "ós" "os" "ós" "os") '(simple-vector 6))
               (floor number-of-leading-zeros 3))))))
  (export 'jelző-képző-rag-egész-számokhoz))
