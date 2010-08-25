;;; -*- Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It defines several string utility functions.

;;;  Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).
;;;
;;;  nb. clocc/cllib includes operators with a similar interface, but they 
;;;  ignored some of the constraints.


;;; Content :
;;;
;;;  split-string
;;;  split-sequence
;;;  cons-symbol (package &rest elements)

;;; 20030101 established independant utility
;;; 20030214 string+

(in-package :de.setf.utility.implementation)

(modPackage :de.setf.utility
  (:export
   :concatenate-string
   :cons-symbol
   :string+
   :split-string
   :split-sequence
   :trim-string-whitespace
   :when-symbol
   ))

(defun trim-string-whitespace (string)
  (string-trim #(#\space #\tab #\return #\linefeed) string))


;;; these don't do the right thing, as they modify their argument lists

(defun concatenate-string* (list &aux (length 0))
  (declare (type cons list) (type fixnum length))
  (cond ((consp list)
         (mapl #'(lambda (list &aux (elt (first list)))
                   (etypecase elt
                     (cons (setf elt (concatenate-string* elt))
                           (setf (first list) elt))
                     (null )
                     (string ))
                   (when elt (incf length (length elt))))
               list)
         (let ((string (make-string length))
               (index 0))
           (declare (type string string)
                    (type fixnum index))
           (dolist (elt list)
             (when (stringp elt)
               (locally  (declare (type string elt))
                 (dotimes (i (length elt)) (declare (type fixnum i))
                          (setf (schar string index) (char elt i))
                          (incf index)))))
           string))
        (t
         "")))

(defun concatenate-string (&rest list)
  (declare (dynamic-extent list))
  (concatenate-string* list))

;; (concatenate-string "asd" '("qwe" "try") "zxc")

(defun string+ (&rest string-designators)
   (concatenate-string* (mapl #'(lambda (designators &aux (designator (first designators)))
                                  (etypecase designator
                                    (character (setf (first designators)
                                                     (make-string 1 :initial-element designator)))
                                    (null )
                                    (string )
                                    (sequence (setf (first designators) (reduce #'string+ designator)))
                                    (symbol (setf (first designators) (string designator)))))
                              string-designators)))

(defun split-sequence (seq pred &key (start 0) end key strict punctuation-p)
  (let* ((p0 (if strict start (position-if-not pred seq :start start :end end :key key)))
         (p1 0)
         (result (list nil))
         (next result))
    (labels ((collect (x) (setf next (setf (rest next) (list x))))
             (collect-characters (sequence start end)
               (loop (unless (< start end) (return))
                     (collect (elt sequence start))
                     (incf start))))
      (when (and punctuation-p (or (null p0) (plusp p0)))
        (collect-characters seq 0 (or p0 (length seq))))
      (loop (unless (and p0 p1) (return))
            (setf p1 (position-if pred seq :start p0 :end end :key key))
            (collect (subseq seq p0 (or p1 end)))
            (when p1
              (setq p0 (if strict (1+ p1) (position-if-not pred seq :start p1 :end end :key key)))
              (when punctuation-p
                (collect-characters seq p1 (or p0 (length seq)))))))
    (rest result)))

(defun split-string (str chars &rest opts)
  "Split the string on chars."
  (apply #'split-sequence str
         (etypecase chars
           (character
            #'(lambda (ch) (declare (character ch) (character chars)) (eql ch chars)))
           (sequence
            (etypecase (elt chars 0)
              (character #'(lambda (ch) (declare (character ch)) (find ch chars)))
              (fixnum #'(lambda (ch) (declare (character ch)) (find (char-code ch) chars)))))
           (function chars))
         opts))

;(split-string "<<>" ",.<>" :punctuation-p t)
;(split-string "<<>" ",.<>" :punctuation-p nil)
;(split-string "asdf,qwer" ",")
;(split-string "the macro with-namespace-declaration-handler.</p>" #(#x09 #x0A #x0D #x20 #x85 #\( #\) #\, #\< #\> #\.) :punctuation-p t)
;(split-string ",,qwer" "," :strict t)
;(split-string ",,qwer" "," :punctuation-p t :strict t)
;(split-string ",,qwer" ",")



(defun cons-symbol (package &rest args)
  "Construct a symbol given string designators. If package is null, the symbol is
 a new, uninterned symbol."
  (declare (dynamic-extent args))

  (multiple-value-bind (symbol name)
                       (apply #'when-symbol package args)
    (or symbol (intern name package))))


(defun when-symbol (package &rest args)
  (declare (dynamic-extent args))

  (flet ((element-length (element)
           (if element (length (string element)) 0)))
    (declare (dynamic-extent #'element-length))
    (let* ((length (reduce #'+ args :key #'element-length :initial-value 0))
           (name (make-string length))
           (position 0))
      (declare (dynamic-extent name))
      (dolist (el args)
        (when el
          (setf el (string el))
          (replace name  el :start1 position)
          (incf position (length el))))
      (ecase (readtable-case *readtable*)
        (:upcase (map-into name #'char-upcase name))
        (:downcase (map-into name #'char-downcase name))
        (:preserve )
        (:invert (flet ((char-invert (c)
                          (cond ((upper-case-p c) (char-downcase c))
                                ((lower-case-p c) (char-upcase c))
                                (t c))))
                   (declare (dynamic-extent #'char-invert))
                   (map-into name #'char-invert name))))
      (if package
        (or (find-symbol name package)
            (values nil (copy-seq name)))
        (make-symbol (copy-seq name))))))

(unless (find-package "_")
  (defpackage "_" (:use)
    (:documentation "An isolated package for macro definition symbols.")))


:de.setf.utility
