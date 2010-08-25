;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "This file is a place-holder for source walking operators for the 'de.setf.utility' library."
  
  (copyright
   "Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;;
;;; utilities

(defun read-file-package-names (pathname &aux names form (eof (gensym)) (error (gensym)))
  (labels ((extract-name (form)
             (when (consp form)
               (let ((operator (first form)))
                 (cond ((eq operator 'eval-when)
                        (mapcar #'extract-name (cddr form)))
                       ((and (search "package" (string operator) :test #'char-equal)
                             (symbolp (second form)))
                        (pushnew (string (second form)) names :test #'string=)))))))
    (when (pathname-name pathname)
      (with-open-file (stream pathname :direction :input)
        (loop (setf form (handler-case (read stream nil eof)
                           (error () error)))
              (when (or (eq eof form) (eq form error)) (return))
              (when (consp form) (extract-name form)))
        names))))

;(mapcar #'read-file-package-names (directory "LIBRARY:de;setf;utility;*;*.lisp"))

(defmethod walk-packages ((root t) (packages pathname) op &rest options)
  (apply #'walk-packages root
         (remove-duplicates (apply #'append
                                   (mapcar #'read-file-package-names
                                           (if (directory-pathname-p packages)
                                             (directory (make-pathname :name :wild :type :wild :defaults packages))
                                             (list packages)))))
         op
         options))

:de.setf.utility.walker
