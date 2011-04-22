;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines types for the `de.setf.utility.codecs` library."
  
  (:copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
 'de.setf.utility.codecs' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility.codecs' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility.codecs, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(defconstant +string-element-type+ 'character)

;; don't tell a compiler more than it needs to know, otherwise shorter vectors can conflict with declarations
(deftype byte-buffer (&optional length)
  (declare (ignore length))
  `(array (unsigned-byte 8) (*)))

(deftype simple-byte-buffer (&optional length)
  (declare (ignore length))
  `(simple-array (unsigned-byte 8) (*)))

(deftype character-buffer (&optional length)
  (declare (ignore length))
  `(array character (*)))

(deftype simple-character-buffer (&optional length)
  (declare (ignore length))
  `(simple-array character (*)))

;;; floating point boundary constants
;;; define them where an implementation has not prepared them
;;;
;;; extended from corkill's openmcl addition


#-sbcl
(declaim (double-float double-float-positive-infinity
                       double-float-negative-infinity)
         (single-float single-float-positive-infinity
                       single-float-negative-infinity))

(declaim (double-float double-float-nan)
         (single-float single-float-nan))

#+mcl
(unless (boundp 'double-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant double-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0d0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant double-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0d0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl (and clozure (not ccl-1.4)))
(unless (boundp 'double-float-nan)
  (defconstant double-float-nan
    (unwind-protect
      (locally (declare (special double-float-positive-infinity double-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ double-float-positive-infinity double-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+(or mcl clozure)
(unless (boundp 'single-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant single-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0f0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant single-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0f0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl clozure)
(unless (boundp 'single-float-nan)
  (defconstant single-float-nan
    (unwind-protect
      (locally (declare (special single-float-positive-infinity single-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ single-float-positive-infinity single-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+sbcl  ;; works on osx and linux
(unless (boundp 'single-float-nan)
  ;; do _not_ define as constants as sbcl's compiler chokes on any reference to them
  ;; do _not_ let it compile the value expressions as it tries to constant-fold them, with similar results.
  (sb-vm::with-float-traps-masked (:invalid)
    (defparameter single-float-nan
      (eval '(+ single-float-positive-infinity single-float-negative-infinity)))
    (defparameter double-float-nan
      (eval '(+ double-float-positive-infinity double-float-negative-infinity)))))


#+lispworks
(progn
  (defconstant double-float-positive-infinity +1D++0)
  (defconstant double-float-negative-infinity -1D++0)
  (defconstant single-float-positive-infinity +1F++0)
  (defconstant single-float-negative-infinity -1F++0)

  (defconstant single-float-nan SYSTEM::*SINGLE-FLOAT-NAN*)
  (defconstant double-float-nan SYSTEM::*DOUBLE-FLOAT-NAN*))
