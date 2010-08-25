;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)
#+digitool
(:documentation
  "This file defines types for the `de.setf.utility.codecs` library."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  )

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
  (sb-vm::with-float-traps-masked (:invalid)
    (defconstant single-float-nan
      (eval '(+ single-float-positive-infinity single-float-negative-infinity)))
    (defconstant double-float-nan
      (eval '(+ double-float-positive-infinity double-float-negative-infinity)))))

