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
