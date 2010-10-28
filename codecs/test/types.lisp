;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file tests type definitions for the `de.setf.utility.codecs` library."
  
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

(test:test codec.types.float
  (and (typep double-float-positive-infinity 'double-float)
       (typep double-float-negative-infinity 'double-float)
       (typep (symbol-value 'double-float-nan) 'double-float)
       (typep (symbol-value 'single-float-nan) 'single-float)
       (typep single-float-positive-infinity 'single-float)
       (typep single-float-negative-infinity 'single-float)))

(test:test codecs.types.buffer
  (and (typep (make-array 4 :element-type '(unsigned-byte 8)) 'simple-byte-buffer)
       (typep (make-array 4 :element-type '(unsigned-byte 8)) 'byte-buffer)
       (typep (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0) 'byte-buffer)
       (typep (make-array 4 :element-type 'character) 'simple-character-buffer)
       (typep (make-array 4 :element-type 'character) 'character-buffer)
       (typep (make-array 4 :element-type 'character :fill-pointer 0) 'character-buffer)))

