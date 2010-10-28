;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file tests utilities for the `de.setf.utility.codecs` library."
  
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

(test:test codecs.signed-byte
  (flet ((test-sign (op specs)
           (loop for (bytes value) in specs
                 unless (= (funcall op bytes) value)
                 return nil
                 finally (return t))))
    (let ((l 0))
      (and (test-sign 'sign-byte-8 '((#x80 -128) (#xfe -2) (#x00 0) (#x01 1) (#x7f 127)))
           (test-sign 'sign-byte-16 `((#x8000 ,(- (expt 2 15))) (#xfffe -2)
                                      (#x00 0)
                                      (#x0001 1) (#x7fff ,(1- (expt 2 15)))))
           (test-sign 'sign-byte-32 `((#x80000000 ,(- (expt 2 31))) (#xfffffffe -2)
                                      (#x00000000 0)
                                      (#x00000001 1) (#x7fffffff ,(1- (expt 2 31)))))
           (test-sign 'sign-byte-64 `((#x8000000000000000 ,(- (expt 2 63)) (#xfffffffffffffffe -2)
                                       (#x0000000000000000 0)
                                       (#x0000000000000001 1) (#x7fffffffffffffff ,(1- (expt 2 63))))))
           (setf l 8)
           (= (sign-byte #xfe 8) -2 (sign-byte #xfe l))
           (setf l 16)
           (= (sign-byte #xfffe 16) -2 (sign-byte #xfffe l))
           (setf l 32)
           (= (sign-byte #xfffffffe 32) -2 (sign-byte #xfffffffe l))
           (setf l 64)
           (= (sign-byte #xfffffffffffffffe 64) -2 (sign-byte #xfffffffffffffffe l))))))


(test:test codecs.ensure-buffer-length
  (let ((buffer (make-array 32 :adjustable t :fill-pointer 7)))
    (setf buffer (ensure-buffer-length buffer 63))
    (and (= (length buffer) 7)
         (>= (array-dimension buffer 0) 63))))

(test:test codecs.cons-length
  (and (= (cons-length '(a s d f)) 4)
       (= (cons-length '(a s d . f)) 3)))
