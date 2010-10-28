;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
 "This file defines float codecs for the `de.setf.utility.codecs` library."
  
  (:copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
 'de.setf.utility.codecs' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility.codecs' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility.codecs, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (:description "Comprises direct tests for float <-> integer conversion and 32/64 stream/buffer operators."))


(test:test codecs.ieee-754-64
  "Test ieee-754-64 packed-integer/float conversion.
     See http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml for the values."
  (null (remove t '(;; all NAN are encoded as positive silent
                    #xFFF0000000000000
                    #xFFEFFFFFFFFFFFFF #x8010000000000000 #x800FFFFFFFFFFFFF #x8000000000000001
                    #x8000000000000000 #x0000000000000000
                    #x0000000000000001 #x000FFFFFFFFFFFFF #x0010000000000000 #x7FEFFFFFFFFFFFFF
                    #x7FF8000000000000
                    #x4039000000000000 #xC039000000000000 #x3FF0000000000000 #xBFF0000000000000
                    #x4000000000000000 #xC000000000000000 #x3FD5555555555555 #xBFD5555555555555)
                :key #'(lambda (x)
                         (cond ((eql (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x)) x))
                               (t
                                (warn "ieee-754-64 failed: #x~16,'0x -> ~d -> #x~16,'0x, ~d"
                                      x (ieee-754-64-integer-to-float x)
                                      (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))
                                      (ieee-754-64-integer-to-float (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))))
                                x))))))

(test:test codecs.ieee-754-32
  "Test ieee-754-32 packed-integer/float conversion.
     See http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml for the values."
  (null (remove t '(;; all NAN are encoded as positive silent
                    #xFF800000
                    #xFF7FFFFF #x80800000 #x807FFFFF #x80000001
                    #x80000000 #x0000000
                    #x00000001 #x007FFFFF #x00800000 #x7F7FFFFF
                    #x7F800000  
                    ;; various numbers
                    #x41c80000 #xc1c80000 #x3f800000 #xbf800000
                    #x40000000 #xc0000000 #x3eaaaaab #xbeaaaaab)
                :key #'(lambda (x)
                         (cond ((eql (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x)) x))
                               (t
                                (warn "ieee-754-32 failed: #x~8,'0x -> ~d -> #x~8,'0x, ~d"
                                      x (ieee-754-32-integer-to-float x)
                                      (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))
                                      (ieee-754-32-integer-to-float (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))))
                                x))))))



(test:test codecs.buffer-ieee-codecs
  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((do-codec (encoder decoder value)
               (funcall encoder buffer value 0)
               (eql (funcall decoder buffer 0) value)))
      (dolist (value (list double-float-negative-infinity most-negative-double-float
                           -1.0d0 least-negative-double-float
                           0.0d0 double-float-nan
                           least-positive-double-float 1.0d0
                           most-positive-double-float double-float-positive-infinity)
                     t)
        (unless (do-codec #'buffer-set-float-64 #'buffer-get-float-64 value)
          (return nil)))
      (dolist (value (list single-float-negative-infinity most-negative-single-float
                           -1.0s0 least-negative-single-float
                           0.0s0 single-float-nan
                           least-positive-single-float 1.0s0
                           most-positive-single-float single-float-positive-infinity)
                     t)
        (unless (do-codec #'buffer-set-float-32 #'buffer-get-float-32 value)
          (return nil))))))


(test:test codecs.stream-ieee-codecs
  (let ((stream (make-instance 'vector-io-stream)))
    (labels ((do-codec (encoder decoder value)
               (stream-position stream 0)
               (funcall encoder stream value)
               (stream-position stream 0)
               (eql (funcall decoder stream) value)))
      (dolist (value (list double-float-negative-infinity most-negative-double-float
                           -1.0d0 least-negative-double-float
                           0.0d0 double-float-nan
                           least-positive-double-float 1.0d0
                           most-positive-double-float double-float-positive-infinity)
                     t)
        (unless (do-codec #'stream-write-float-64 #'stream-read-float-64 value)
          (return nil)))
      (dolist (value (list single-float-negative-infinity most-negative-single-float
                           -1.0s0 least-negative-single-float
                           0.0s0 single-float-nan
                           least-positive-single-float 1.0s0
                           most-positive-single-float single-float-positive-infinity)
                     t)
        (unless (do-codec #'stream-write-float-32 #'stream-read-float-32 value)
          (return nil))))))
