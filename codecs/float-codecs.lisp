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
  
  (:description "Defines constants, functions and compiler macros for floating data operations on streams and buffers.

 The constants bind the extremes and a subset of the special values for positive/negative infinity and NAN.
 If there are use-cases for others, the set can be extended. The implementation is repackaged from the AMQP
 wire codecs, with the tests still just in that library. See
 [Vickery's](http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml#tables) reference for boundary/test
 values.

 The operators implement the combinations for destination (stream / buffer), direction (read / write), and
 size (single / double). The buffer operators require an additional position argument and return the new
 position as a second value. The general process is to transcode through an intermediary integer
 to cache the IEEE components for buffer/stream coding.

    (mapcar #'type-of  '(1.0f0 1.0d0 1.0s0 1.0l0))

 indicates that abcl, allegro, clozure, cmucl, ecl, mcl, sbcl, and clozure all

    (SINGLE-FLOAT DOUBLE-FLOAT SINGLE-FLOAT DOUBLE-FLOAT)

 lispworks distinguishes  (SINGLE-FLOAT DOUBLE-FLOAT SHORT-FLOAT DOUBLE-FLOAT)
 clisp distinguishes      (SINGLE-FLOAT DOUBLE-FLOAT SHORT-FLOAT LONG-FLOAT)

 Given which, the basic distinction is made between single-float and double-float.
 If a case exists for long-float or short-float, runtime-conditionalized methods
 can be added to map those to double/single to write, but it will not be possible
 to recover them reading.

    (list most-positive-single-float
          most-positive-short-float
          most-positive-double-float
          most-positive-long-float)

 shows the magnitudes."))


(declaim (ftype (function (stream double-float) t) stream-write-double)
         (ftype (function (stream single-float) t) stream-write-float)
         (ftype (function (stream) double-float) stream-read-double)
         (ftype (function (stream) single-float) stream-read-float))

(declaim (ftype (function (stream double-float fixnum) fixnum) buffer-set-double)
         (ftype (function (stream single-float fixnum) fixnum) buffer-set-float)
         (ftype (function (stream fixnum) (values double-float fixnum)) buffer-get-double)
         (ftype (function (stream fixnum) (values single-float fixnum)) buffer-get-float))




;;;
;;; floating point conversion is brute force.

(defun ieee-754-32-integer-to-float (integer)
  (let* ((negative-p (logbitp 31 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7f800000 integer) -23))
         (exponent (- raw-exponent 127))
         (fraction (logand #x007fffff integer)))
    (case raw-exponent
      (#xff
       (if (zerop fraction)
         (if negative-p single-float-negative-infinity single-float-positive-infinity)
         single-float-nan))
      (#x00
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0f0 0.0f0)
         (float (* sign (* fraction (expt 2 (- exponent 22)))) single-float-epsilon)))
      (t
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -23))) (expt 2 exponent))
              single-float-epsilon)))))

(defun ieee-754-64-integer-to-float (integer)
  (let* ((negative-p (logbitp 63 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7ff0000000000000 integer) -52))
         (exponent (- raw-exponent 1023))
         (fraction (logand #x000fffffffffffff integer)))
    (case raw-exponent
      (#x7ff
       (if (zerop fraction)
         (if negative-p double-float-negative-infinity double-float-positive-infinity)
         double-float-nan))
      (#x000
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0d0 0.0d0)
         (float (* sign (* fraction (expt 2 (- exponent 51)))) double-float-epsilon)))
      (t
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -52))) (expt 2 exponent))
              double-float-epsilon)))))

;; (eql (ieee-754-32-integer-to-float #b00111110001000000000000000000000) 0.15625)
;; (eql (ieee-754-32-integer-to-float #b11000010111011010100000000000000) -118.625)

(defun raw-deconstruct-single-float (float)
  (etypecase float
    (single-float )
    (double-float (setf float (float float 1.0f0))))
  #+ccl (multiple-value-bind (fraction exponent sign)
                             (ccl::fixnum-decode-short-float float)
          (values fraction exponent (plusp sign)))
  ;; from sbcl:src;code;float.lisp
  #+sbcl (let* ((bits (sb-kernel::single-float-bits (abs float)))
                (exp (ldb sb-vm:single-float-exponent-byte bits))
                (sig (ldb sb-vm:single-float-significand-byte bits))
                (sign (minusp (float-sign float))))
           (values sig exp sign))
  #-(or ccl sbcl) (error "NYI: raw-deconstruct-single-float"))

(defun raw-deconstruct-double-float (float)
  (etypecase float
    (single-float (setf float (float float 1.0d0)))
    (double-float ))
  #+ccl (multiple-value-bind (hi lo exp sign) (ccl::%integer-decode-double-float float)
          (values (logior (ash hi 28) lo) exp (minusp sign)))
  #+sbcl (let* ((abs (abs float))
                (hi (sb-kernel::double-float-high-bits abs))
                (lo (sb-kernel::double-float-low-bits abs))
                (exp (ldb sb-vm:double-float-exponent-byte hi))
                ;(sig (ldb sb-vm:double-float-significand-byte hi))
                (sign (minusp (float-sign float))))
           (values
            (logior (ash (logior (ldb sb-vm:double-float-significand-byte hi)
                                 sb-vm:double-float-hidden-bit)
                         32)
                    lo)
            exp sign))
  #-(or ccl sbcl) (error "NYI: raw-deconstruct-double-float"))


(defun ieee-754-32-float-to-integer (float)
  (cond ((= float single-float-negative-infinity)
         #xff800000)
        ((= float single-float-positive-infinity)
         #x7f800000)
        ;; allow for sbcl inability to compile code with nan constants 
        (#-sbcl (eql float single-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7fc00000)
        ((= float 0.0f0)
         (if (minusp (float-sign float)) #x80000000 #x00000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-deconstruct-single-float float)
           (if (zerop exponent)
             (logior (if sign #x80000000 0)
                     (logand fraction #x007fffff))
             (logior (if sign #x80000000 0)
                     (ash exponent 23)
                     (logand fraction #x007fffff)))))))

(defun ieee-754-64-float-to-integer (float)
  (cond ((= float double-float-negative-infinity)
         #xfff0000000000000)
        ((= float double-float-positive-infinity)
         #x7ff0000000000000)
        ;; allow for sbcl inability to compile code with nan constants                                                                                
        (#-sbcl (eql float double-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7ff8000000000000)        
        ((= float 0.0d0)
         (if (minusp (float-sign float)) #x8000000000000000 #x0000000000000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-deconstruct-double-float float)
           (if (zerop exponent)
             (logior (if sign #x8000000000000000 0)
                     (logand fraction #x000fffffffffffff))
             (logior (if sign #x8000000000000000 0)
                     (ash exponent 52)
                     (logand fraction #x000fffffffffffff)))))))


;;;
;;; stream/buffer codecs

(defun decode-float-64-bytes (get-byte)
  (declare (type (function () (unsigned-byte 8)) get-byte)
           (dynamic-extent get-byte))
  (let ((value 0))
    (declare (type (unsigned-byte 64) value))
    (dotimes (i 8)
      (setf value (+ (ash value 8) (funcall get-byte))))
    (ieee-754-64-integer-to-float value)))

(defun stream-read-float-64 (stream)
  (multiple-value-bind (function arg) (stream-reader stream)
    (declare (type (function (t) (or (unsigned-byte 8) null)) function))
    (flet ((get-byte ()
             (or (funcall function arg)
                 (error 'end-of-file :stream stream))))
      (decode-float-64-bytes #'get-byte))))
                  
(defun buffer-get-float-64 (buffer position)
  (assert-argument-type buffer-get-float-64 buffer byte-buffer)
  (assert-condition (and (typep position 'fixnum) (<= (+ position 8) (length buffer)))
                    buffer-get-float-64 "value overflows buffer: (~s + ~s), ~s"
                    position 8 (length buffer))
  (flet ((get-byte ()
           (aref buffer (shiftf position (1+ position)))))
    (declare (dynamic-extent #'get-byte)
             (ftype (function () (unsigned-byte 8)) get-byte))
    (values (decode-float-64-bytes #'get-byte) position)))



(defun decode-float-32-bytes (get-byte)
  (declare (type (function () (unsigned-byte 8)) get-byte)
           (dynamic-extent get-byte))
  (let ((value 0))
    (declare (type (unsigned-byte 64) value))
    (dotimes (i 4)
      (setf value (+ (ash value 8) (funcall get-byte))))
    (ieee-754-32-integer-to-float value)))

(defun stream-read-float-32 (stream)
  (multiple-value-bind (function arg) (stream-reader stream)
    (declare (type (function (t) (or (unsigned-byte 8) null)) function))
    (flet ((get-byte ()
             (or (funcall function arg)
                 (error 'end-of-file :stream stream))))
      (decode-float-32-bytes #'get-byte))))

(defun buffer-get-float-32 (buffer position)
  (assert-argument-type buffer-get-float-32 buffer byte-buffer)
  (assert-condition (and (typep position 'fixnum) (<= (+ position 4) (length buffer)))
                    buffer-get-float-32 "value overflows buffer: (~s + ~s), ~s"
                    position 4 (length buffer))
  (flet ((get-byte ()
           (aref buffer (shiftf position (1+ position)))))
    (declare (dynamic-extent #'get-byte)
             (ftype (function () (unsigned-byte 8)) get-byte))
    (values (decode-float-32-bytes #'get-byte) position)))



(defun encode-float-64-bytes (put-byte value)
  (declare (type (function ((unsigned-byte 8)) t) put-byte)
           (dynamic-extent put-byte))
  (let ((int-value (ieee-754-64-float-to-integer value))
        (bit-offset 56))
    (declare (type (unsigned-byte 64) int-value))
    (assert  (typep int-value '(unsigned-byte 64)) ()
             'type-error :datum int-value :expected-type '(unsigned-byte 64))
    (dotimes (i 8)
      (funcall put-byte (ldb (byte 8 bit-offset) int-value))
      (decf bit-offset 8))))

(defun stream-write-float-64 (stream value)
  (multiple-value-bind (function arg) (stream-writer stream)
    (declare (type (function (t (unsigned-byte 8)) t) function))
    (flet ((put-byte (byte)
             (declare (type (unsigned-byte 8) byte))
             (funcall function arg byte)))
      (declare (dynamic-extent #'put-byte)
               (ftype (function ((unsigned-byte 8)) t) put-byte))
      (encode-float-64-bytes #'put-byte value)
      value)))

(defun buffer-set-float-64 (buffer value position)
  (assert-argument-type buffer-set-float-64 buffer byte-buffer)
  (flet ((put-byte (byte)
           (setf (aref buffer (shiftf position (1+ position))) byte)
           (values)))
    (declare (dynamic-extent #'put-byte)
             (ftype (function ((unsigned-byte 8)) (values)) put-byte))
    (encode-float-64-bytes #'put-byte value)
    position))



(defun encode-float-32-bytes (put-byte value)
  (declare (type (function ((unsigned-byte 8)) t) put-byte)
           (dynamic-extent put-byte))
  (let ((int-value (ieee-754-32-float-to-integer value)))
    (declare (type (unsigned-byte 32) int-value))
    (assert  (typep int-value '(unsigned-byte 32)) ()
             'type-error :datum int-value :expected-type '(unsigned-byte 32))
    (funcall put-byte (ldb (byte 8 24) int-value))
    (funcall put-byte (ldb (byte 8 16) int-value))
    (funcall put-byte (ldb (byte 8 8) int-value))
    (funcall put-byte (ldb (byte 8 0) int-value))))

(defun stream-write-float-32 (stream value)
  (multiple-value-bind (function arg) (stream-writer stream)
    (declare (type (function (t (unsigned-byte 8)) t) function))
    (flet ((put-byte (byte)
             (declare (type (unsigned-byte 8) byte))
             (funcall function arg byte)))
      (declare (dynamic-extent #'put-byte)
               (ftype (function ((unsigned-byte 8)) t) put-byte))
      (encode-float-32-bytes #'put-byte value)
      value)))

(defun buffer-set-float-32 (buffer value position)
  (assert-argument-type buffer-set-float-32 buffer byte-buffer)
  (flet ((put-byte (byte)
           (setf (aref buffer (shiftf position (1+ position))) byte)
           (values)))
    (declare (dynamic-extent #'put-byte)
             (ftype (function ((unsigned-byte 8)) (values)) put-byte))
    (encode-float-32-bytes #'put-byte value)
    position))


