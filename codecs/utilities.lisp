;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)
#+digitool
(:documentation
  "This file defines utilities for the `de.setf.utility.codecs` library."
  
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

(macrolet ((def-signed-byte (bit-count)
             (let ((name (cons-symbol :de.setf.utility.codecs :sign-byte- (prin1-to-string  bit-count)))
                   (max-positive (1- (expt 2 (1- bit-count))))
                   (mask (1- (expt 2 bit-count))))
             `(progn (declaim (inline ,name))
                     (defun ,name (byte)
                       ,(format nil "Convert unsigned to signed ~s-bit byte." bit-count)
                       (if (> byte ,max-positive)          ;  convert
                         (- (logxor ,mask (1- byte)))
                         byte))))))
  (def-signed-byte 8)
  (def-signed-byte 16)
  (def-signed-byte 32)
  (def-signed-byte 64))

(defun unsigned-byte-8 (datum)
  (logand datum #xff))

(define-compiler-macro unsigned-byte-8 (datum)
  `(logand ,datum #xff))


(defun sign-byte (value bit-count)
  (let ((max-positive (1- (expt 2 (1- bit-count))))
        (mask (1- (expt 2 bit-count))))
    (if (> value max-positive)          ;  convert
      (- (logxor mask (1- value)))
      value)))

(define-compiler-macro sign-byte (&whole form value bit-count)
  (if (integerp bit-count)
    (let ((max-positive (1- (expt 2 (1- bit-count))))
          (mask (1- (expt 2 bit-count))))
      `(if (> ,value ,max-positive)          ;  convert
         (- (logxor ,mask (1- ,value)))
         ,value))
    form))


(defun ensure-buffer-length (buffer new-length)
  (let ((size (array-dimension buffer 0)))
    (unless (>= size new-length)
      (setf size (+ new-length size))
      (setf buffer (adjust-array buffer size)))
    buffer))

(defun cons-length (x &optional (length 0))
  (declare (fixnum length))
  (if (consp x)
    (cons-length (rest x) (1+ length))
    length))


;;; trivial methods absent runtime-specifica
;;; no eof suppression as it should not happen in mid-term
#-(or ccl sbcl)
(defmethod stream-reader ((stream t))
  (values #'stream-read-byte stream))

#+sbcl
(defmethod stream-reader ((stream t))
  (values #'read-byte stream))

#-(or ccl sbcl)
(defmethod stream-writer ((stream t))
  (values #'stream-write-byte stream))

#+sbcl
(defmethod stream-writer ((stream t))
  (values #'(lambda (stream byte)
              ;; loud tracing
              ;; (sb-posix:syslog 0 " [~3,'0d]" byte)
              (write-byte byte stream))
          stream))
