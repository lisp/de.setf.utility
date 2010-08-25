;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)
#+digitool
(:documentation
  "This file defines byte codecs for the `de.setf.utility.codecs` library."
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.resource' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.resource' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.resource' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (description "Defines functions and compiler macros for unsigned/signed byte operations on streams and buffers.
 The operators implement the combinations for destination (stream / buffer), direction (read / write), and
 bit count (8, 16, 32, 64).

 In order to limit duplication and to optimize stream access, the core encode/decode operators are implemented
 in terms of accessor functions which yield the next byte to decode or accept the next byte to encode.
 These operators are paired with compiler-macros which unwrap the scaling loop for the respective byte size.

 The interface operators include both general operators, which accept a run-time size and size-specific operators.
 The latter generate type constraints and supply a constant byte size in order to enable size-specific compilation.
 The stream operators accept the stream and object to write, while the buffer operators accept an additional
 position argument and return the new position as an additional value."))




(defun decode-unsigned-byte (get-byte bit-count)
  (declare (type (function () (unsigned-byte 8)) get-byte)
           (dynamic-extent get-byte))
  (let* ((byte-count (ecase bit-count (8 1) (16 2) (32 4) (64 8)))
         (value 0))
    (declare (type unsigned-byte value) (type fixnum byte-count))
    (dotimes (i byte-count value)
      (setf value
            (if (= i 0) (funcall get-byte) (+ (ash value 8) (funcall get-byte)))))))

(defun decode-signed-byte (get-byte bit-count)
  (declare (dynamic-extent get-byte))
  (let ((value (decode-unsigned-byte get-byte bit-count)))
    (ecase bit-count
      (8 (sign-byte-8 value))
      (16 (sign-byte-16 value))
      (32 (sign-byte-32 value))
      (64 (sign-byte-64 value)))))

(define-compiler-macro decode-unsigned-byte (&whole form get-byte bit-count)
  (if (integerp bit-count)
    (let ((byte-count (ecase bit-count (8 1) (16 2) (32 4) (64 8)))
          (call-form (if (and (consp get-byte) (eq (first get-byte) 'function))
                       (list (second get-byte))
                       `(funcall ,get-byte))))
      `(let ((value 0))
         (declare (type unsigned-byte value))
         ,@(loop for i from 0 below byte-count
                 collect 
                 (if (= i 0)
                   `(setf value ,call-form)
                   `(setf value (+ (ash value 8) ,call-form))))
         value))
    form))

(define-compiler-macro decode-signed-byte (&whole form get-byte bit-count)
  (if (integerp bit-count)
    `(sign-byte (decode-unsigned-byte ,get-byte ,bit-count) ,bit-count)
    form))


(defun encode-byte (put-byte value bit-count)
  (declare (type (function ((unsigned-byte 8)) t) put-byte)
           (dynamic-extent put-byte))
  (let* ((byte-count (ecase bit-count (8 1) (16 2) (32 4) (64 8)))
         (bit-offset (ecase bit-count (8 0) (16 8) (32 24) (64 56))))
    (declare (type unsigned-byte value) (type fixnum byte-count bit-offset))
    (dotimes (i byte-count)
      (funcall put-byte (ldb (byte 8 bit-offset) value))
      (decf bit-offset 8))
    (values)))

(define-compiler-macro encode-byte (&whole form put-byte value bit-count)
  (if (integerp bit-count)
    (let ((byte-count (ecase bit-count (8 1) (16 2) (32 4) (64 8)))
          (bit-offset (ecase bit-count (8 0) (16 8) (32 24) (64 56))))
      `(let ((_::value ,value))
         (declare (type unsigned-byte _::value))
         ,@(loop for i from (1- byte-count) downto 0
                 collect (if (and (consp put-byte) (eq (first put-byte) 'function))
                           `(,(second put-byte) (ldb (byte 8 ,bit-offset) _::value))
                           `(funcall ,put-byte (ldb (byte 8 ,bit-offset) _::value)))
                 do (decf bit-offset 8))
         (values)))
    form))


(defun stream-read-unsigned-byte (stream bit-count)
  (multiple-value-bind (function arg) (stream-reader stream)
    (flet ((get-byte ()
             (or (funcall function arg)
                 (error 'end-of-file :stream stream))))
      (declare (dynamic-extent #'get-byte)
               (ftype (function () (unsigned-byte 8)) get-byte))
      (decode-unsigned-byte #'get-byte bit-count))))

(defun stream-read-signed-byte (stream bit-count)
  (sign-byte (stream-read-unsigned-byte stream bit-count) bit-count))


(defun stream-write-signed-byte (stream value bit-count)
  (multiple-value-bind (function arg) (stream-writer stream)
    (let ((type (ecase bit-count (8 '(signed-byte 8)) (16 '(signed-byte 16)) (32 '(signed-byte 32)) (64 '(signed-byte 64)))))
      (flet ((put-byte (byte)
               (funcall function arg byte)))
        (declare (dynamic-extent #'put-byte)
                 (ftype (function ((unsigned-byte 8)) t) put-byte))
        (assert (typep value type) ()
                'type-error :datum value :expected-type type)
        (encode-byte #'put-byte value bit-count)
        value))))

(defun stream-write-unsigned-byte (stream value bit-count)
  (multiple-value-bind (function arg) (stream-writer stream)
    (let ((type (ecase bit-count (8 '(unsigned-byte 8)) (16 '(unsigned-byte 16)) (32 '(unsigned-byte 32)) (64 '(unsigned-byte 64)))))
      (flet ((put-byte (byte)
               (funcall function arg byte)))
        (declare (dynamic-extent #'put-byte)
                 (ftype (function ((unsigned-byte 8)) t) put-byte))
        (assert (typep value type) ()
                'type-error :datum value :expected-type type)
        (encode-byte #'put-byte value bit-count)
        value))))


(defun buffer-get-signed-byte (buffer bit-count position)
  (declare (type fixnum position))
  (flet ((get-byte ()
           (aref (aref buffer (shiftf position (1+ position))))))
      (declare (dynamic-extent #'get-byte)
               (ftype (function () (unsigned-byte 8)) get-byte))
      (values (decode-signed-byte #'get-byte bit-count) position)))

(defun buffer-get-unsigned-byte (buffer bit-count position)
  (flet ((get-byte ()
           (aref (aref buffer (shiftf position (1+ position))))))
      (declare (dynamic-extent #'get-byte)
               (ftype (function () (unsigned-byte 8)) get-byte))
      (values (decode-unsigned-byte #'get-byte bit-count) position)))


(defun buffer-set-signed-byte (buffer value bit-count position)
  (let ((type (ecase bit-count (8 '(signed-byte 8)) (16 '(signed-byte 16)) (32 '(signed-byte 32)) (64 '(signed-byte 64)))))
    (flet ((put-byte (byte)
             (setf (aref buffer (shiftf position (1+ position))) byte)
             (values)))
      (declare (dynamic-extent #'put-byte)
               (ftype (function ((unsigned-byte 8)) (values)) put-byte))
      (assert (typep value type) ()
              'type-error :datum value :expected-type type)
      (encode-byte #'put-byte value bit-count)
      position)))

(defun buffer-set-unsigned-byte (buffer value bit-count position)
  (let ((type (ecase bit-count (8 '(unsigned-byte 8)) (16 '(unsigned-byte 16)) (32 '(unsigned-byte 32)) (64 '(unsigned-byte 64)))))
    (flet ((put-byte (byte)
             (setf (aref buffer (shiftf position (1+ position))) byte)
             (values)))
      (declare (dynamic-extent #'put-byte)
               (ftype (function ((unsigned-byte 8)) (values)) put-byte))
      (assert (typep value type) ()
              'type-error :datum value :expected-type type)
      (encode-byte #'put-byte value bit-count)
      position)))


(macrolet ((def-codec (bit-count)
             (let* ((bit-count-string (princ-to-string bit-count))
                    (byte-count (ecase bit-count (8 1) (16 2) (32 4) (64 8)))
                    (signed-type (ecase bit-count (8 '(signed-byte 8)) (16 '(signed-byte 16)) (32 '(signed-byte 32)) (64 '(signed-byte 64))))
                    (unsigned-type (ecase bit-count (8 '(unsigned-byte 8)) (16 '(unsigned-byte 16)) (32 '(unsigned-byte 32)) (64 '(unsigned-byte 64))))
                    (signed-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-signed-byte- bit-count-string))
                    (unsigned-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-unsigned-byte- bit-count-string))
                    (signed-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-signed-byte- bit-count-string))
                    (unsigned-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-unsigned-byte- bit-count-string))
                    (signed-getter-name (cons-symbol :de.setf.utility.codecs :buffer-get-signed-byte- bit-count-string))
                    (unsigned-getter-name (cons-symbol :de.setf.utility.codecs :buffer-get-unsigned-byte- bit-count-string))
                    (signed-setter-name (cons-symbol :de.setf.utility.codecs :buffer-set-signed-byte- bit-count-string))
                    (unsigned-setter-name (cons-symbol :de.setf.utility.codecs :buffer-set-unsigned-byte- bit-count-string)))
               
               `(progn
                  (defun ,signed-getter-name (buffer position)
                    (assert-argument-type ,signed-setter-name buffer byte-buffer)
                    (flet ((get-byte ()
                             (aref buffer (shiftf position (1+ position)))))
                      (declare (dynamic-extent #'get-byte)
                               (ftype (function () (unsigned-byte 8)) get-byte))
                      (values (decode-signed-byte #'get-byte ,bit-count) position)))
                  (defun ,unsigned-getter-name (buffer position)
                    (assert-argument-type ,signed-setter-name buffer byte-buffer)
                    (flet ((get-byte ()
                             (aref buffer (shiftf position (1+ position)))))
                      (declare (dynamic-extent #'get-byte)
                               (ftype (function () (unsigned-byte 8)) get-byte))
                      (values (decode-unsigned-byte #'get-byte ,bit-count) position)))
                  
                  (defun ,signed-setter-name (buffer value position)
                    (assert-argument-type ,signed-setter-name value ,signed-type)
                    (assert-argument-type ,signed-setter-name buffer byte-buffer)
                    (assert-condition (and (typep position 'fixnum) (<= (+ position ,byte-count) (length buffer)))
                                      ,signed-setter-name "value overflows buffer: (~s + ~s), ~s"
                                      position ,byte-count (length buffer))
                    (flet ((put-byte (byte)
                             (setf (aref buffer (shiftf position (1+ position))) byte)
                             (values)))
                      (declare (dynamic-extent #'put-byte)
                               (ftype (function ((unsigned-byte 8)) (values)) put-byte))
                      (encode-byte #'put-byte value ,bit-count)
                      position))
                  (defun ,unsigned-setter-name (buffer value position)
                    (assert-argument-type ,signed-setter-name value ,unsigned-type)
                    (assert-argument-type ,signed-setter-name buffer byte-buffer)
                    (assert-condition (and (typep position 'fixnum) (<= (+ position ,byte-count) (length buffer)))
                                      ,signed-setter-name "value overflows buffer: (~s + ~s), ~s"
                                      position ,byte-count (length buffer))
                    ,(if (eql bit-count 8)
                       `(progn (setf (aref buffer position) (logand value #xff))
                               (1+ position))
                       `(flet ((put-byte (byte)
                                 (declare (type byte-buffer buffer))
                                 (setf (aref buffer (shiftf position (1+ position))) byte)
                                 (values))
                               (simple-put-byte (byte)
                                 (declare (type simple-byte-buffer buffer))
                                 (setf (aref buffer (shiftf position (1+ position))) byte)
                                 (values)))
                          (declare (dynamic-extent #'put-byte #'simple-put-byte)
                                   (ftype (function ((unsigned-byte 8)) (values)) put-byte simple-put-byte))
                          (encode-byte (etypecase buffer
                                         (simple-byte-buffer #'simple-put-byte)
                                         (byte-buffer #'put-byte))
                                       value ,bit-count)
                          position)))
                  
                  (defun ,signed-reader-name (stream)
                    (multiple-value-bind (function arg) (stream-reader stream)
                      (declare (type (function (t) (or (unsigned-byte 8) null)) function))
                      (flet ((get-byte ()
                               (or (funcall function arg)
                                   (error 'end-of-file :stream stream))))
                        (declare (dynamic-extent #'get-byte)
                                 (ftype (function () (unsigned-byte 8)) get-byte))
                        (decode-signed-byte #'get-byte ,bit-count))))
                  (defun ,unsigned-reader-name (stream)
                    (multiple-value-bind (function arg) (stream-reader stream)
                      (declare (type (function (t) (or (unsigned-byte 8) null)) function))
                      (flet ((get-byte ()
                               (or (funcall function arg)
                                   (error 'end-of-file :stream stream))))
                        (declare (dynamic-extent #'get-byte)
                                 (ftype (function () (unsigned-byte 8)) get-byte))
                        (decode-unsigned-byte #'get-byte ,bit-count))))
                  
                  (defun ,signed-writer-name (stream value)
                    (declare (type ,signed-type value))
                    (assert-argument-type ,signed-writer-name value ,signed-type)
                    (multiple-value-bind (function arg) (stream-writer stream)
                      (declare (type (function (t (unsigned-byte 8)) t) function))
                      (flet ((put-byte (byte)
                               (declare (type (unsigned-byte 8) byte))
                               (funcall function arg byte)))
                        (declare (dynamic-extent #'put-byte)
                                 (ftype (function ((unsigned-byte 8)) t) put-byte))
                        (encode-byte #'put-byte value ,bit-count)
                        value)))
                  (defun ,unsigned-writer-name (stream value)
                    (declare (type ,unsigned-type value))
                    (assert-argument-type ,unsigned-writer-name value ,unsigned-type)
                    (multiple-value-bind (function arg) (stream-writer stream)
                      (declare (type (function (t (unsigned-byte 8)) t) function))
                      (flet ((put-byte (byte)
                               (declare (type (unsigned-byte 8) byte))
                               (funcall function arg byte)))
                        (declare (dynamic-extent #'put-byte)
                                 (ftype (function ((unsigned-byte 8)) t) put-byte))
                        (encode-byte #'put-byte value ,bit-count)
                        value)))))))
  (def-codec 8)
  (def-codec 16)
  (def-codec 32)
  (def-codec 64))
