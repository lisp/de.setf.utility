;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines string codecs for the `de.setf.utility.codecs` library."
  
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
  
  (:description "Defines functions and compiler macros for character sequence operations on streams and buffers.
 The operators implement the combinations for destination (stream / buffer), direction (read / write), string
 length bit count (8, 16, 32), and encoding (is05589 and utf8).

 The interface operators include both general operators, which accept a run-time size and size-specific operators.
 The latter generate type constraints and supply a constant byte size in order to enable size-specific compilation."))



(macrolet ((def-codec (length-bit-count)
             ;; for a given bit size fo the length field,
             ;; generate iso8859, utf8 stream and buffer operators
             
             (let* ((bit-count-string (princ-to-string length-bit-count))
                    (length-byte-count (ecase length-bit-count (8 1) (16 2) (32 4)))
                    (iso-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-string-iso- bit-count-string))
                    (utf8-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-string-utf8- bit-count-string))
                    (iso-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-string-iso- bit-count-string))
                    (utf8-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-string-utf8- bit-count-string))
                    (read-byte-name (cons-symbol :de.setf.utility.codecs :stream-read-unsigned-byte- bit-count-string))
                    (write-byte-name (cons-symbol :de.setf.utility.codecs :stream-write-unsigned-byte- bit-count-string))
                    (getter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-iso- bit-count-string))
                    (getter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-utf8- bit-count-string))
                    (setter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-iso- bit-count-string))
                    (setter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-utf8- bit-count-string))
                    (get-byte-name (cons-symbol :de.setf.utility.codecs :buffer-get-unsigned-byte- bit-count-string))
                    (set-byte-name (cons-symbol :de.setf.utility.codecs :buffer-set-unsigned-byte- bit-count-string)))

               `(progn
                  (defun ,getter-iso-name (buffer position)
                    (declare (type byte-buffer buffer))
                    (declare (type fixnum position))
                    (let* ((length (,get-byte-name buffer position)))
                      (declare (type fixnum length))
                      (incf position ,length-byte-count)
                      (if (plusp length)
                        (let ((result (make-array length :element-type +string-element-type+)))
                          (declare (type character-buffer result))
                          (assert-condition (<= (+ position length) (length buffer))
                                            ,getter-iso-name "string overflows buffer: (~s + ~s), ~s"
                                            position length (length buffer))
                          (dotimes (i length)
                            (setf (aref result i)
                                  (code-char (aref buffer position)))
                            (incf position))
                          (values result position))
                        (values "" position))))
                  (defun ,getter-utf8-name (buffer position)
                    (declare (type byte-buffer buffer))
                    (declare (type fixnum position))
                    (let* ((length (,get-byte-name buffer position))
                           (decoder (load-time-value (content-encoding-byte-decoder (content-encoding :utf-8)))))
                      (declare (type fixnum length))
                      (incf position ,length-byte-count)
                      (if (plusp length)
                        (let ((result (make-array length :element-type +string-element-type+ :adjustable t :fill-pointer 0))
                              (end (+ position length)))
                          (declare (type character-buffer result))
                          (assert-condition (<= end (length buffer))
                                            ,getter-utf8-name "string size overflows buffer: (~s + ~s), ~s"
                                            position length (length buffer))
                          (flet ((buffer-extract-byte (buffer)
                                   (declare (type byte-buffer buffer))
                                   (assert-condition (< position end)
                                                     ,getter-utf8-name "string decoding overflows own size: ~s, ~s"
                                                     position end)
                                   (aref buffer (shiftf position (1+ position)))))
                            (declare (dynamic-extent #'buffer-extract-byte))         ; just in case
                            (loop (when (>= position end) (return))
                                  (vector-push-extend (funcall decoder #'buffer-extract-byte buffer) result)))
                          (values result end))
                        (values "" position))))

                  (defun ,setter-iso-name (buffer value position)
                    (declare (type byte-buffer buffer))
                    (declare (type fixnum position)
                             (type string value))
                    (assert-argument-type ,setter-iso-name value string)   ; no remorse
                    (let* ((length (length value)))
                      (assert-condition (< length ,(expt 2 length-bit-count))
                                        ,setter-iso-name "String overflows the size constraint")
                      (assert-condition (and (typep position 'fixnum) (<= (+ position length ,length-byte-count) (length buffer)))
                                        ,setter-iso-name "value overflows buffer: (~s + ~s), ~s"
                                        position (+ length ,length-byte-count) (length buffer))
                      (setf position (,set-byte-name buffer length position))
                      (dotimes (i length)
                        ; (print (list length value buffer position i (aref value i)))
                        (setf (aref buffer position) (char-code (aref value i)))
                        (incf position))
                      (values position buffer)))
                  (defun ,setter-utf8-name (buffer value position  &optional (size nil))
                    (declare (type byte-buffer buffer))
                    (declare (type fixnum position)
                             (type string value))
                    (assert-argument-type ,setter-utf8-name buffer byte-buffer)
                    (assert-argument-type ,setter-utf8-name value string)
                    (let* ((length (length value))
                           (max-position 0)
                           (encoder (load-time-value (content-encoding-byte-encoder (content-encoding :utf-8))))
                           (size (or size (size-string value (load-time-value (content-encoding :utf-8))))))
                      (assert-condition (< size ,(expt 2 length-bit-count))
                                        ,setter-utf8-name "String overflows the size constraint")
                      (assert-condition (and (typep position 'fixnum) (<= (+ position size ,length-byte-count) (length buffer)))
                                        ,setter-utf8-name "value overflows buffer: (~s + ~s), ~s"
                                        position (+ size ,length-byte-count) (length buffer))
                      (setf position (,set-byte-name buffer size position))
                      (setf max-position (+ position size))
                      ; (print (list (list length size) value buffer (length buffer) max-position))
                      (flet ((buffer-insert-byte (buffer byte)
                               (declare (type byte-buffer buffer))
                               (declare (type (unsigned-byte 8) byte))
                               ;; check bounds here as it's finally the encoded positioning
                               (assert-condition (< position max-position)
                                                 ,setter-utf8-name "String overflows encoded size: ~s, ~s"
                                                 position max-position)
                               (setf (aref buffer position) byte)
                               (incf position)))
                        (declare (dynamic-extent #'buffer-insert-byte))    ; just in case
                        (dotimes (i length)        ; can't check bounds here either
                          ; (print (list length value buffer position i (aref value i)))
                          (funcall encoder (char value i) #'buffer-insert-byte buffer))
                        (values position buffer))))

                  (defun ,iso-reader-name (stream)
                    (multiple-value-bind (function arg) (stream-reader stream)
                      (declare (type (function (t) (or (unsigned-byte 8) null)) function))
                      (flet ((get-byte ()
                               (or (funcall function arg)
                                   (error 'end-of-file :stream stream))))
                        (declare (dynamic-extent #'get-byte)
                                 (ftype (function () (unsigned-byte 8)) get-byte))
                        (let ((length (,read-byte-name stream)))
                          (declare (type fixnum length))
                          (if (plusp length)
                            (let ((result (make-array length :element-type +string-element-type+)))
                              (declare (type simple-character-buffer result))
                              (dotimes (i length)
                                (setf (aref result i) (code-char (get-byte))))
                              result)
                            "")))))
                  (defun ,utf8-reader-name (stream)
                    (multiple-value-bind (function arg) (stream-reader stream)
                      (declare (type (function (t) (or (unsigned-byte 8) null)) function))
                      (let ((length (,read-byte-name stream))
                            (decoder (load-time-value (content-encoding-byte-decoder (content-encoding :utf-8)))))
                        (declare (type fixnum length))
                        (if (plusp length)
                          (let ((result (make-array length :element-type +string-element-type+ :adjustable t :fill-pointer 0)))
                            (declare (type character-buffer result))
                            (flet ((get-counted-byte (ignore)
                                     (declare (ignore ignore))
                                     (assert-condition (> length 0)
                                                       ,utf8-reader-name "string decoding overflows own size.")
                                     (decf length)
                                     (or (funcall function arg)
                                         (error 'end-of-file :stream stream))))
                              (declare (dynamic-extent #'get-counted-byte))
                              (loop (when (<= length 0) (return))
                                    (vector-push-extend (funcall decoder #'get-counted-byte nil) result)))
                            result)
                          ""))))
                  
                  (defun ,iso-writer-name (stream value)
                    (declare (type string value))
                    (assert-argument-type ,iso-writer-name value string)
                    (multiple-value-bind (function arg) (stream-writer stream)
                      (declare (type (function (t (unsigned-byte 8)) t) function))
                      (let* ((length (length value)))
                        (assert-condition (< length ,(expt 2 length-bit-count))
                                          ,iso-writer-name "String overflows the size constraint")
                        (,write-byte-name stream length)
                        (dotimes (i length)
                          (funcall function arg (char-code (aref value i))))
                        value)))
                  (defun ,utf8-writer-name (stream value &optional (size nil))
                    ,(format nil "Encode a string VALUE to the given STREAM.
                                  STREAM ; stream
                                  VALUE : (STRING ~d)
                                  SIZE : (or null fixnum) : the UTF8 encoded byte count. If null computed per encoding."
                             (1- (expt 2 length-bit-count)))
                    (declare (type string value))
                    (assert-argument-type ,utf8-writer-name value string)
                    (multiple-value-bind (function arg) (stream-writer stream)
                      (declare (type (function (t (unsigned-byte 8)) t) function))
                      (let* ((encoder (load-time-value (content-encoding-byte-encoder (content-encoding :utf-8))))
                             (size (or size (size-string value (load-time-value (content-encoding :utf-8))))))
                        (,write-byte-name stream size)
                        (dotimes (i (length value))
                          (funcall encoder (aref value i) function arg))
                        value)))))))

  (def-codec 8)
  (def-codec 16)
  (def-codec 32))

(defun stream-read-string-iso-sized (stream length)
  (multiple-value-bind (function arg) (stream-reader stream)
    (declare (type (function (t) (or (unsigned-byte 8) null)) function))
    (flet ((get-byte ()
             (or (funcall function arg)
                 (error 'end-of-file :stream stream))))
      (declare (dynamic-extent #'get-byte)
               (ftype (function () (unsigned-byte 8)) get-byte))
      (declare (type fixnum length))
      (if (plusp length)
        (let ((result (make-array length :element-type +string-element-type+)))
          (declare (type simple-character-buffer result))
          (dotimes (i length)
            (setf (aref result i) (code-char (get-byte))))
          result)
        ""))))

(defun stream-write-string-iso-sized (stream value size)
  (declare (type string value))
  (assert-argument-type stream-write-string-iso-sized value string)
  (multiple-value-bind (function arg) (stream-writer stream)
    (declare (type (function (t (unsigned-byte 8)) t) function))
    (let* ((length (length value)))
      (assert-condition (<= length size)
                        stream-write-string-iso-sized "String overflows the size constraint")
      (when (< length size)
        (dotimes (i (- size length)) (funcall function arg #.(char-code #\space))))
      (dotimes (i length)
        (funcall function arg (char-code (aref value i))))
      value)))
