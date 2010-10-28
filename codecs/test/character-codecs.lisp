;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file tests string codecs for the `de.setf.utility.codecs` library."
  
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

(test:test codecs.buffer-character-codecs
  (let* ((buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (good-strings (list "" "01234567"
                             (let ((i (char-code #\a)))
                               (map-into (make-string (- (length buffer) 4)) #'(lambda () (values (code-char i) (incf i)))))
                             (map 'string #'code-char `(#x0000  #x00FF #x0100 #x01FF #x0200
                                                        ,(if (> char-code-limit 65536) #x10FFFF #xffff))))))
    (dolist (length-bit-count '(8 16 32) t)
      (let* ((bit-count-string (princ-to-string length-bit-count))
             (getter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-iso- bit-count-string))
             (getter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-utf8- bit-count-string))
             (setter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-iso- bit-count-string))
             (setter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-utf8- bit-count-string)))
        (unless (dolist (string good-strings t)
                  (funcall setter-iso-name buffer string 0)
                  (when (every #'(lambda (c) (<= (char-code c) 255)) string)
                    (unless (equal (funcall getter-iso-name buffer 0) buffer)
                      (return nil)))
                  (funcall setter-utf8-name buffer string 0)
                  (unless (equal (funcall getter-utf8-name buffer 0) string)
                    (return nil))))))))

(test:test codecs.buffer-character-codecs.errors
  "Test the effect of access at the end: over-run should signal a type error."
  (let* ((buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
         (string "01234567"))
    (dolist (length-bit-count '(8 16 32) t)
      (let* ((bit-count-string (princ-to-string length-bit-count))
             (length-byte-count (/ length-bit-count 8))
             (getter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-iso- bit-count-string))
             (getter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-utf8- bit-count-string))
             (setter-iso-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-iso- bit-count-string))
             (setter-utf8-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-utf8- bit-count-string)))
        (unless (and (typep (nth-value 1 (ignore-errors (funcall setter-iso-name string buffer
                                                                 (1+ (- (length buffer) (+ (length string) length-byte-count))))))
                             'type-error)
                     (funcall setter-iso-name buffer string (- (length buffer) (+ (length string) length-byte-count)))
                     (equal (funcall getter-iso-name buffer (- (length buffer) (+ (length string) length-byte-count)))
                             string)
                     (typep (nth-value 1 (ignore-errors (funcall setter-utf8-name string buffer
                                                                 (1+ (- (length buffer) (+ (length string) length-byte-count))))))
                             'type-error)
                     (funcall setter-utf8-name buffer string (- (length buffer) (+ (length string) length-byte-count)))
                     (equal (funcall getter-utf8-name buffer (- (length buffer) (+ (length string) length-byte-count)))
                             string))
          (return nil))))))


(test:test codecs.stream-character-codecs
  (let ((stream (make-instance 'vector-io-stream))
        (strings (list "" "01234567"
                            (let ((i (char-code #\a)))
                              (map-into (make-string 32) #'(lambda () (values (code-char i) (incf i)))))
                            (map 'string #'code-char `(#x0000  #x00FF #x0100 #x01FF #x0200
                                                       ,(if (> char-code-limit 65536) #x10FFFF #xffff))))))
    (labels ((do-codec (encoder decoder value)
               (stream-position stream 0)
               (funcall encoder stream value)
               (stream-position stream 0)
               (eql (funcall decoder stream) value)))
      (dolist (length-bit-count '(8 16 32) t)
        (let* ((bit-count-string (princ-to-string length-bit-count))
               (reader-iso-name (cons-symbol :de.setf.utility.codecs :stream-read-string-iso- bit-count-string))
               (reader-utf8-name (cons-symbol :de.setf.utility.codecs :stream-read-string-utf8- bit-count-string))
               (writer-iso-name (cons-symbol :de.setf.utility.codecs :stream-write-string-iso- bit-count-string))
               (writer-utf8-name (cons-symbol :de.setf.utility.codecs :stream-write-string-utf8- bit-count-string)))
          (unless (dolist (string strings t)
                    (unless (and (do-codec writer-iso-name reader-iso-name string)
                                 (do-codec writer-utf8-name reader-utf8-name string))
                      (return nil)))))))))
  

(test:test codecs.stream-string-sized
   (let ((stream (make-instance 'vector-io-stream)))
    (labels ((do-codec (encoder decoder value &optional (size (length value)))
               (stream-position stream 0)
               (funcall encoder stream value size)
               (stream-position stream 0)
               (equal (funcall decoder stream size)
                      (concatenate 'string (make-string (- size (length value)) :initial-element #\space) value))))
      (dolist (spec '("" ("" 10) "asdf" ("asdf" 10))
                     t)
        (unless (apply #'do-codec #'stream-write-string-iso-sized #'stream-read-string-iso-sized
                       (if (listp spec) spec (list spec)))
          (return nil))))))


