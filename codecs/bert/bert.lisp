;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines a BERT encoding for the `de.setf.utility.codecs.bert` library."
  
  (:copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (description "The BERT codec implements the 'Binary ERlang Term' ([BERT-RPC](http://bert-rpc.org/)) data
 interchange format with respect to unigned byte arrays and binary streams. It is analogous to the BERT
 implementation in (CLERIC)[http://github.com/flambard/CLERIC], but implements symmetric coding/decoding
 operators and targets pre-allocated storage for both encoding cases.

 The immediate purpose is to support RDF/BERT, which requires just a subset of the full BERT codec set:
 * nil
 * atom 
 * true
 * false
 * dict
 * time
 * list
 * float
 * string"))

(defpackage :de.setf.utility.codecs.bert (:use ))

(defparameter +tag.binary-external+ 0)


;; string accessors encode as UTF8

(macrolet ((def-codec (length-bit-count tag)
             (let* ((bit-count-string (princ-to-string length-bit-count))
                    (reader-name (cons-symbol :de.setf.utility.codecs.bert :stream-read-string- bit-count-string))
                    (writer-name (cons-symbol :de.setf.utility.codecs.bert :stream-write-string- bit-count-string))
                    (utf8-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-string-utf8- bit-count-string))
                    (utf8-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-string-utf8- bit-count-string))
                    (getter-name (cons-symbol :de.setf.utility.codecs.bert :buffer-get-string- bit-count-string))
                    (setter-name (cons-symbol :de.setf.utility.codecs.bert :buffer-set-string- bit-count-string))
                    (utf8-getter-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-utf8- bit-count-string))
                    (utf8-setter-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-utf8- bit-count-string)))
               `(progn
                  (defun ,reader-name (stream)
                    (,utf8-reader-name stream))
                  (defun ,writer-name (stream string)
                    (de.setf.utility.codecs:stream-write-unsigned-byte-8 stream ,tag)
                    (,utf8-writer-name stream string))

                  (defun ,getter-name (buffer &optional (position 0))
                    (,utf8-getter-name buffer position))
                  (defun ,setter-name (buffer string &optional (position 0))
                    (de.setf.utility.codecs:buffer-set-unsigned-byte-8 buffer ,tag position)
                    (,utf8-setter-name buffer string (1+ position)))))))
  
  (def-codec 8 +tag.binary-external+)
  (def-codec 16 +tag.binary-external+))


;;; atom accessors are string-like, but w/o the utf encoding

(macrolet ((def-codec (length-bit-count tag)
             (let* ((bit-count-string (princ-to-string length-bit-count))
                    (reader-name (cons-symbol :de.setf.utility.codecs.bert :stream-read-atom- bit-count-string))
                    (writer-name (cons-symbol :de.setf.utility.codecs.bert :stream-write-atom- bit-count-string))
                    (iso-reader-name (cons-symbol :de.setf.utility.codecs :stream-read-string-iso- bit-count-string))
                    (iso-writer-name (cons-symbol :de.setf.utility.codecs :stream-write-string-iso- bit-count-string))
                    (getter-name (cons-symbol :de.setf.utility.codecs.bert :buffer-get-atom- bit-count-string))
                    (setter-name (cons-symbol :de.setf.utility.codecs.bert :buffer-set-atom- bit-count-string))
                    (iso-getter-name (cons-symbol :de.setf.utility.codecs :buffer-get-string-iso- bit-count-string))
                    (iso-setter-name (cons-symbol :de.setf.utility.codecs :buffer-set-string-iso- bit-count-string)))
               `(progn
                  (defun ,reader-name (stream)
                    (intern (,iso-reader-name stream) *package*))
                  (defun ,writer-name (stream value)
                    (let ((string (symbol-name value)))
                      (de.setf.utility.codecs:stream-write-unsigned-byte-8 stream ,tag)
                      (,iso-writer-name stream string)))

                  (defun ,getter-name (buffer &optional (position 0))
                    (multiple-value-bind (string position)
                                         (,iso-getter-name buffer position)
                      (values (intern string *package*) position)))
                  (defun ,setter-name (buffer value &optional (position 0))
                    (let ((string (symbol-name value)))
                      (de.setf.utility.codecs:buffer-set-unsigned-byte-8 buffer ,tag position)
                      (,iso-setter-name buffer string (1+ position))))))))
  
  (def-codec 8 +tag.binary-external+)
  (def-codec 16 +tag.binary-external+))



