;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines a BERT/ETF encoding for the `de.setf.utility.codecs.bert` library."
  
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
  
  (:description "The ETF codec implements the 'Binary ERlang Term' ([BERT-RPC](http://bert-rpc.org/)) data
 interchange format with respect to unigned byte arrays and binary streams. It is analogous to the BERT
 implementation in (CLERIC)[http://github.com/flambard/CLERIC], but implements symmetric coding/decoding
 operators and targets pre-allocated storage for vector encoding.

 It supports the [BERT](http://bert-rpc.org/) term set, with additional
 [standard](http://erlang.org/doc/apps/erts/erl_ext_dist.html) terms in order to support
 [rspec](http://rspec.info/) through RPC.

 * integer
 * float
 * atom  (for symbols)
 * tuple (for vector values)
 * bytelist
 * list (for cl:list values and byte-buffer values shorter than 65536)
 * binary (for utf-8 encoded cl:string values)
 * nil (both cl:nil and a distinguished nil value)
 * booleans (distinguished true and false values)
 * dictionary
 * time
 * regex
 * string (not cl:string)

 Please note:
 * As languages model booleans incompatibly, they are not overloaded on t/nil.
 Two distinguished values serve as markers: the self-evaluating constants, etf:true and etf:false.
 * The Erlang and BERT specifications disappoint the reader in their treatment of binary/string terms.
 This implementation follows the BERT implementation (@1.1.2), in that, string_ext indicates a
 byte sequence, not a string, while binary_ext indicates an utf8-coded string. As per the ETF
 specification, byte sequences longer than 65535 are encoded as lists, with reversible decoding
 left to the application."))


;;;
;;; type tags per http://erlang.org/doc/apps/erts/erl_ext_dist.html (@ v 131)
(defconstant etf:atom_cache_ref 82)
(defconstant etf:small_integer_ext 97)
(defconstant etf:integer_ext 98)
(defconstant etf:float_ext 99)
(defconstant etf:atom_ext 100)
(defconstant etf:reference_ext 101)
(defconstant etf:port_ext 102)
(defconstant etf:pid_ext 103)
(defconstant etf:small_tuple_ext 104)
(defconstant etf:large_tuple_ext 105)
(defconstant etf:nil_ext 106)
(defconstant etf:string_ext 107)
(defconstant etf:list_ext 108)
(defconstant etf:binary_ext 109)
(defconstant etf:small_big_ext 110)
(defconstant etf:large_big_ext 111)
(defconstant etf:new_reference_ext 114)
(defconstant etf:small_atom_ext 115)
(defconstant etf:fun_ext 117)
(defconstant etf:new_fun_ext 112)
(defconstant etf:export_ext 113)
(defconstant etf:bit_binary_ext 77)
(defconstant etf:new_float_ext 70)

(defconstant etf::version_number 131
  "The version indicator is prepended by encode-term to the encoded data stream/buffer and required
 by decode-term for decoding.")

(defconstant etf:nil 'etf:nil
  "The constant marker which designates a language-neutral NIL, distinct from nil_ext.")
(defconstant etf:true 'etf:true
  "The constant marker which designates a language-neutral true, distinct from t.")
(defconstant etf:false 'etf:false
  "The constant marker which designates a language-neutral false, distinct from nil.")

(defparameter *stream-decode-dispatch-table* (make-array 256 :initial-element nil)
  "A vector which dispatches from type tag to stream decoding operator.")

(defparameter *buffer-decode-dispatch-table* (make-array 256 :initial-element nil)
  "A vector which dispatches from type tag to stream decoding operator.")

(defparameter etf:*intern-operator* #'intern
  "A function, of two arguments, the atom name and a package designator, which maps atom names to symbols in the
 current etf:*package*. If the function returns nil - inorder to limit resources, the decoder signals an error.
 The default, intern, neither case folds nor constrains the domain. An alternative, find-symbol, would constrain
 atoms to pre-existing symbols.")

(defparameter etf:*package* (find-package :keyword)
  "The package into which atoms are interned. Ths initial value is the :keyword package. If rebound, the standard
 BERT keywords must be visible in order to decode 'standard' formats.")

(defparameter *write-small-atoms* nil
  "BERT.rb does not support them")

(defparameter etf:*buffer-set-term-hook* 'funcall
  "A function of four arguments, the base setter, the buffer, the term, and the current position,
 which is called for each invocation of buffer-set-term, passing generic-buffer-set-term as the base
 encoder. The initial value is funcall.")

(defparameter etf:*buffer-get-term-hook* 'funcall
  "A function of three arguments, the base getter, the buffer, and the current position,
 which is called for each invocation of buffer-get-term, passing an internal operator, buffer-get-tag-and-data,
 as the base getter. The initial value is funcall.")
  
(defparameter etf:*stream-write-term-hook* 'funcall
  "A function of three arguments, the base writer, the buffer, and the term
 which is called for each invocation of stream-write-term, passing generic-stream-write-term as the base
 encoder. The initial value is funcall.")

(defparameter etf:*stream-read-term-hook* 'funcall
  "A function of two arguments, the base reader, and the term,
 which is called for each invocation of stream-read-term, passing an internal operator, stream-read-tag-and-data,
 as the base reader. The initial value is funcall.")

;;; BERT 'standard' coding:
;;; as the time has no standard representation, the tagged vector suffices
;;; a dict<->hash-table coding makes sense only for large hash tables, it would require
;;; on-the-fly coding to avoid consing the entire map. if there's an immediate meed there is
;;; code in the thrift codecs.

(defun etf::bert-stream-read-term-hook (continuation stream)
  (let ((object (funcall continuation stream)))
    (if (vectorp object)
      (case (length object)
        (2
         (if (eq (aref object 0) :|bert|)
           (case (aref object 1)
             (:|nil| etf:nil)
             (:|true| etf:true)
             (:|false| etf:false))
           object))
        (3
         (if (and (eq (aref object 0) :|bert|) (eq (aref object 1) :|dict|))
           (loop for entry in (aref object 2)
                 collect (cons (aref entry 0) (aref entry 1)))
           object))
        (t object))
      object)))

(defun etf::bert-stream-write-term-hook (continuation stream object)
   (funcall continuation stream
            (typecase object
              (cons
               (if (and (consp (car object)) (not (listp (cdar object))))
                 ;; encode a dictionary
                 (vector :|bert| :|dict| (loop for (key . value) in object collect (vector key value)))
                 object))
              (symbol
               (case object
                 (etf:nil #(:|bert| :|nil|))
                 (etf:true #(:|bert| :|true|))
                 (etf:false #(:|bert| :|false|))
                 (t object)))
              (t
               object))))


(defun etf::bert-buffer-get-term-hook (continuation buffer position)
  (multiple-value-bind (object position)
                       (funcall continuation buffer position)
    (values (if (vectorp object)
              (case (length object)
                (2
                 (if (eq (aref object 0) :|bert|)
                   (case (aref object 1)
                     (:|nil| etf:nil)
                     (:|true| etf:true)
                     (:|false| etf:false))
                   object))
                (3
                 (if (and (eq (aref object 0) :|bert|) (eq (aref object 1) :|dict|))
                   (loop for entry in (aref object 2)
                         collect (cons (aref entry 0) (aref entry 1)))
                   object))
                (t object))
              object)
            position)))

(defun etf::bert-buffer-set-term-hook (continuation buffer object position)
   (funcall continuation buffer
            (typecase object
              (cons
               (if (and (consp (car object)) (not (listp (cdar object))))
                 ;; encode a dictionary
                 (vector :|bert| :|dict| (loop for (key . value) in object collect (vector key value)))
                 object))
              (symbol
               (case object
                 (etf:nil #(:|bert| :|nil|))
                 (etf:true #(:|bert| :|true|))
                 (etf:false #(:|bert| :|false|))
                 (t object)))
              (t
               object))
            position))

;;;
;;; stream reading

(defun etf:stream-read-term (stream)
  (flet ((stream-read-tag-and-data (stream)
           (let ((tag (etf::stream-read-unsigned-byte-8 stream)))
             (funcall (or (aref *stream-decode-dispatch-table* tag)
                          #'(lambda (stream) (error "Invalid or unsupported tag from stream: ~s; ~s." tag stream)))
                      stream))))
    (funcall etf::*stream-read-term-hook* #'stream-read-tag-and-data stream)))

(defun etf::stream-read-atom (stream)
  (let ((symbol-name (etf::stream-read-string-iso-16 stream)))
    (or (funcall etf:*intern-operator* symbol-name etf:*package*)
        (error "Invalid atom name: ~s." symbol-name))))

(defun etf::stream-read-float (stream)
  "unpack a 31-byte string and decode it as a float"
  (let ((float-string (make-array 31 :element-type 'base-char :initial-element #\space)))
    (dotimes (i 31)
      (setf (schar float-string i) (code-char (etf::stream-read-unsigned-byte-8 stream))))
    (de.setf.utility.meta:parse-float float-string)))

(defun etf::stream-read-integer (stream)
  (etf::stream-read-signed-byte-32 stream))

(defun etf::stream-read-large-big-integer (stream)
  (let* ((byte-count (etf::stream-read-unsigned-byte-32 stream))
         (sign (etf::stream-read-unsigned-byte-8 stream))
         (bit-count (* byte-count 8))
         (value 0))
    (do ((term-position 0 (+ term-position 8)))
        ((>= term-position bit-count))
      (setf (ldb (byte 8 term-position) value) (etf::stream-read-unsigned-byte-8 stream)))
    (if (zerop sign) value (- value))))

(defun etf::stream-read-large-tuple (stream)
  (let* ((count (etf::stream-read-unsigned-byte-32 stream))
         (tuple (make-array count)))
    (dotimes (i count)
      (setf (aref tuple i) (etf::stream-read-term stream)))
    tuple))

(defun etf::stream-read-list (stream)
  (let* ((count (etf::stream-read-unsigned-byte-32 stream)))
    (collect-list (collect :last last :predicate nil)
      (dotimes (i count)
        (collect (etf::stream-read-term stream)))
      (setf (last) (etf::stream-read-term stream)))))

(defun etf::stream-read-new-float (stream)
  (etf::stream-read-float-64 stream))

(defun etf::stream-read-nil (stream)
  (declare (ignore stream))
  nil)

(defun etf::stream-read-small-atom (stream)
  (let ((symbol-name (etf::stream-read-string-iso-8 stream)))
    (or (funcall etf:*intern-operator* symbol-name etf:*package*)
        (error "Invalid atom name: ~s." symbol-name))))

(defun etf::stream-read-small-big-integer (stream)
  (let* ((byte-count (etf::stream-read-unsigned-byte-8 stream))
         (sign (etf::stream-read-unsigned-byte-8 stream))
         (bit-count (* byte-count 8))
         (value 0))
    (do ((term-position 0 (+ term-position 8)))
        ((>= term-position bit-count))
      (setf (ldb (byte 8 term-position) value) (etf::stream-read-unsigned-byte-8 stream)))
    (if (zerop sign) value (- value))))

(defun etf::stream-read-small-integer (stream)
  (etf::stream-read-unsigned-byte-8 stream))

(defun etf::stream-read-small-tuple (stream)
  (let* ((count (etf::stream-read-unsigned-byte-8 stream))
         (tuple (make-array count)))
    (dotimes (i count)
      (setf (aref tuple i) (etf::stream-read-term stream)))
    tuple))

(defun etf::stream-read-vector-16 (stream)
  (let* ((size (etf::stream-read-unsigned-byte-16 stream))
         (buffer (make-array size :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)))

(defun etf::stream-read-string-32 (stream)
  (etf::stream-read-string-utf8-32 stream))


;;;
;;; stream writing

(defgeneric generic-stream-write-term (stream term)
  (:argument-precedence-order term stream)

  (:method (stream (term cons))
    (etf::stream-write-list stream term))

  (:method (stream (term float))
    (etf::stream-write-new-float stream term))

  (:method (stream (term integer))
    (typecase term
      ((unsigned-byte 8)
       (etf::stream-write-small-integer stream term))
      ((signed-byte 32)
       (etf::stream-write-integer stream term))
      (t
       ;; leave further distinction until the bits/bytes are counted
       (etf::stream-write-big-integer stream term))))

  (:method (stream (term null))
    (etf::stream-write-nil stream term))

  (:method (stream (term rational))
    (generic-stream-write-term stream (float term)))

  (:method (stream (term string))
    (etf::stream-write-string-32 stream term))

  (:method (stream (term symbol))
    "Emit an atom for the symbol; distinguish true and false."
    (if (and (< (length (symbol-name term)) 256)
             *write-small-atoms*)
      (etf::stream-write-small-atom stream term)
      (etf::stream-write-atom stream term)))

  (:method (stream (term vector))
    (if (typep term 'byte-buffer)
      (if (< (length term) 65536) 
        (etf::stream-write-vector-16 stream term)
        (etf::stream-write-vector-list stream term))
      (if (< (length term) 256)
        (etf::stream-write-small-tuple stream term)
        (etf::stream-write-large-tuple stream term))))

  (:method (stream (term function))
    (funcall term stream)))

(defun etf:stream-write-term (stream term)
  (funcall etf::*stream-write-term-hook* #'generic-stream-write-term stream term))


(defun etf::stream-write-atom (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:atom_ext)
  (etf::stream-write-string-iso-16 stream (symbol-name term)))

(defun etf::stream-write-integer (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:integer_ext)
  (etf::stream-write-signed-byte-32 stream term))

(defun etf::stream-write-big-integer (stream term)
  (let* ((abs (abs term))
         (sign (if (= abs term) 0 1))
         (byte-count (ceiling (integer-length abs) 8))
         (bit-count (* byte-count 8)))
    (cond ((> byte-count 255)
           (etf::stream-write-unsigned-byte-8 stream etf:large_big_ext)
           (etf::stream-write-unsigned-byte-32 stream byte-count))
          (t
           (etf::stream-write-unsigned-byte-8 stream etf:small_big_ext)
           (etf::stream-write-unsigned-byte-8 stream byte-count)))
    (etf::stream-write-unsigned-byte-8 stream sign)
    (do ((position 0 (+ position 8)))
        ((>= position bit-count))
      (etf::stream-write-unsigned-byte-8 stream (ldb (byte 8 position) term)))))
    

(defun etf::stream-write-large-tuple (stream term)
  (let ((count (length term)))
    (etf::stream-write-unsigned-byte-8 stream etf:large_tuple_ext)
    (etf::stream-write-unsigned-byte-32 stream count)
    (dotimes (i count)
      (etf:stream-write-term stream (aref term i)))))

(defun etf::stream-write-list (stream term)
  (let ((count (cons-length term 0)))
    (etf::stream-write-unsigned-byte-8 stream etf:list_ext)
    (etf::stream-write-unsigned-byte-32 stream count)
    (loop (cond ((consp term)
                 (etf:stream-write-term stream (pop term)))
                (t
                 (etf:stream-write-term stream term)
                 (return))))))

(defun etf::stream-write-new-float (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:new_float_ext)
  (etf::stream-write-float-64 stream term))

(defun etf::stream-write-nil (stream term)
  (declare (ignore term))
  (etf::stream-write-unsigned-byte-8 stream etf:nil_ext)
  nil)

(defun etf::stream-write-small-atom (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:small_atom_ext)
  (etf::stream-write-string-iso-8 stream (symbol-name term)))

(defun etf::stream-write-small-integer (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:small_integer_ext)
  (etf::stream-write-unsigned-byte-8 stream term))

(defun etf::stream-write-small-tuple (stream term)
  (let ((count (length term)))
    (etf::stream-write-unsigned-byte-8 stream etf:small_tuple_ext)
    (etf::stream-write-unsigned-byte-8 stream count)
    (dotimes (i count)
      (etf:stream-write-term stream (aref term i)))))

(defun etf::stream-write-vector-16 (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:string_ext)
  (etf::stream-write-unsigned-byte-16 stream (length term))
  (write-sequence term stream))

(defun etf::stream-write-vector-list (stream term)
  (let ((size (length term)))
    (etf::stream-write-unsigned-byte-8 stream etf:list_ext)
    (etf::stream-write-unsigned-byte-32 stream size)
    (loop for element across term
          do (etf::stream-write-small-integer stream element))
    (etf::stream-write-nil stream nil)))

(defun etf::stream-write-string-32 (stream term)
  (etf::stream-write-unsigned-byte-8 stream etf:binary_ext)
  (etf::stream-write-string-utf8-32 stream term))


;;;
;;; buffer getters

(defun etf:buffer-get-term (buffer position)
  (flet ((buffer-get-tag-and-data (buffer position)
           (let* ((tag (etf::buffer-get-unsigned-byte-8 buffer position))
                  (getter (or (aref *buffer-decode-dispatch-table* tag)
                              (error "Invalid tag from buffer: ~s; ~s." tag buffer))))
             (funcall getter buffer (1+ position)))))
    (funcall etf::*buffer-get-term-hook* #'buffer-get-tag-and-data buffer position)))
         
(defun etf::buffer-get-atom (buffer position)
  (multiple-value-bind (symbol-name position)
                       (etf::buffer-get-string-iso-16 buffer position)
    (values (or (funcall etf:*intern-operator* symbol-name etf:*package*)
                (error "Invalid atom name: ~s." symbol-name))
            position)))

(defun etf::buffer-get-float (buffer position)
  "unpack a 31-byte string and decode it as a float"
  (let ((float-string (make-string 31 :initial-element #\space)))
    (dotimes (i 31)
      (setf (schar float-string i) (code-char (aref buffer position)))
      (incf position))
    (values (with-standard-io-syntax (let ((*read-eval* t)) (read-from-string float-string)))
            position)))

(defun etf::buffer-get-integer (buffer position)
  (etf::buffer-get-signed-byte-32 buffer position))

(defun etf::buffer-get-large-big-integer (buffer position)
  (let* ((byte-count (etf::buffer-get-unsigned-byte-32 buffer position))
         (sign (etf::buffer-get-unsigned-byte-8 buffer (1+ position)))
         (bit-count (* byte-count 8))
         (value 0))
    (incf position 5)
    (do ((term-position 0 (+ term-position 8)))
        ((>= term-position bit-count))
      (setf (ldb (byte 8 term-position) value) (etf::buffer-get-unsigned-byte-8 buffer position))
      (incf position))
    (values (if (zerop sign) value (- value)) position)))

(defun etf::buffer-get-large-tuple (buffer position)
  (let ((count 0)
        (element nil)
        (tuple nil))
    (multiple-value-setq (count position)
      (etf::buffer-get-unsigned-byte-32 buffer position))
    (setf tuple (make-array count))
    (dotimes (i count)
      (multiple-value-setq (element position)
        (etf:buffer-get-term buffer position))
      (setf (aref tuple i) element))
    (values tuple position)))

(defun etf::buffer-get-list (buffer position)
  (let ((count 0)
        (element nil))
    (multiple-value-setq (count position)
      (etf::buffer-get-unsigned-byte-32 buffer position))
    (let ((list (collect-list (collect :last last :predicate nil)
                  (dotimes (i count)
                    (multiple-value-setq (element position)
                      (etf:buffer-get-term buffer position))
                    (collect element))
                  (multiple-value-setq (element position)
                    (etf:buffer-get-term buffer position))
                  (setf (last) element))))
      (values list position))))

(defun etf::buffer-get-nil (buffer position)
  (declare (ignore buffer))
  (values nil position))

(defun etf::buffer-get-new-float (buffer position)
  (etf::buffer-get-float-64 buffer position))

(defun etf::buffer-get-small-atom (buffer position)
  (multiple-value-bind (symbol-name position)
                       (etf::buffer-get-string-iso-8 buffer position)
    (values (or (funcall etf:*intern-operator* symbol-name etf:*package*)
                (error "Invalid atom name: ~s." symbol-name))
            position)))

(defun etf::buffer-get-small-big-integer (buffer position)
  (let* ((byte-count (etf::buffer-get-unsigned-byte-8 buffer position))
         (sign (etf::buffer-get-unsigned-byte-8 buffer (1+ position)))
         (bit-count (* byte-count 8))
         (value 0))
    (incf position 2)
    (do ((term-position 0 (+ term-position 8)))
        ((>= term-position bit-count))
      (setf (ldb (byte 8 term-position) value) (etf::buffer-get-unsigned-byte-8 buffer position))
      (incf position))
    (values (if (= sign 1) (- value) value) position)))

(defun etf::buffer-get-small-integer (buffer position)
  (etf::buffer-get-unsigned-byte-8 buffer position))

(defun etf::buffer-get-small-tuple (buffer position)
  (let ((count 0)
        (element nil)
        (tuple nil))
    (multiple-value-setq (count position)
      (etf::buffer-get-unsigned-byte-8 buffer position))
    (setf tuple (make-array count))
    (dotimes (i count)
      (multiple-value-setq (element position)
        (etf:buffer-get-term buffer position))
      (setf (aref tuple i) element))
    (values tuple position)))

(defun etf::buffer-get-vector-16 (buffer position)
  (let* ((size (etf::buffer-get-unsigned-byte-16 buffer position))
         (end (+ position 2 size)))
    (assert-condition (<= end (length buffer))
                      etf::buffer-get-vector-16 "vector overflows buffer: (~s + ~s), ~s"
                      (+ position 2) size (length buffer))
    (values (subseq buffer (+ position 2) end)
            end)))

(defun etf::buffer-get-string-32 (buffer position)
  (etf::buffer-get-string-utf8-32 buffer position))


;;;
;;; buffer setters

(defgeneric generic-buffer-set-term (buffer term position)
  (:argument-precedence-order term buffer position)

  (:method (buffer (term cons) position)
    (etf::buffer-set-list buffer term position))

  (:method (buffer (term float) position)
    (etf::buffer-set-new-float buffer term position))

  (:method (buffer (term integer) position)
    (typecase term
      ((unsigned-byte 8)
       (etf::buffer-set-small-integer buffer term position))
      ((signed-byte 32)
       (etf::buffer-set-integer buffer term position))
      (t
       (etf::buffer-set-big-integer buffer term position))))

  (:method (buffer (term null) position)
    (etf::buffer-set-nil buffer term position))

  (:method (buffer (term string) position)
    (let* ((size (size-string term (load-time-value (content-encoding :utf-8)))))
      (etf::buffer-set-string-32 buffer term position size)))

  (:method (buffer (term symbol) position)
    (if (and (< (length (symbol-name term)) 256)
             *write-small-atoms*)
      (etf::buffer-set-small-atom buffer term position)
      (etf::buffer-set-atom buffer term position)))

  (:method (buffer (term vector) position)
    (if (typep term 'byte-buffer)
      (if (< (length term) 65536)
        (etf::buffer-set-vector-16 buffer term position)
        (etf::buffer-set-vector-list buffer term position))
      (if (< (length term) 256)
        (etf::buffer-set-small-tuple buffer term position)
        (etf::buffer-set-large-tuple buffer term position)))))

(defun etf:buffer-set-term (buffer term position)
  (funcall etf::*buffer-set-term-hook* #'generic-buffer-set-term buffer term position))


(defun etf::buffer-set-atom (buffer term  position)
  (let* ((string (symbol-name term))
         (new-length (+ position 1 2 (length string))))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:atom_ext position)
    (etf::buffer-set-string-iso-16 buffer string (1+ position))
    (values buffer new-length)))

(defun etf::buffer-set-big-integer (buffer term position)
  (let* ((abs (abs term))
         (sign (if (= abs term) 0 1))
         (byte-count (ceiling (integer-length abs) 8))
         (length-byte-count (if (> byte-count 255) 4 1))
         (bit-count (* byte-count 8))
         (new-length (+ position 1 length-byte-count 1 byte-count)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (setf position
          (cond ((> byte-count 255)
                 (etf::buffer-set-unsigned-byte-8 buffer etf:large_big_ext position)
                 (etf::buffer-set-unsigned-byte-32 buffer byte-count (1+ position)))
                (t
                 (etf::buffer-set-unsigned-byte-8 buffer etf:small_big_ext position)
                 (etf::buffer-set-unsigned-byte-8 buffer byte-count (1+ position)))))
    (setf position (etf::buffer-set-unsigned-byte-8 buffer sign position))
    (do ((term-position 0 (+ term-position 8)))
        ((>= term-position bit-count))
      (setf position (etf::buffer-set-unsigned-byte-8 buffer (ldb (byte 8 term-position) term) position)))
    (values buffer new-length)))

(defun etf::buffer-set-integer (buffer term position)
  (let ((new-length (+ position 1 4)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:integer_ext position)
    (etf::buffer-set-signed-byte-32 buffer term (1+ position))
    (values buffer new-length)))

(defun etf::buffer-set-large-tuple (buffer term position)
  (let ((count (length term))
        (new-length (+ position 1 4)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:large_tuple_ext position)
    (etf::buffer-set-unsigned-byte-32 buffer count (1+ position))
    (setf position new-length)
    (dotimes (i count)
      (multiple-value-setq (buffer position)
        (etf:buffer-set-term buffer (aref term i) position)))
    (values buffer position)))

(defun etf::buffer-set-list (buffer term position)
  (let ((count (cons-length term))
        (new-length (+ position 1 4)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:list_ext position)
    (etf::buffer-set-unsigned-byte-32 buffer count (1+ position))
    (incf position 5)
    (loop (cond ((consp term)
                 (multiple-value-setq (buffer position)
                   (etf:buffer-set-term buffer (pop term) position)))
                (t
                 (multiple-value-setq (buffer position)
                   (etf:buffer-set-term buffer term position))
                 (return))))
    (values buffer position)))

(defun etf::buffer-set-new-float (buffer term position)
  (let ((new-length (+ position 1 8)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:new_float_ext position)
    (etf::buffer-set-float-64 buffer term (1+ position))
    (values buffer new-length)))

(defun etf::buffer-set-nil (buffer term position)
  (declare (ignore term))
  (let ((new-length (+ position 1)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:nil_ext position)
    (values buffer new-length)))

(defun etf::buffer-set-small-atom (buffer term position)
  (let* ((string (symbol-name term))
         (new-length (+ position 1 1 (length string))))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:small_atom_ext position)
    (etf::buffer-set-string-iso-8 buffer string (1+ position))
    (values buffer new-length)))

(defun etf::buffer-set-small-integer (buffer term position)
  (let ((new-length (+ position 1 1)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:small_integer_ext position)
    (etf::buffer-set-unsigned-byte-8 buffer term (1+ position))
    (values buffer new-length)))

(defun etf::buffer-set-small-tuple (buffer term position)
  (let ((count (length term))
        (new-length (+ position 1 1)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:small_tuple_ext position)
    (etf::buffer-set-unsigned-byte-8 buffer count (1+ position))
    (setf position new-length)
    (dotimes (i count)
      (multiple-value-setq (buffer position)
        (etf:buffer-set-term buffer (aref term i) position)))
    (values buffer position)))

(defun etf::buffer-set-vector-16 (buffer term position)
  (let* ((size (length term))
         (new-length (+ position 1 2 size)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:string_ext position)
    (etf::buffer-set-unsigned-byte-16 buffer size (1+ position))
    (replace buffer term :start1 (+ position 3) :end1 new-length)
    (values buffer new-length)))

(defun etf::buffer-set-vector-list (buffer term position)
  ;; adjust the buffer size from that computed for the argument vector and
  ;; unroll the list coding for small integer elements
  (let* ((size (length term))
         (new-length (+ position 1 4 (* size 2))))
    (setf buffer (ensure-buffer-length buffer new-length))
    (setf position (etf::buffer-set-unsigned-byte-8 buffer etf:list_ext position))
    (setf position (etf::buffer-set-unsigned-byte-32 buffer size position))
    (loop for element across term
          do (setf position (etf::buffer-set-unsigned-byte-8 buffer etf:small_integer_ext position)
                   position (etf::buffer-set-unsigned-byte-8 buffer element position)))
    (etf::buffer-set-unsigned-byte-8 buffer etf:nil_ext position)))

(defun etf::buffer-set-string-32 (buffer term position &optional (size nil))
  (unless size
    (setf size (size-string term (load-time-value (content-encoding :utf-8)))))
  (let* ((new-length (+ position 1 4 size)))
    (setf buffer (ensure-buffer-length buffer new-length))
    (etf::buffer-set-unsigned-byte-8 buffer etf:binary_ext position)
    (etf::buffer-set-string-utf8-32 buffer term (1+ position) size)
    (values buffer new-length)))


;;;
;;; interface operators
;;; - etf:encode-term (term destination)
;;; - etf:decode-term (source)


(defgeneric etf:encode-term (term destination)
  (:method ((term t) (stream stream)) 
    (etf::stream-write-unsigned-byte-8 stream etf::version_number)
    (etf:stream-write-term stream term))

  (:method ((term t) (buffer vector))
    (setf buffer (ensure-buffer-length buffer 1))
    (etf::buffer-set-unsigned-byte-8 buffer etf::version_number 0)
    (etf:buffer-set-term buffer term 1)))

(defun etf:encode-bert-term (term destination)
  (let ((etf:*buffer-set-term-hook* #'etf::bert-buffer-set-term-hook)
        (etf:*stream-write-term-hook* #'etf::bert-stream-write-term-hook))
    (etf:encode-term term destination)))


(defgeneric etf:decode-term (source &key package)
  (:method ((stream stream) &key ((:package etf:*package*) etf:*package*))
    (assert-argument-type etf:decode-term etf:*package* package)
    (let ((version-number (etf::stream-read-unsigned-byte-8 stream)))
      (assert (eql version-number etf::version_number) ()
              "Invalid ETF version: ~s." version-number))
    (etf::stream-read-term stream))

  (:method ((buffer vector) &key ((:package etf:*package*) etf:*package*))
    (assert-argument-type etf:decode-term etf:*package* package)
    (let ((version-number (etf::buffer-get-unsigned-byte-8 buffer 0)))
      (assert (eql version-number etf::version_number) ()
              "Invalid ETF version: ~s." version-number))
    (etf:buffer-get-term buffer 1)))

(defun etf:decode-bert-term (source &rest args &key package)
  (declare (ignore package) (dynamic-extent args))
  (let ((etf:*buffer-get-term-hook* #'etf::bert-buffer-get-term-hook)
        (etf:*stream-read-term-hook* #'etf::bert-stream-read-term-hook))
    (apply #'etf:decode-term source args)))


(defun etf:term-to-binary (object)
  (etf:encode-term object (make-array 128 :element-type '(unsigned-byte 8))))


(defun etf:binary-to-term (buffer)
  (etf:decode-term buffer))



;;;
;;; initialize dispatch tables

(let ((dispatch-table *stream-decode-dispatch-table*))
  (fill dispatch-table nil)
  (macrolet ((set-dispatch (tag function)
               `(setf (aref dispatch-table ,tag) (function ,function)))
             (set-dispatches (&rest dispatches)
               `(progn ,@(loop for (tag function) in dispatches
                               collect `(set-dispatch ,tag ,function)))))
    (set-dispatches (etf:atom_ext etf::stream-read-atom)
                    (etf:binary_ext etf::stream-read-string-32)
                    (etf:float_ext etf::stream-read-float)
                    (etf:integer_ext etf::stream-read-integer)
                    (etf:large_big_ext etf::stream-read-large-big-integer)
                    (etf:large_tuple_ext etf::stream-read-large-tuple)
                    (etf:list_ext etf::stream-read-list)
                    (etf:new_float_ext etf::stream-read-new-float)
                    (etf:nil_ext etf::stream-read-nil)
                    (etf:small_atom_ext etf::stream-read-small-atom)
                    (etf:small_big_ext etf::stream-read-small-big-integer)
                    (etf:small_integer_ext etf::stream-read-small-integer)
                    (etf:small_tuple_ext etf::stream-read-small-tuple)
                    (etf:string_ext etf::stream-read-vector-16))))


(let ((dispatch-table *buffer-decode-dispatch-table*))
  (fill dispatch-table nil)
  (macrolet ((set-dispatch (tag function)
               `(setf (aref dispatch-table ,tag) (function ,function)))
             (set-dispatches (&rest dispatches)
               `(progn ,@(loop for (tag function) in dispatches
                               collect `(set-dispatch ,tag ,function)))))
    (set-dispatches (etf:atom_ext etf::buffer-get-atom)
                    (etf:binary_ext etf::buffer-get-string-32)
                    (etf:float_ext etf::buffer-get-float)
                    (etf:integer_ext etf::buffer-get-integer)
                    (etf:large_big_ext etf::buffer-get-large-big-integer)
                    (etf:large_tuple_ext etf::buffer-get-large-tuple)
                    (etf:list_ext etf::buffer-get-list)
                    (etf:new_float_ext etf::buffer-get-new-float)
                    (etf:nil_ext etf::buffer-get-nil)
                    (etf:small_atom_ext etf::buffer-get-small-atom)
                    (etf:small_big_ext etf::buffer-get-small-big-integer)
                    (etf:small_integer_ext etf::buffer-get-small-integer)
                    (etf:small_tuple_ext etf::buffer-get-small-tuple)
                    (etf:string_ext etf::buffer-get-vector-16))))


#+(or)
(progn
  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-term (print (etf::encode-term `(131 1 ,(map-into (make-string 5) #'code-char (list 97 98 99 100 8364))
                                                 ,(make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(7 6 5 4 3 2 1 0))
                                                 a (1 . 2) 1.2d0 nil #(1 2) ((a . 1) (b . 2))
                                                 ,(expt 2 64) -1 -255
                                                 etf:nil etf:true etf:false) buffer))))

  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-bert-term (print (etf::encode-bert-term `(1 ,(map-into (make-string 5) #'code-char (list 97 98 99 100 8364))
                                                 ,(make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(7 6 5 4 3 2 1 0))
                                                 a (1 . 2) 1.2d0 nil #(1 2) ((a . 1) (b . 2))
                                                 etf:nil etf:true etf:false) buffer))))

  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-term (print (etf::encode-term `(,(expt 2 64)) buffer))))


  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-term (print (etf::encode-term #(:|reply| #(DE.SETF.UTILITY.ETF:TRUE (3))) buffer))))

  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::buffer-set-unsigned-byte-8 buffer 131 0))

  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-term (print (etf::encode-term 1.2d0 buffer))))

  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (etf::decode-term (print (etf::encode-term `( -18) buffer))))

  (defun time-encode (value &key (count 1024)
                            (buffer (make-array 32 :element-type '(unsigned-byte 8))))
    (time (dotimes (x count)
            (setf buffer (etf:buffer-set-term buffer value 0)))))

  (time-encode '(1 2))
  (time-encode most-positive-double-float)
  (time-encode 1)
  (time-encode most-positive-fixnum)
  (time-encode (+ most-positive-fixnum 1))
  (time-encode nil)
  (time-encode (make-string 32 :initial-element #\a))
  (time-encode 'asdf)
  (time-encode #(1 2 3 4))
  ;; 9.582 seconds, 3K for the first buffer extension
  (time-encode (loop repeat 32 collect (make-string 32 :initial-element #\a)))


  (defun time-cleric-encode (value &key (count 1024))
    (time (dotimes (x count)
            (cleric:encode value))))
  (time-cleric-encode '(1 2))
  (time-cleric-encode most-positive-double-float)
  (time-cleric-encode 1)
  (time-cleric-encode most-positive-fixnum)
  (time-cleric-encode (+ most-positive-fixnum 1))
  (time-cleric-encode nil)
  (time-cleric-encode (make-string 24 :initial-element #\a))
  (time-cleric-encode 'asdf)
  (time-cleric-encode (bert::tuple 1 2 3 4))
  ;; all relatively harmless. until one starts large collections
  ;; mcl 5.2, ppcg5 14.362 second 133MB
  (time-cleric-encode (loop repeat 32 collect (make-string 32 :initial-element #\a)))
  )

 
