;;; -*- Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)


(:documentation "This file defines stream/buffer character set codecs for the 'de.setf.utility.mime.' library"
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description
  "The encoding names are taken from the [IANA document](http://www.iana.org/assignments/character-sets)
 with character set names. The codec logic is from de.setf.xml. There are alternatives, but they were not
 suitable

- net.common-lisp.babel concerns sequence codecs
- clozure cl's l1-unicode includes stream codecs, but would mean extracting them from
  a much more extensive library.

As the (ultimate) goal is to en/decode to/from network buffers, just the stream operators
from de.setf.xml suffice."))


(defparameter *content-encodings* (make-hash-table ))

(defclass content-encoding ()
  ((name
    :initarg :name :initform (error "name required.")
    :reader content-encoding-name
    :type keyword)
   (encoded-code-point-size
    :initform (error "encoded-code-point-size required.") :initarg :encoded-code-point-size
    :reader content-encoding-encoded-code-point-size
    :type (or integer function null)
    :documentation "Specified the number of octets required to encode a code point with this
 encoding. If that is a constant, this is the byte count. Otherwise a function to compute the size from a
 character.")
   (byte-decoder
    :initarg :byte-decoder :initform (error "byte-decoder required")
    :reader content-encoding-byte-decoder
    :documentation "A function of two arguments (source byte-reader),
 where the byte-reader applied to the source returns an unsigned byte,
 or NIL, for EOF")
   (byte-encoder
    :initarg :byte-encoder :initform (error "byte-encoder required")
    :reader content-encoding-byte-encoder
    :documentation "A function of two arguments (source byte-reader),
 where the byte-reader applied to the source returns an unsigned byte,
 or NIL, for EOF")
   (documentation
    :initarg :documentation :initform nil
    :accessor content-encoding-documentation)))


(def-class-constructor content-encoding
  (:method ((keyword symbol) &rest initargs)
    (declare (dynamic-extent initargs))
    (flet ((define-encoding (encoding)
             ;; check the type
             (setf encoding (content-encoding encoding))
             ;; define it
             (setf (content-encoding (content-encoding-name encoding)) encoding)))
      (if (keywordp keyword)
        ;; iff additional arguments follow the initial keyword, make the instance
        ;; otherwise - for a single keyword, treat it as an encoding designator
        (if initargs
          (define-encoding (apply #'make-instance *class.content-encoding* keyword initargs))
          (content-encoding (or (gethash keyword *content-encodings*)
                                (error "Invalid character encoding: ~s." keyword))))
        (define-encoding (apply #'make-instance keyword initargs))))))


(defun (setf content-encoding) (encoding name)
  (when (gethash name *content-encodings*)
    (warn "redefining encoding: ~s." name))
  (setf (gethash name *content-encodings*) encoding))



(flet ((utf-8-encode (char put-byte destination)
         (declare (optimize (speed 3) (safety 0)))
         (macrolet ((emit (code) `(funcall put-byte destination ,code)))
           (let ((code (char-code char)))
             (declare (type (mod #x110000) code))
             (cond ((<= code 255)
                    (emit code))
                   ((<= code #x03ff)
                    (emit (logior #b11000000 (ash code -6)))
                    (emit (logior #b10000000 (logand code #b00111111))))
                   ((<= code #xffff)
                    (emit (logior #b11100000 (ash code -12)))
                    (emit (logior #b10000000 (logand (ash code -6) #b00111111)))
                    (emit (logior #b10000000 (logand code #b00111111))))
                   (t
                    (emit (logior #b111100000 (ash code -18)))
                    (emit (logior #b10000000 (logand (ash code -12) #b00111111)))
                    (emit (logior #b10000000 (logand (ash code -6) #b00111111)))
                    (emit (logior #b10000000 (logand code #b00111111))))))))
       (utf-8-decode (get-byte source &aux byte1)
         (flet ((read-byte-code ()
                  (or (funcall get-byte source)
                      (return-from utf-8-decode nil))))
           (declare (type fixnum byte1)
                    (ftype (function () fixnum) read-byte-code)
                    (optimize (speed 3) (safety 0)))
           (setf byte1 (read-byte-code))
           (code-char
            (cond ((= 0 (logand #x80 byte1))
                   byte1)
                  ((= #xc0 (logand #xe0 byte1))
                   (logior (ash (logand byte1 #x1f) 6)
                           (logand (read-byte-code) #x3f)))
                  ((= #xe0 (logand #xf0 byte1))
                   (logior (logior (ash (logand byte1 #x0f) 12)
                                   (ash (logand (read-byte-code) #x3f) 6))
                           (logand (read-byte-code) #x3f)))
                  ((= #xf0 (logand #xf8 byte1))
                   (logior (ash (logand #x07 byte1) 18)
                           (ash (logand #x3f (read-byte-code)) 12)
                           (ash (logand #x3f (read-byte-code)) 6)
                           (logand (read-byte-code) #x3f)))
                  (t
                   (error "Illegal UTF-8 data: x~2,'0x." byte1))))))
       (utf8-code-point-size (char)
         (let ((code (char-code char)))
             (declare (type (mod #x110000) code))
             (cond ((<= code 255)    1)
                   ((<= code #x03ff) 2)
                   ((<= code #xffff) 3)
                   (t                4)))))
  (content-encoding :name :utf-8
                    :encoded-code-point-size #'utf8-code-point-size
                    :byte-decoder #'utf-8-decode
                    :byte-encoder #'utf-8-encode
                    :documentation "http://en.wikipedia.org/wiki/Utf-8"))


(flet ((iso-8859-1-encode (char put-byte destination)
         (let ((code (char-code char)))
           (declare (type (mod #x100) code))
           (assert (< code #x100) () "Cannot be encoded as iso-8859-1: ~s" char)
           (funcall put-byte destination code)))
       (iso-8859-1-decode (get-byte source)
         (code-char (or (funcall get-byte source)
                        (return-from iso-8859-1-decode nil)))))
  (content-encoding :name :iso-8859-1
                    :encoded-code-point-size 1
                    :byte-decoder #'iso-8859-1-decode
                    :byte-encoder #'iso-8859-1-encode
                    :documentation " http://en.wikipedia.org/wiki/ISO/IEC_8859-1"))


(setf (content-encoding :us-ascii) :iso-8859-1)
(setf (content-encoding :ascii) :iso-8859-1)

;;; (eq (content-encoding :iso-8859-1) (content-encoding :us-ascii))


(defgeneric compute-charset-codecs (mime-type)
  (:method ((charset null))
    (compute-charset-codecs :iso-8859-1))
  (:method ((charset symbol))
    (compute-charset-codecs (content-encoding charset)))
  (:method ((type mime:*/*))
    (compute-charset-codecs (mime-type-charset type)))
  (:method ((encoding content-encoding))
    (values (content-encoding-byte-decoder encoding)
            (content-encoding-byte-encoder encoding))))


(defgeneric encode-string (string encoding)
  (:method ((string t) (encoding symbol))
    (encode-string string (content-encoding encoding)))

  (:method ((string string) (encoding content-encoding))
    (let* ((byte-sizer (content-encoding-encoded-code-point-size encoding))
           (encoder (content-encoding-byte-encoder encoding))
           (size (etypecase byte-sizer
                   (null (length string))
                   (integer (* (length string) byte-sizer))
                   (function (loop for char across string
                                   sum (funcall byte-sizer char)))))
           (result (make-array size :element-type '(unsigned-byte 8)))
           (position 0))
      (flet ((put-byte (buffer byte)
               (setf (aref buffer position) byte)
               (incf position)))
        (declare (dynamic-extent #'put-byte))
        (loop for char across string
              do (funcall encoder char #'put-byte result)))
      result)))

(defgeneric size-string (string encoding)
  (:method ((string t) (encoding symbol))
    (size-string string (content-encoding encoding)))

  (:method ((string string) (encoding content-encoding))
    (let* ((byte-sizer (content-encoding-encoded-code-point-size encoding)))
      (etypecase byte-sizer
        (null (length string))
        (integer (* (length string) byte-sizer))
        (function (loop for char across string
                        sum (funcall byte-sizer char)))))))

;;; (map 'string #'code-char (encode-string "asdf" :utf-8))
