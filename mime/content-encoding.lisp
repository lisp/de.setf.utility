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
from de.setf.xml suffice.

The decoding permits surrogate pairs in utf-8 under the control of *utf8-surrogates-allowed*.
The encoding always encodes 4-byte values for the surrogates.
(see http://unicode.org/faq/utf_bom.html#utf8-4)"))


(defparameter *content-encodings* (make-hash-table ))
(defparameter *utf8-iso8859-allowed* nil
  "When true, permit anomalous iso-8859 bytes (eg >= #xf1) in utf-8")
(defparameter *utf8-surrogates-allowed* t
  "When true, allow and decode surrogate pairs in utf-8")


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

(define-condition simple-encoding-error (simple-type-error)
  ()
  (:default-initargs
      :format-control "The character ~S ~@[(code x~2,'0x) ~]cannot be encoded as ~a."))
(defun simple-encoding-error (&key datum expected-type encoding)
  (error 'simple-encoding-error :datum datum :expected-type expected-type
         :format-arguments (list datum (typecase datum (character (char-code datum))) encoding)))
;;; (dsu:simple-encoding-error :datum #\c :expected-type 'string :encoding t)
;;; (dsu:simple-encoding-error :datum 1 :expected-type 'string :encoding t)

(define-condition simple-decoding-error (simple-type-error)
  ()
  (:default-initargs
      :format-control "The code x~2,'0x cannot be decoded as ~a."))
(defun simple-decoding-error (&key datum expected-type encoding)
  (error 'simple-decoding-error :datum datum :expected-type expected-type
         :format-arguments (list datum encoding)))
;;; (dsu:simple-decoding-error :datum 1 :expected-type 'string :encoding t)

(def-class-constructor content-encoding
  (:method ((name string) &rest initargs)
    (declare (dynamic-extent initargs))
    (if (equal name "")
      nil
      (apply #'content-encoding (or (find-symbol (string-upcase name) :keyword)
                                    (error "Invalid character encoding: ~s." name))
             initargs)))
  (:method ((keyword symbol) &rest initargs)
    (declare (dynamic-extent initargs))
    (flet ((define-encoding (encoding)
             ;; check the type
             (setf encoding (content-encoding encoding))
             ;; define it
             (setf (content-encoding (content-encoding-name encoding)) encoding)))
      (typecase keyword
        (null nil)
        (keyword
         ;; iff additional arguments follow the initial keyword, make the instance
         ;; otherwise - for a single keyword, treat it as an encoding designator
         (if initargs
           (define-encoding (apply #'make-instance *class.content-encoding* keyword initargs))
           (content-encoding (or (gethash keyword *content-encodings*)
                                 (error "Invalid character encoding: ~s." keyword)))))
        (t
         (define-encoding (apply #'make-instance keyword initargs)))))))


(defun (setf content-encoding) (encoding name)
  (when (gethash name *content-encodings*)
    (warn "redefining encoding: ~s." name))
  (setf (gethash name *content-encodings*) encoding))

(defmethod content-encoding-name ((encoding null))
  nil)


(flet ((utf-8-encode (char put-byte destination)
         ;; ecode surrogates as a four-byte sequence
         (declare (optimize (speed 3) (safety 0)))
         (macrolet ((emit (code) `(funcall put-byte destination ,code)))
           (let ((code (char-code char)))
             (declare (type (mod #x110000) code))
             (cond ((< code #x80)
                    (emit code))
                   ((< code #x800)
                    (emit (logior #b11000000 (ash code -6)))
                    (emit (logior #b10000000 (logand code #b00111111))))
                   ((< code #x10000)
                    (emit (logior #b11100000 (ash code -12)))
                    (emit (logior #b10000000 (logand (ash code -6) #b00111111)))
                    (emit (logior #b10000000 (logand code #b00111111))))
                   (t
                    (emit (logior #b11110000 (ash code -18)))
                    (emit (logior #b10000000 (logand (ash code -12) #b00111111)))
                    (emit (logior #b10000000 (logand (ash code -6) #b00111111)))
                    (emit (logior #b10000000 (logand code #b00111111))))))))
       (utf-8-decode (get-byte source)
         ;; decode two-element surrogates
         (flet ((read-byte-code ()
                  (or (funcall get-byte source)
                      (return-from utf-8-decode nil))))
           (declare (ftype (function () fixnum) read-byte-code)
                    (optimize (speed 3) (safety 0)))
           (flet ((read-char-code ( &aux (byte1 0))
                    (declare (type fixnum byte1))
                    (setf byte1 (read-byte-code))
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
                          (*utf8-iso8859-allowed*
                           byte1)
                          (t
                           (simple-decoding-error :datum byte1 :encoding :utf-8)))))
             (let ((code (read-char-code)))
               (cond ((< code #xd800)
                      (code-char code))
                     ((>= code #xe000)  ;; above surrogates
                      (code-char code))
                     ((and (< code #xdc00) *utf8-surrogates-allowed*)
                      (let ((low-code (read-char-code)))
                        (cond ((<= #xdc00 low-code #xdfff)
                               (code-char (+ #x10000 (+ (ash (logand code #x03ff) 10) (logand #x03ff low-code)))))
                              (t ;; not a low surrogate
                               (simple-decoding-error :datum (cons code low-code) :encoding :utf-8)))))
                     (t
                      (simple-decoding-error :datum code :encoding :utf-8)))))))
       (utf8-code-point-size (char)
         (let ((code (char-code char)))
             (declare (type (mod #x110000) code))
             (cond ((< code #x80)    1)
                   ((< code #x800)   2)
                   ((< code #x10000) 3)
                   (t                4)))))
  (content-encoding :name :utf-8
                    :encoded-code-point-size #'utf8-code-point-size
                    :byte-decoder #'utf-8-decode
                    :byte-encoder #'utf-8-encode
                    :documentation "http://en.wikipedia.org/wiki/Utf-8"))


(flet ((iso-8859-1-encode (char put-byte destination)
         (let ((code (char-code char)))
           (declare ;(type (mod #x100) code)
                    ;; permit assertion to run
                    (optimize (speed 3) (safety 0)))
           (unless (< code #x100)
             (simple-encoding-error :datum char :encoding :iso-8859-1))
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
  (:method ((type mime:*/*))
    ;; compute the codec's for the respective character set
    (compute-charset-codecs (mime-type-charset type)))
  (:method ((charset null))
    ;; absent a charset, or for application/octet-stream, fall-back to
    ;; the codecs for iso-8859-1
    (compute-charset-codecs :iso-8859-1))
  (:method ((charset string))
    (compute-charset-codecs (content-encoding charset)))
  (:method ((charset symbol))
    (compute-charset-codecs (content-encoding charset)))
  (:method ((encoding content-encoding))
    (values (content-encoding-byte-decoder encoding)
            (content-encoding-byte-encoder encoding)
            (content-encoding-encoded-code-point-size encoding))))


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

;;; test 128 - 255 encoding
(let ((string (map 'string 'code-char #(83 112 114 225 118 99 101 32
                                        112 114 111 106 101 107 116 117
                                        44 32 118 101 100 111 117 99
                                        237 32 112 114 111 106 101 107
                                        116 117 46))))
  (assert (equalp (encode-string string :utf-8)
                  #(83 112 114 195 161 118 99 101
                    32 112 114 111 106 101 107 116
                    117 44 32 118 101 100 111 117
                    99 195 173 32 112 114 111 106
                    101 107 116 117 46))
          () "Invalid utf encoding result"))
