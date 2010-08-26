;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the package definition for the codec module for the 'de.setf.utility' Common Lisp library.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; `de.setf.utility` is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the the GNU Lesser General Public License as published by the Free Software Foundation.
;;;
;;; `de.setf.utility` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with `de.setf.utility`, as `lgpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).


(in-package :cl-user)


(defpackage :de.setf.utility.codecs
  (:use )
  (:documentation "The API package for BERT-encoding operators.")
  (:export :buffer-get-float-32
           :buffer-get-float-64
           :buffer-get-signed-byte
           :buffer-get-signed-byte-8
           :buffer-get-signed-byte-16
           :buffer-get-signed-byte-32
           :buffer-get-signed-byte-64
           :buffer-get-string-iso-8
           :buffer-get-string-iso-16
           :buffer-get-string-iso-32
           :buffer-get-string-utf8-8
           :buffer-get-string-utf8-16
           :buffer-get-string-utf8-32
           :buffer-get-unsigned-byte
           :buffer-get-unsigned-byte-8
           :buffer-get-unsigned-byte-16
           :buffer-get-unsigned-byte-32
           :buffer-get-unsigned-byte-64
           :buffer-set-float-32
           :buffer-set-float-64
           :buffer-set-signed-byte
           :buffer-set-signed-byte-8
           :buffer-set-signed-byte-16
           :buffer-set-signed-byte-32
           :buffer-set-signed-byte-64
           :buffer-set-string-iso-8
           :buffer-set-string-iso-16
           :buffer-set-string-iso-32
           :buffer-set-string-utf8-8
           :buffer-set-string-utf8-16
           :buffer-set-string-utf8-32
           :buffer-set-unsigned-byte
           :buffer-set-unsigned-byte-8
           :buffer-set-unsigned-byte-16
           :buffer-set-unsigned-byte-32
           :buffer-set-unsigned-byte-64
           :sign-byte
           :sign-byte-8
           :sign-byte-16
           :sign-byte-32
           :sign-byte-64
           :stream-read-float-32
           :stream-read-float-64
           :stream-read-signed-byte
           :stream-read-signed-byte-8
           :stream-read-signed-byte-16
           :stream-read-signed-byte-32
           :stream-read-signed-byte-64
           :stream-read-string-iso-8
           :stream-read-string-iso-16
           :stream-read-string-iso-32
           :stream-read-string-iso-sized
           :stream-read-string-utf8-8
           :stream-read-string-utf8-16
           :stream-read-string-utf8-32
           :stream-read-unsigned-byte
           :stream-read-unsigned-byte-8
           :stream-read-unsigned-byte-16
           :stream-read-unsigned-byte-32
           :stream-read-unsigned-byte-64
           :stream-write-float-32
           :stream-write-float-64
           :stream-write-signed-byte
           :stream-write-signed-byte-8
           :stream-write-signed-byte-16
           :stream-write-signed-byte-32
           :stream-write-signed-byte-64
           :stream-write-string-iso-8
           :stream-write-string-iso-16
           :stream-write-string-iso-32
           :stream-write-string-iso-sized
           :stream-write-string-utf8-8
           :stream-write-string-utf8-16
           :stream-write-string-utf8-32
           :stream-write-unsigned-byte
           :stream-write-unsigned-byte-8
           :stream-write-unsigned-byte-16
           :stream-write-unsigned-byte-32
           :stream-write-unsigned-byte-64
           :unsigned-byte-8))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :de.setf.utility.codecs :de.setf.utility.implementation)

  #+clozure
  (import '(ccl:double-float-positive-infinity
            ccl:double-float-negative-infinity
            #+ccl-1.4 ccl:double-float-nan)
          :de.setf.utility.implementation)
  #+sbcl
  (import '(sb-ext:double-float-positive-infinity
            sb-ext:double-float-negative-infinity
            sb-ext:single-float-positive-infinity
            sb-ext:single-float-negative-infinity)
          :de.setf.utility.implementation))
