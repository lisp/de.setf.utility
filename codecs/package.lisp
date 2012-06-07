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
  (:nicknames :dsu.codecs)
  (:use )
  (:documentation "The API package for encoding operators.")
  
  (:import-from :de.setf.utility
                :stream-reader
                :stream-writer
                :stream-tyi
                :stream-tyo)
  
  #+allegro
  (:import-from :excl
                :stream-line-column
                :stream-write-char
                :stream-write-string
                )
  #+ccl
  (:import-from :ccl
                :open-stream-p
                :stream-clear-input
                :stream-clear-output
                :stream-direction
                :stream-eofp
                :stream-finish-output
                :stream-force-output
                :stream-fresh-line
                :stream-listen
                :stream-position
                :stream-read-byte
                :stream-write-byte
                :stream-write-string
                )
  #+clozure
  (:import-from :ccl
                :double-float-positive-infinity
                :double-float-negative-infinity
                #+ccl-1.4 :double-float-nan
                #:stream-advance-to-column
                #:stream-line-column
                #:stream-peek-char
                #:stream-read-char-no-hang
                #:stream-read-char
                #:stream-read-line
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-char
                )
  #+lispworks
  (:import-from :stream
                :stream-advance-to-column
                :stream-clear-input
                :stream-clear-output
                :stream-file-position
                :stream-finish-output
                :stream-force-output
                :stream-line-column
                :stream-peek-char
                :stream-read-byte
                :stream-read-char
                :stream-read-char-no-hang
                :stream-read-line
                :stream-read-sequence
                :stream-start-line-p
                :stream-terpri
                :stream-unread-char
                :stream-write-byte
                :stream-write-char
                :stream-write-sequence
                :stream-write-string)
  #+mcl
  (:import-from :ccl
                :stream-close
                :stream-read-sequence
                :stream-untyi
                :stream-write-sequence
                )
  #+sbcl
  (:import-from :sb-gray
                :open-stream-p
                :stream-advance-to-column
                :stream-clear-input
                :stream-clear-output
                :stream-file-position
                :stream-finish-output
                :stream-force-output
                :stream-fresh-line
                :stream-line-column
                :stream-listen
                :stream-peek-char
                :stream-read-byte
                :stream-read-char
                :stream-read-char-no-hang
                :stream-read-line
                :stream-read-sequence
                :stream-start-line-p
                :stream-terpri
                :stream-unread-char
                :stream-write-byte
                :stream-write-char
                :stream-write-sequence
                :stream-write-string
                )
  #+sbcl
  (:import-from :sb-ext
                :double-float-positive-infinity
                :double-float-negative-infinity
                :single-float-positive-infinity
                :single-float-negative-infinity)
  
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
           :double-float-positive-infinity
           :double-float-nan
           :double-float-negative-infinity
           :sign-byte
           :sign-byte-8
           :sign-byte-16
           :sign-byte-32
           :sign-byte-64
           :single-float-positive-infinity
           :single-float-nan
           :single-float-negative-infinity  
           :stream-advance-to-column
           :stream-clear-input
           :stream-clear-output
           :stream-close
           :stream-direction
           :stream-eofp
           :stream-file-position
           :stream-finish-output
           :stream-force-output
           :stream-fresh-line
           :stream-line-column
           :stream-listen
           :stream-peek-char
           :stream-position             ; ccl benefits from the cross-export
           :stream-read-byte
           :stream-read-char
           :stream-read-char-no-hang
           :stream-read-float-32
           :stream-read-float-64
           :stream-read-line
           :stream-read-sequence
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
           :stream-reader
           :stream-start-line-p
           :stream-terpri
           :stream-tyi
           :stream-tyo
           :stream-untyi
           :stream-unread-char
           :stream-write-byte
           :stream-write-char
           :stream-write-float-32
           :stream-write-float-64
           :stream-write-sequence
           :stream-write-signed-byte
           :stream-write-signed-byte-8
           :stream-write-signed-byte-16
           :stream-write-signed-byte-32
           :stream-write-signed-byte-64
           :stream-write-string
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
           :stream-writer
           :unsigned-byte-8))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :de.setf.utility.codecs :de.setf.utility.implementation))
