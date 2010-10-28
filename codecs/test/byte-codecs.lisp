;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file tests byte codecs for the `de.setf.utility.codecs` library."
  
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
  
  (:description "The tests mirror the definitions in the respective library file."))


(test:test codecs.decode-byte 
  (let* ((position 0)
         (buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for x from 0 below 8 do (setf (aref buffer x) (1+ x)
                                        (aref buffer (+ 8 x)) (- 255 x)))
    (labels ((get-byte () (aref buffer (shiftf position (1+ position))))
             (do-decode (start decoder specs)
               (loop for (value bits) in specs
                     do (let ((decoded-value (progn (setf position start) (funcall decoder #'get-byte bits))))
                          (unless (= value decoded-value)
                            (return nil)))
                     finally (return t)))
             (inline-decode-unsigned-byte (getter bits)         ; trigger compiler macro
               (ecase bits
                 (8 (decode-unsigned-byte getter 8))
                 (16 (decode-unsigned-byte getter 16))
                 (32 (decode-unsigned-byte getter 32))
                 (64 (decode-unsigned-byte getter 64))))
             (inline-decode-signed-byte (getter bits)         ; trigger compiler macro
               (ecase bits
                 (8 (decode-signed-byte getter 8))
                 (16 (decode-signed-byte getter 16))
                 (32 (decode-signed-byte getter 32))
                 (64 (decode-signed-byte getter 64)))))
      (and (do-decode 0 #'decode-unsigned-byte '((#x01 8) (#x0102 16) (#x01020304 32) (#x0102030405060708 64)))
           (do-decode 0 #'decode-signed-byte '((#x01 8) (#x0102 16) (#x01020304 32) (#x0102030405060708 64)))
           (do-decode 8 #'decode-unsigned-byte '((#xff 8) (#xfffe 16) (#xfffefdfc 32) (#xfffefdfcfbfaf9f8 64)))
           (do-decode 8 #'decode-signed-byte `((,(sign-byte #xff 8) 8) (,(sign-byte #xfffe 16) 16)
                                               (,(sign-byte #xfffefdfc 32) 32)
                                               (,(sign-byte #xfffefdfcfbfaf9f8 64) 64)))
           (do-decode 0 #'inline-decode-unsigned-byte '((#x01 8) (#x0102 16) (#x01020304 32) (#x0102030405060708 64)))
           (do-decode 0 #'inline-decode-signed-byte '((#x01 8) (#x0102 16) (#x01020304 32) (#x0102030405060708 64)))
           (do-decode 8 #'inline-decode-unsigned-byte '((#xff 8) (#xfffe 16) (#xfffefdfc 32) (#xfffefdfcfbfaf9f8 64)))
           (do-decode 8 #'inline-decode-signed-byte `((,(sign-byte #xff 8) 8) (,(sign-byte #xfffe 16) 16)
                                                      (,(sign-byte #xfffefdfc 32) 32)
                                                      (,(sign-byte #xfffefdfcfbfaf9f8 64) 64))))))
  :prerequisites (:codecs.signed-byte))



(test:test codecs.encode-byte 
  (let* ((position 0)
         (buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for x from 0 below 8 do (setf (aref buffer x) (1+ x)
                                        (aref buffer (+ 8 x)) (- 255 x)))
    (labels ((put-byte (byte) (setf (aref buffer (shiftf position (1+ position))) byte))
             (get-byte () (aref buffer (shiftf position (1+ position))))
             (do-codec (encoder decoder value bits)
               (let ((decoded-value
                      (progn (setf position 0)
                             (funcall encoder #'put-byte value bits)
                             (setf position 0)
                             (funcall decoder #'get-byte bits))))
                 (= value decoded-value)))
             (inline-decode-unsigned-byte (getter bits)         ; trigger compiler macro
               (ecase bits
                 (8 (decode-unsigned-byte getter 8))
                 (16 (decode-unsigned-byte getter 16))
                 (32 (decode-unsigned-byte getter 32))
                 (64 (decode-unsigned-byte getter 64))))
             (inline-decode-signed-byte (getter bits)         ; trigger compiler macro
               (ecase bits
                 (8 (decode-signed-byte getter 8))
                 (16 (decode-signed-byte getter 16))
                 (32 (decode-signed-byte getter 32))
                 (64 (decode-signed-byte getter 64))))
             (inline-encode-unsigned-byte (setter value bits)         ; trigger compiler macro
               (ecase bits
                 (8 (encode-unsigned-byte setter value 8))
                 (16 (encode-unsigned-byte setter value 16))
                 (32 (encode-unsigned-byte setter value 32))
                 (64 (encode-unsigned-byte setter value 64))))
             (inline-encode-signed-byte (setter value bits)         ; trigger compiler macro
               (ecase bits
                 (8 (encode-signed-byte setter value 8))
                 (16 (encode-signed-byte setter value 16))
                 (32 (encode-signed-byte setter value 32))
                 (64 (encode-signed-byte setter value 64)))))
      (dolist (bits '(8 16 32 64) t)
        (let* ((smin (- (expt 2 (1- bits))))
               (smax (1- (expt 2 (1- bits))))
               (umax (1- (expt 2 bits)))
               (uvalues (list 0 1 (floor (/ umax 2)) umax))
               (svalues (list smin (floor (/ smin 2)) -1 0 1 (floor (/ smax 2)) smax)))
          (and (dolist (uvalue uvalues t)
                 (unless (and (do-codec #'encode-unsigned-byte #'decode-unsigned-byte uvalue bits)
                              (do-codec #'inline-encode-unsigned-byte #'inline-decode-unsigned-byte uvalue bits)
                              )
                   (return nil)))
               (dolist (svalue svalues t)
                 (unless (and (do-codec #'encode-signed-byte #'decode-signed-byte svalue bits)
                              (do-codec #'inline-encode-signed-byte #'inline-decode-signed-byte svalue bits))
                   (return nil))))))))
  :prerequisites (:codecs.decode-byte))



(test:test codecs.buffer-byte-codecs
  (let ((buffer (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (labels ((do-codec (encoder decoder value)
               (funcall encoder buffer value 0)
               (= (funcall decoder buffer 0) value)))
    (dolist (bit-count '(8 16 32 64) t)
        (let* ((bit-count-string (princ-to-string bit-count))
               (signed-getter (cons-symbol :de.setf.utility.codecs :buffer-get-signed-byte- bit-count-string))
               (signed-setter (cons-symbol :de.setf.utility.codecs :buffer-set-signed-byte- bit-count-string))
               (unsigned-getter (cons-symbol :de.setf.utility.codecs :buffer-get-unsigned-byte- bit-count-string))
               (unsigned-setter (cons-symbol :de.setf.utility.codecs :buffer-set-unsigned-byte- bit-count-string))
               (smin (- (expt 2 (1- bit-count))))
               (smax (1- (expt 2 (1- bit-count))))
               (umax (1- (expt 2 bit-count)))
               (uvalues (list 0 1 (floor (/ umax 2)) umax))
               (svalues (list smin (floor (/ smin 2)) -1 0 1 (floor (/ smax 2)) smax)))
          (and (dolist (uvalue uvalues t)
                 (unless (do-codec unsigned-setter unsigned-getter uvalue)
                   (return nil)))
               (dolist (svalue svalues t)
                 (unless (do-codec signed-setter signed-getter svalue)
                   (return nil))))))))
  :prerequisites (:codecs.encode-byte))


(test:test codecs.stream-byte-codecs
  (let ((stream (make-instance 'vector-io-stream)))
    (labels ((do-codec (encoder decoder value)
               (stream-position stream 0)
               (funcall encoder stream value)
               (stream-position stream 0)
               (= (funcall decoder stream) value)))
    (dolist (bit-count '(8 16 32 64) t)
        (let* ((bit-count-string (princ-to-string bit-count))
               (signed-reader (cons-symbol :de.setf.utility.codecs :stream-read-signed-byte- bit-count-string))
               (signed-writer (cons-symbol :de.setf.utility.codecs :stream-write-signed-byte- bit-count-string))
               (unsigned-reader (cons-symbol :de.setf.utility.codecs :stream-read-unsigned-byte- bit-count-string))
               (unsigned-writer (cons-symbol :de.setf.utility.codecs :stream-write-unsigned-byte- bit-count-string))
               (smin (- (expt 2 (1- bit-count))))
               (smax (1- (expt 2 (1- bit-count))))
               (umax (1- (expt 2 bit-count)))
               (uvalues (list 0 1 (floor (/ umax 2)) umax))
               (svalues (list smin (floor (/ smin 2)) -1 0 1 (floor (/ smax 2)) smax)))
          (and (dolist (uvalue uvalues t)
                 (unless (do-codec unsigned-writer unsigned-reader uvalue)
                   (return nil)))
               (dolist (svalue svalues t)
                 (unless (do-codec signed-writer signed-reader svalue)
                   (return nil)))))))))

