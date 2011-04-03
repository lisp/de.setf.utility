;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines byte vector streams for the `de.setf.utility.codecs` library."
  
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

  (:description "Defines a binary stream to wrap a byte vector for use (at least) in tests.
 Adapted from the cl-xml version to default i/o to unsigned byte operations, but allow signed streams
 as, eg. specified for thrift."))


(defparameter *vector-stream-element-type* '(unsigned-byte 8))

;;;
;;; abstract

(defclass vector-stream ()
  ((position
    :initform 0
    :reader get-stream-position :writer setf-stream-position)
   (vector
    ;; nb no type information, othreise sbcl objects to latr specification
    :reader get-vector-stream-vector :writer setf-vector-stream-vector
    :type vector)
   (signed)
   (force-output-hook
    :initform nil :initarg :force-output-hook
    :accessor stream-force-output-hook
    :documentation "A function of one argument, the stream, called as the
     base implementation of stream-force-output.")
   #+(or CMU sbcl lispworks) (direction :initarg :direction)
   )
  (:default-initargs
    :element-type *vector-stream-element-type*))


(defClass vector-input-stream (vector-stream
                               #+ALLEGRO excl::fundamental-binary-input-stream
                               #+LispWorks stream:fundamental-stream
                               #+(and MCL digitool) ccl::input-binary-stream
                               #+(and MCL openmcl) fundamental-binary-input-stream
                               #+CMU extensions:fundamental-binary-input-stream
                               #+sbcl sb-gray:fundamental-binary-input-stream
                               #+CormanLisp stream
                               )
  ()
  (:default-initargs :direction :input))

(defClass vector-output-stream (vector-stream
                                #+ALLEGRO excl::fundamental-binary-output-stream
                                #+LispWorks stream:fundamental-stream
                                #+(and MCL digitool) ccl::output-binary-stream
                                #+(and MCL openmcl) fundamental-binary-output-stream
                                #+CMU extensions:fundamental-binary-output-stream
                                #+sbcl sb-gray:fundamental-binary-output-stream
                                #+CormanLisp stream
                                )
  ()
  (:default-initargs :direction :output))

(defclass vector-io-stream (vector-input-stream vector-output-stream)
  ()
  (:default-initargs :direction :io))


(defun make-vector-stream-buffer (length &optional (element-type *vector-stream-element-type*))
  (make-array length :element-type element-type :initial-element 0))


(defmethod shared-initialize
           ((instance vector-stream) (slots t) &key (vector nil vector-s) (length 128)
            element-type)
  (with-slots (position signed) instance
    (setf position 0)
    (setf signed (ecase (first element-type) (signed-byte t) (unsigned-byte nil)))
    (when vector-s
      (setf-vector-stream-vector
       (etypecase vector
         (string (map-into (make-vector-stream-buffer (length vector) element-type)
                           #'char-code vector))
         (cl:cons (map-into (make-vector-stream-buffer (length vector) element-type)
                            #'(lambda (datum)
                                (etypecase datum
                                  (fixnum datum)
                                  (character (char-code datum))))
                            vector))
         (vector vector)
         (null (make-vector-stream-buffer length element-type)))
       instance))
    (call-next-method)
    (unless (slot-boundp instance 'vector)
      (setf-vector-stream-vector (make-vector-stream-buffer length element-type) instance))))

#+cmu
(let ((old-definition (fdefinition 'stream-element-type)))
  (unless (typep old-definition 'generic-function)
    (fmakunbound 'stream-element-type)
    (defgeneric stream-element-type (stream))
    (setf (documentation 'stream-element-type 'function)
          (documentation old-definition 'function))
    (defmethod stream-element-type (stream)
      (funcall old-definition stream))))

(defmethod stream-element-type ((stream vector-stream))
  (array-element-type (get-vector-stream-vector stream)))

(defmethod stream-position ((stream vector-stream) &optional new)
  (with-slots (vector) stream
    (if new
      (setf-stream-position (min (length vector) new) stream)
      (get-stream-position stream))))

#+(or)
(defmethod amqp.u:stream-file-position ((stream vector-stream) &optional new)
  (when (null new) 
    (stream-position stream)))


(defmethod stream-eofp ((stream vector-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defmethod stream-finish-output ((stream vector-stream))
  nil)

(defmethod print-object
           ((vs vector-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  (print-unreadable-object (vs stream :type t)
    (princ (get-vector-stream-vector vs) stream)))

(defmethod stream-force-output ((stream vector-stream))
  (let ((hook (stream-force-output-hook stream)))
    (when hook (funcall hook stream))))

#-mcl
(defmethod open-stream-p ((stream vector-stream))
  t)

(defgeneric vector-stream-vector (vector-stream)
  (:documentation "Return the written subsequence and reset the position")
  (:method ((stream vector-stream))
    (with-slots (vector position) stream
      (prog1 (subseq vector 0 position)
        (setf position 0)))))

(defgeneric (setf vector-stream-vector) (vector vector-stream)
  (:method ((new-vector vector) (stream vector-stream))
    (with-slots (vector position) stream
    (assert (equal (array-element-type new-vector) (array-element-type vector)) ()
            "Invalid vector stream element type: ~s." (array-element-type new-vector))
      (setf position 0
            vector new-vector))))

;;;
;;; input

(defmethod stream-read-byte ((stream vector-input-stream))
  (with-slots (position vector signed) stream
    (when (< position (length vector))
      (let ((byte (aref vector position)))
        (incf position)
        (if (and (> byte 127) signed)
          (- (logxor 255 (1- byte)))
          byte)))))


(defmethod stream-read-sequence ((stream vector-input-stream) (sequence vector)
                                  #+mcl &key #-mcl &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (min (+ position (- end start)) (length vector))))
      (when (> new-position position)
        (replace sequence vector
                 :start1 start :end1 end
                 :start2 position :end2 new-position)
        (setf position new-position))
      new-position)))


(defMethod stream-tyi ((stream vector-input-stream))
  (stream-read-byte stream))

(defMethod stream-untyi ((stream vector-input-stream) (datum integer))
  (with-slots (position vector) stream
    (cond ((> position 0)
           (decf position)
           (setf (svref vector position) datum))
          (t
           (error 'end-of-file :stream stream)))))

(defMethod stream-reader ((stream vector-input-stream))
  (with-slots (vector position) stream
    (flet ((simple-vector-reader (ignore)
             (declare (ignore ignore))
             (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 8) (*)) vector))
             (when (< position (length vector))
               (prog1 (aref vector position)
                 (incf position))))
           (vector-reader (ignore)
             (declare (ignore ignore))
             (declare (optimize (speed 3) (safety 0))
                      (type (array (unsigned-byte 8) (*)) vector))
             (when (< position (length vector))
               (prog1 (aref vector position)
                 (incf position)))))
      (etypecase vector
        (simple-vector #'simple-vector-reader)
        (vector #'vector-reader)))))

;;;
;;; output


(defmethod stream-write-byte ((stream vector-output-stream) (datum integer) &aux next)
  (with-slots (position vector) stream
    (unless (< (setf next (1+ position)) (length vector))
      (setf vector
            (adjust-array vector (+ next (floor (/ next 4)))
                          :element-type (array-element-type vector))))
    (setf (aref vector position)
          (logand #xff datum))
    (setf position next)))


(defmethod stream-write-sequence
           #-mcl ((stream vector-output-stream) (sequence vector) &optional (start 0) (end nil))
           #+mcl ((stream vector-output-stream) (sequence vector) &key (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (+ position (- end start))))
      (when (> new-position position)
        (unless (< new-position (length vector))
          (setf vector
                (adjust-array vector (floor (+ new-position (floor (/ new-position 4))))
                              :element-type (array-element-type vector))))
        (replace vector sequence
                 :start1 position :end1 new-position
                 :start2 start :end2 end)
        (setf position new-position))
      new-position)))

(defmethod stream-write-string
           #-mcl ((stream vector-output-stream) (sequence string) &optional (start 0) (end nil))
           #+mcl ((stream vector-output-stream) (sequence string) start end)
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (+ position (- end start))))
      (when (> new-position position)
        (unless (< new-position (length vector))
          (setf vector
                (adjust-array vector (floor (+ new-position (floor (/ new-position 4))))
                              :element-type (array-element-type vector))))
        (dotimes (i (- end start))
          (setf (aref vector position) (char-code (char sequence start)))
          (incf position)
          (incf start))
        (setf position new-position))
      new-position)))

(defmethod stream-tyo ((stream vector-output-stream) (datum integer))
  (stream-write-byte stream datum))

(defmethod stream-tyo ((stream vector-output-stream) (datum character))
  ;; no unicode
  (stream-write-byte stream (char-code datum)))

(defmethod stream-write-char ((stream vector-output-stream) (datum character))
  (stream-write-byte stream (char-code datum)))

;; support ascii character strings...
(defmethod stream-writer ((stream vector-output-stream))
  (with-slots (vector position) stream
    (flet ((simple-vector-writer (next datum)
             (declare (optimize (speed 3) (safety 0))
                      (type (vector (unsigned-byte 8) *) vector))
             (when (>= position (length vector))
               (setf next (+ position 1))
               (setf vector (adjust-array vector (+ next (floor (/ next 4)))
                                          :element-type (array-element-type vector))))
             (when (characterp datum)
               (setf datum (char-code datum)))
             (setf (aref vector position) (logand #xff datum))
             (incf position))
           (vector-writer (next datum)
             (declare (optimize (speed 3) (safety 0)))
             (when (>= position (length vector))
               (setf next (1+ position))
               (setf vector (adjust-array vector (+ next (floor (/ next 4)))
                                          :element-type (array-element-type vector))))
             (when (characterp datum)
               (setf datum (char-code datum)))
             (setf (aref vector position) (logand #xff datum))
             (incf position)))
      (etypecase vector
        (simple-vector #'simple-vector-writer)
        (vector #'vector-writer)))))
