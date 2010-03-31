;;; -*- Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;
;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;; It defines generic operators to print object instance slots
;;;
;;;  Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).


(modpackage :de.setf.utility
  (:export
   :def-print-object-slots
   :print-object-slots
   :print-object-slot-names
   :*print-object-slots*))

(defvar *print-object-slots* t
  "binds an indicator for use by applications to turn the behaviour on and off.")

(defgeneric print-object-slots (instance stream)
  ;(:argument-precedence-order stream instance)
  (:method-combination denominated-progn)
  
  (:method :around ((instance t) (stream t))
           (if *print-object-slots*
             (call-next-method)
             (write-string "[...]" stream)))
      
  (:method :between ((instance t) (stream t)) (write-string " " stream))
  
  (:method :qualifying ((instance t) (stream t))
    "the general method just computes the names of the combined class and instance slots."
    (print-object-slot-names instance stream)))


(defgeneric print-object-slot-names (instance-class stream-class)
  (:documentation "this is used by the denominated-progn method combination for print-object-slots to
    constrain the method qualifiers introspectively.")

  (:method ((instance standard-object) (stream t))
    (print-object-slot-names (class-of instance) stream))
  
  (:method ((instance-class standard-class) (stream t))
    (finalize-if-needed instance-class)
    (class-slot-names instance-class)))
    
    

(defmacro def-print-object-slots (specialized-parameter-list slot-print-specs)
  (let ((stream-var (second specialized-parameter-list))
        (object-var (first (first specialized-parameter-list))))
    (when (consp stream-var)
      (setf stream-var (first stream-var)))
   `(progn
      ,@(mapcar #'(lambda (slot-print-spec)
                    (etypecase slot-print-spec
                      (symbol
                       `(defmethod print-object-slots ,slot-print-spec
                                   ,specialized-parameter-list
                          (format ,stream-var ,(format nil "[~a: ~~a]" slot-print-spec)
                                  (slot-value ,object-var ',slot-print-spec))))
                      (cons
                       (destructuring-bind (slot-designators &rest print-spec)
                           slot-print-spec
                         (etypecase slot-designators
                           (cons )
                           ((and symbol (not null))
                            (setf slot-designators (list slot-designators))))
                         (etypecase (first print-spec)
                           (string (setf print-spec `((format ,stream-var ,@print-spec))))
                           (list ))     ; allow a null print-spec
                         `(defmethod print-object-slots
                              ,@slot-designators
                              ,specialized-parameter-list
                            ,@print-spec)))))
                 slot-print-specs))))
                        
:de.setf.utility

