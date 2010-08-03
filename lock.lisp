;;; -*- Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (intersection '(:clozure :digitool :sbcl) *features*)
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))


(:documentation "This file defines mp/lock utilities for the 'de.setf.utility' library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(modpackage :de.setf.utility.lock
  (:use )
  (:nicknames :setf.lock)
  (:export
   :call-with-object-locked
   :instance-lock
   :with-object-locked
   :synchronized-object
   :run-in-thread
   ))

(defmacro setf.lock:with-object-locked ((object) &rest body)
  (let* ((function (gensym)))
    `(flet ((,function () ,@body))
       (declare (dynamic-extent #',function))
       (setf.lock:call-with-object-locked #',function ,object))))

#+digitool
(setf (ccl:assq 'synchronized ccl::*fred-special-indent-alist*) 1)

#+ccl
(defun setf.lock:call-with-object-locked (function instance)
  "invoke the argument function while holding the lock on the argument instance."
  (with-lock-grabbed ((setf.lock:instance-lock instance))
    (funcall function)))

#+sbcl
(defun setf.lock:call-with-object-locked (function instance)
  "invoke the argument function while holding the lock on the argument instance."
  (sb-thread:with-mutex ((setf.lock:instance-lock instance))
    (funcall function)))

(declaim (ftype (function (&key (name t)) t) make-lock))

#-ccl
(setf (fdefinition 'make-lock)
      #+allegro #'mp:make-process-lock
      #+lispworks #'mp:make-lock
      #+sbcl #'sb-thread:make-mutex)
  

(defVar *instance-locks*
  #+clozure (make-hash-table :test 'eq :weak t)
  #+digitool (make-hash-table :test 'eq :weak t)
  #+sbcl (make-hash-table :test 'eq :weakness :key)
  )

;; make a lock for the registry itself
(setf (gethash *instance-locks* *instance-locks*) (make-lock))

(defclass setf.lock:synchronized-object ()
  ((lock
    ;; :initform nil
    :initarg :lock
    :reader get-instance-lock))
  (:documentation
   "a synchronized-object binds a lock for use with the synchronized and call-with-instance-lock-held.
    if no lock is provided as an initialization argument, one with be created upon first reference."))

(defGeneric setf.lock:instance-lock (instance)
  (:method ((datum t))
           (flet ((new-instance-lock ()
                    (or (gethash datum *instance-locks*)
                        (setf (gethash datum *instance-locks*) (make-lock)))))
             (declare (dynamic-extent #'new-instance-lock))
             (or (gethash datum *instance-locks*)
                 (setf.lock:call-with-object-locked #'new-instance-lock *instance-locks*))))
  (:documentation
   "retrieve an instance's lock. if the instance is not a synchronized object manage a lock for it in a central,
    weak hashtable."))

(defmethod setf.lock:instance-lock ((instance setf.lock:synchronized-object))
  "generate and bind an instance lock upon demand only."
  (if (slot-boundp instance 'lock)
    (get-instance-lock instance)
    (setf (slot-value instance 'lock) (make-lock))))

#+digitool
(defun setf.lock:run-in-thread (function
                   &key (name "anonymous") (priority 0)
                   parameters)
  "Runs function in it's own thread."
  (let ((keys `(:name , name :priority ,priority)))
    (declare (dynamic-extent keys))
    (apply #'ccl:process-run-function keys function parameters)))

#+clozure
(defun setf.lock:run-in-thread (function
                   &key (name "anonymous") (priority 0)
                   parameters)
  "Runs function in it's own thread."
  (let ((keys `(:name , name :priority ,priority)))
    (declare (dynamic-extent keys))
    (apply #'ccl:process-run-function keys function parameters)))

#+(or lispworks allegro)
(defun setf.lock:run-in-thread (function
                      &key (name (function-namestring function))
                      priority
                           parameters)
  "Runs function in it's own thread."
  (declare (ignore priority))
  (apply #'mp:process-run-function
         name
         function
         parameters))

#+sbcl
(defun setf.lock:run-in-thread (function
                      &key (name (function-namestring function))
                      priority
                           parameters)
  "Runs function in it's own thread."
  (declare (ignore priority))
  (flet ((run-op () (apply function parameters)))
    (sb-thread:make-thread #'run-op :name name)))





#|
(defparameter *w* (make-instance 'fred-window))
(defun wtw (w string)
  (synchronized w
                (terpri w)
                (dotimes (x (length string)) (write-char (char string x) w))))

(synchronized (datum (front-window)) (print datum))
(synchronized datum (print datum))


;; test timing for slot-boundp vs slot-unbound vs null

(defclass slot-test ()
  ((slot :initarg :slot :reader slot-test-slot)))

(defmethod slot-unbound ((class standard-class) (instance slot-test) (slot (eql 'slot)))
  (setf (slot-value instance 'slot) :x))

(defmethod test-by-slot-null ((instance slot-test))
  (with-slots (slot) instance (or slot (setf slot :y))))

(defmethod test-by-slot-unbound ((instance slot-test))
  (with-slots (slot) instance slot))

(defmethod test-by-slot-boundp ((instance slot-test))
  (with-slots (slot) instance
    (if (slot-boundp instance 'slot)
      slot
      (setf slot :z))))


(let ((instance (make-instance 'slot-test)))
  ;; initialize 57  / reference 16 ms
  (time (dotimes (x 100000) (setf (slot-value instance 'slot) nil) (test-by-slot-null instance)))
  (time (dotimes (x 100000)                                        (test-by-slot-null instance)))
  ;; initialize 147 / reference 17 ms
  (time (dotimes (x 100000) (slot-makunbound instance 'slot)       (test-by-slot-unbound instance)))
  (time (dotimes (x 100000)                                        (test-by-slot-unbound instance)))
  ;; initialize 147 / reference 17 ms
  (time (dotimes (x 100000) (slot-makunbound instance 'slot)       (slot-test-slot instance)))
  (time (dotimes (x 100000)                                        (slot-test-slot instance)))
  ;; initialize 75 / reference 26 ms
  (time (dotimes (x 100000) (slot-makunbound instance 'slot)       (test-by-slot-boundp instance)))
  (time (dotimes (x 100000)                                        (test-by-slot-boundp instance)))
  )

;; according to which the specialized slot-unbound method has a high initial cose, but
;; negligible long-term cost at the benefit simpler use. if a standard reader suffices, then
;; the method is faster.

|#
