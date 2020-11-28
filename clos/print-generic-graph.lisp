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

;;; (dsu:graph-function 'spocq.si::graph-store-response #p"/tmp/graph-store-response.dot")
;;; (dsu:graph-function 'spocq.i::compute-bgp-lambda #p"/tmp/compute-bgp-lambda.dot")
;;; (dsu:graph-function 'rlmdb::map-repository-statements #p"/tmp/map-repository-statements.dot")


(modpackage :de.setf.utility
  (:export
   :graph-function))

(defparameter *dot-cache* nil)

(defgeneric dsu:graph-function (function destination &key attributes)
  (:documentation "Given a generic function, generate a dot document which encodes
   the relations among methods.")

  (:method ((name symbol) (destination t) &rest args)
    (apply #'dsu:graph-function (fdefinition name) destination args))

  (:method ((function t) (destination pathname) &rest args)
    (with-open-file (output destination :direction :output :if-does-not-exist :create :if-exists :supersede)
      (apply #'dsu:graph-function function output args)))

  (:method ((function generic-function) (destination stream) &key (attributes ()))
    (let ((*dot-cache* (make-hash-table :test 'equal))
          (*gensym-counter* 0)
          (setf.dot:*pretty* *print-pretty*))
      (flet ((put-graph-model ()
               (encode-function-graph dot:*context* function)))
        (destructuring-bind (&key (rankdir "LR") (fontname "courier")
                                  (edge '(:fontname "courier"))
                                  (node '(:fontname "courier"))
                                  &allow-other-keys) attributes
          (apply #'dot:context-put-graph (make-instance 'setf.dot:stream :stream destination)
                 (generic-function-name function) #'put-graph-model
                 :rankdir rankdir
                 :fontname fontname
                 :edge edge
                 :node node
                 :label (format nil "[~{~a~^ x ~}]" (sb-mop:generic-function-lambda-list function))
                 attributes))))
    (when *print-pretty*
      (fresh-line destination))
    function))

(defun encode-function-graph (dot-context function)
  (let ((methods (generic-function-methods function)))
    (loop for method in methods
      do (encode-method-graph-node dot-context method))
    (loop for method in methods
      for arguments = (compute-method-prototypes method)
      for applicable-methods = (compute-applicable-methods function arguments)
      do (loop for (first next) on applicable-methods by #'cddr
           do (encode-method-graph-link dot-context first next)))))

(defun compute-method-prototypes (method)
  (loop for specializer in (method-specializers method)
    ;;do (print specializer)
    collect (specializer-prototype specializer)))

(defgeneric specializer-name (specializer)
  (:method ((class class)) (class-name class))
  #+sbcl (:method ((specializer SB-MOP:EQL-SPECIALIZER))
           (specializer-name (SB-MOP:EQL-SPECIALIZER-object specializer)))
  (:method ((value t)) (write-to-string value)))

(defun encode-method-graph-node (dot-context method)
  (or (gethash method *dot-cache*)
    (let ((record-text (format nil "~{~a~^|~}~@[|~a~^, ~]"
                               (mapcar #'specializer-name (method-specializers method))
                               (method-qualifiers method)))
          (id (symbol-name (gensym "NODE"))))
      (setf (gethash method *dot-cache*) id)
      (dot:context-put-node dot-context id
                            :label record-text
                            :shape "record")
      id)))

(defun encode-method-graph-link (dot-context from to)
  (let ((from-id (gethash from *dot-cache*))
        (to-id (gethash to *dot-cache*)))
    (unless (gethash (cons from-id to-id) *dot-cache*)
      (setf (gethash (cons from-id to-id) *dot-cache*) t)
      (dot:context-put-edge dot-context to-id from-id))))

                        
:de.setf.utility

