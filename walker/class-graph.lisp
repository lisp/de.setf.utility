;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)


(:documentation "This file defines a graphviz-based grapher for package constituency and
 used-by/uses relations for the 'de.setf.utility' library."
  
  (copyright
   "Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (history
   (delta 20021120)
   (delta 20031101 "janderson" "cleaned up and adjusted for denominated methods")
   (delta 20031210 "janderson" "corrected qualifier slot name for walker class")
   (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators,
 isolate the runtime-depencies.")))


(defun graph-classes (&key packages (stream *standard-output*) class
                           (name "classes")
                           (level nil)
                           (count nil)
                           (options nil)
                           (put-node #'(lambda (class)
                                         (setf.dot:put-eol)
                                         (setf.dot:put-node (class-name class))))
                           (put-edge #'(lambda (class other-class relation)
                                         (setf.dot:put-eol)
                                         (case relation
                                           (superclass
                                            (setf.dot:put-edge (class-name other-class) (class-name class)
                                                               :label "is-a" :arrowhead "none" :arrowtail "normal"))
                                           (subclass
                                            (setf.dot:put-edge (class-name class) (class-name other-class)
                                                               :label "is-a" :arrowhead "none" :arrowtail "normal"))
                                           (t
                                            #+ignore ;; handled as part of the node generation
                                            (setf.dot:put-edge (class-name class) (class-name other-class)
                                                               :label (string relation))))))
                           (put-appendix nil)
                           (size *graph-size*)
                           (rankdir *graph-rankdir*)
                           (ratio *graph-ratio*)
                           (margin *graph-margin*)
                           (graph-attributes '())
                           (graph-arguments graph-attributes))
  (let ((walk-count 0))
    (flet ((put-statement (class &optional (other-class nil oc-p) relation)
             (when (and (or (null level) (<= *walk-depth* level))
                        (or (null count) (<= walk-count count)))
               (cond (oc-p
                      ;; an edge
                      (funcall put-edge class other-class relation))
                     (t
                      (incf walk-count)
                      (funcall put-node class))))
             class))
      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                                &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream name
               #'(lambda ()
                   (apply #'walk-classes class packages #'put-statement options)
                   (when put-appendix (funcall put-appendix)))
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               graph-arguments)))
    walk-count))


(defun print-classes (&key packages (stream *standard-output*) class)
  (let ((walk-count 0))
    (flet ((print-class (class &optional (other nil other-p) relation)
             (cond (other-p
                    (terpri stream)
                    (dotimes (x (* 5 *walk-depth*)) (write-char #\space stream))
                    (format stream "~a -> ~a" relation (class-name other)))
                   (t
                    (terpri stream)
                    (dotimes (x (* 5 *walk-depth*)) (write-char #\space stream))
                    (format stream "~a:" (class-name class))))
             (incf walk-count)
             class))
      (walk-classes class packages #'print-class))
    walk-count))



:de.setf.utility.walker

