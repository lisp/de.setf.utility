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

           

(defun graph-packages (&key packages (stream *standard-output*)
                            (root (first packages))
                            (name "packages")
                            (level nil)
                            (count nil)
                            (options nil)
                            (size *graph-size*)
                            (rankdir *graph-rankdir*)
                            (ratio *graph-ratio*)
                            (margin *graph-margin*)
                            (graph-attributes '())
                            (graph-arguments graph-attributes))
  (let ((walk-count 0))
    (flet ((put-statement (package &optional (other-package nil op-p) relation)
             (when (and (or (null level) (<= *walk-depth* level))
                        (or (null count) (<= walk-count count)))
               (cond (op-p
                      ;; a link
                      (setf.dot:put-edge (package-name package) (package-name other-package)
                                         :label (string relation)))
                     (t
                      (incf walk-count)
                      (setf.dot:put-node (package-name package)))))))
      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                           &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream name
               #'(lambda () (apply #'walk-packages root packages #'put-statement options))
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               graph-arguments)))
    walk-count))


(defun print-packages (&key packages (stream *standard-output*) (root (first packages)))
  (let ((walk-count 0))
    (flet ((print-package-node (package &optional (other-package nil op-p) relation)
             (cond (op-p
                    (terpri stream)
                    (dotimes (x (+ 5 (* 5 *walk-depth*))) (write-char #\space stream))
                    (format stream "~a: ~a" relation (package-name other-package)))
                   (t
                    (terpri stream)
                    (dotimes (x (* 5 *walk-depth*)) (write-char #\space stream))
                    (format stream "~a~@[ ~a~]" (package-name package) (package-nicknames package))))
             (incf walk-count)
             package))
      (walk-packages root packages #'print-package-node))
    walk-count))



:de.setf.utility.walker

