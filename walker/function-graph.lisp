;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "This file defines a graphviz-based grapher for function calls/called relations for the
 'de.setf.utility' library."
  
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
   (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators and
 isolate the runtime-depencies.")))


(defun graph-functions (extent &key (stream *standard-output*)
                                 function (root function)
                             (name nil)
                             (level nil)
                             (count nil)
                             (options nil)
                             (size *graph-size*)
                             (rankdir *graph-rankdir*)
                             (ratio *graph-ratio*)
                             (margin *graph-margin*)
                             (graph-attributes '())
                             (graph-arguments graph-attributes)
                             (url-encoder nil))
  (let ((walk-count 0))
    (labels ((function-nickname (function)
               (let* ((designator (dsw:function-name function))
                      (name (if (consp designator) (second designator) designator))
                      (package (symbol-package name))
                      (p-nick (or (first (package-nicknames package))
                                  (package-name package))))
                 (if (eq package *package*)
                   (symbol-name name)
                   (concatenate 'string p-nick ":" (symbol-name name)))))
             (put-statement (function &optional (other-function nil other-p) relation)
               (when (typep function 'function)
                 (when (and (or (null level) (<= *walk-depth* level))
                            (or (null count) (<= walk-count count)))
                   (cond (other-p
                          ;; a link
                          (when (typep other-function 'function)
                            (setf.dot:put-edge (function-nickname function) (function-nickname other-function)
                                               :label (string relation))))
                         (t
                          (incf walk-count)
                          (setf.dot:put-node (function-nickname function)
                                             :url (encode-url function))))))
               function)
             (encode-url (component)
               (when url-encoder (funcall url-encoder component))))
      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                                &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream (or name
                                                       (if (functionp root)
                                                         (function-nickname root)
                                                         "?"))
               ;; should perhaps choose based on the root - if it's a function the first ways suffices
               ;; #'(lambda () (apply #'walk-functions root extent #'put-statement options))
               #'(lambda () (apply #'walk-image root extent #'put-statement
                                   :excluded-qualifiers '(callers)
                                   options))
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               graph-arguments)))
    
    walk-count))


(defun print-functions (packages &key (stream *standard-output*) function)
  (let ((walk-count 0))
    (flet ((print-function (function &optional (other nil other-p) relation)
             (cond (other-p
                    (terpri stream)
                    (dotimes (x (* 5 *walk-depth*)) (write-char #\space stream))
                    (format stream "~a: ~a" relation (function-name other)))
                   (t
                    (terpri stream)
                    (dotimes (x (* 5 *walk-depth*)) (write-char #\space stream))
                    (format stream "~a:" (function-name function))))
             (incf walk-count)
             function))
      (walk-functions function packages #'print-function))
    walk-count))


:de.setf.utility.walker

