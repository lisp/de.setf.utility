;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  This file is part of the 'de.setf.utility.dot' (or 'dot') library component.
  It defines dets for .dot file generation and other model processing.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2009' AUTHOR='james adam anderson' href='mailto:james.anderson@setf.de'>
  'dot' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'dot' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'dot'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.
  </COPYRIGHT>
 <CHRONOLOGY>
  <DELTA DATE='20021212'>abstracted from xqdm-graph</DELTA>
  <DELTA DATE='20090216'>refactored with simpler interface and tests</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#


(in-package :de.setf.utility.implementation)

(defparameter *dot-model-1*
  '(setf.dot:graph "test" (:rankdir "lr")
              (setf.dot:node "a" :label "node1" :fontsize "9" :fontname "courier")
              (setf.dot:node "b" :label "node2" :fontsize "9" :fontname "courier")
              (setf.dot:node "c" :label "node3" :fontsize "9" :fontname "courier")
              (setf.dot:node "d" :label "node4" :fontsize "9" :fontname "courier")
              (setf.dot:edge "a" "b" :fontname "courier" :fontsize "9" :label "edge1")
              (setf.dot:edge "a" "c" :fontname "courier" :fontsize "9" :label "edge2")
              (setf.dot:edge "b" "c" :fontname "courier" :fontsize "9" :label "edge3")
              (setf.dot:edge "b" "d" :fontname "courier" :fontsize "9" :label "edge4")
              (setf.dot:edge "c" "b" :fontname "courier" :fontsize "9" :label "edge5"))
  "An s-expression dot model equivalent to the <a href='http://www.martin-loetzsch.de/S-DOT/'>S-DOT</a>
 example")

(dsut:test dot/graph/1
           "Tests the operator specializations for the s-expression model representation.
 The constructor context causes the operators to reconstruct an equal model.
 The model is equivalent ."
           (equalp *dot-model-1*
                   (setf.dot:with-context (make-instance 'setf.dot:constructor)
                     (eval *dot-model-1*))))

(dsut:test dot/graph/2
           "Tests the operator specializations for the .dot encoding.
 The constructor context causes the operators to encode the graph to the stream.
 The representation is not the same as S-DOT - i don't think its choices are correct."
           (with-output-to-string (stream)
             (setf.dot:with-context (make-instance 'setf.dot:stream :stream stream)
               (eval *dot-model-1*)))
           "graph test { rankdir=lr;a [label=node1, fontsize=\"9\", fontname=courier];b [label=node2, fontsize=\"9\", fontname=courier];c [label=node3, fontsize=\"9\", fontname=courier];d [label=node4, fontsize=\"9\", fontname=courier];a -- b [fontname=courier, fontsize=\"9\", label=edge1];a -- c [fontname=courier, fontsize=\"9\", label=edge2];b -- c [fontname=courier, fontsize=\"9\", label=edge3];b -- d [fontname=courier, fontsize=\"9\", label=edge4];c -- b [fontname=courier, fontsize=\"9\", label=edge5];}")


(dsut:test dot/graph/3
           "test dot interpretation. write the model as a .dot file, invokes dot, and (if os x), opens the file.
            upon success, the error codes should both be 0"
           (let ((dot-file #p"LIBRARY:de;setf;utility;dot;tests;dot-model-1.dot"))
             (with-open-file (stream dot-file
                                     :direction :output :if-exists :supersede :if-does-not-exist :create)
               (setf.dot:with-context (make-instance 'setf.dot:stream :stream stream :eol (string #\newline)
                                                     :pretty t)
                 (eval *dot-model-1*)))
             ;; requires full program pathname
             (logior (nth-value 1 (bsd:run-command #p"opt:local:bin:dot" "-o" #P"tmp:dot-model-1.jpg" "-Tjpg" dot-file))
                     (nth-value 1 (bsd:run-command "open" #p"tmp:dot-model-1.jpg"))))
           0)


:EOF
