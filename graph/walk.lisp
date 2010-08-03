;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation  "iterative-form model navigation operators for an abstract graph model"

  (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (copyright "COPYRIGHT 2002 james anderson")
   (delta 20020723 "factored out from path-relative behaviour")
   (delta 20030902 "established as a utility")))


(defGeneric walk-ancestors (node function)
  (:documentation
   "iterate with a function over the parent closure of a node.
    child implements this in terms of parent.")
  (:method ((node t) (function t)))
  (:method ((node setf.graph:child) function) (walk-self-and-ancestors (parent node) function))
  (:method ((node setf.graph:root) (function t))))

(defGeneric walk-annotations (node function)
  (:documentation
   "iterate with a function first over the declarations and then the attributes of a node.
    an node implementes this in terms of walk-declarations and walk-attributes")
  (:method ((node t) (function t)))
  (:method ((node setf.graph:node) (function t)) (abstract-method-error #'walk-annotations node function)))

(defGeneric walk-children (node function)
  (:documentation
   "iterate over the children of a document model node.
    an parent specialization must implement this function.")
  (:method ((node t) (function t)))
  (:method ((node setf.graph:parent) (function t)) (abstract-method-error #'walk-children node function))
  (:method ((node setf.graph:leaf) (function t))))

(defGeneric walk-children-reversed (node function)
  (:documentation
   "iterate in reverse document order with over the children of a document model node.
    an parent specialization must implement this function.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:parent) (function t)) (abstract-method-error #'walk-children-reversed node function))
  (:method ((node setf.graph:leaf) (function t)) ))

(defGeneric walk-descendants (node function)
  (:documentation
   "iterate with over the child closure of a node.
    parent implements this in terms of walk-children.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:parent) function)
           (walk-children node #'(lambda (node)
                                   (walk-self node function)
                                   (walk-descendants node function))))
  (:method ((node setf.graph:leaf) (function t)) ))

(defGeneric walk-descendants-reversed (node function)
  (:documentation
   "iterate over the child closure of a node in reverse document order.
    parent implements this in terms of generate-children-reversed.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:parent) function)
           (walk-children-reversed node #'(lambda (node)
                                            (walk-descendants-reversed node function)
                                            (walk-self node function))))
  (:method ((node setf.graph:leaf) (function t)) ))

(defGeneric walk-parent (node function)
  (:documentation
   "apply a function to the parent of a node.
    child implements this in terms of parent.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:child) function) (walk-self (parent node) function))
  (:method ((node setf.graph:root) (function t)) ))
           
(defgeneric walk-predecessor-siblings (node function)
  (:documentation
   "iterate with a function over the siblings which preceed a node.
    node implements this in terms of parent and walk-children-reversed.")
  (:method ((node t) (function t)) nil)
  (:method ((node setf.graph:node) (function t) &aux (found nil))
           (walk-children-reversed (parent node)
                                   #'(lambda (sibling)
                                       (if found
                                         (walk-self sibling function)
                                         (when (eq sibling node) (setf found t))))))
  (:method ((node setf.graph:root) (function t)) )
  (:method ((node setf.graph:leaf) (function t)) ))

(defgeneric walk-predecessors (node function)
  (:documentation
   "iterate over the siblings which preceed a node, the predecessors of the nodes ancestors,
    and the siblings' and predecessors' descendants.
    node implements this in terms of parent, walk-children-reversed, and
    walk-self-and-descendants-reversed.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:node) (function t) &aux (found nil) (parent (parent node)))
           (when parent
             (walk-children-reversed node #'(lambda (p-sibling)
                                              (if found
                                                (walk-self-and-descendants-reversed p-sibling function)
                                                (when (eq p-sibling node) (setf found t)))))
             (walk-predecessors parent function)))
  (:method ((node setf.graph:leaf) (function t)) ))

(defGeneric walk-root (node function)
  (:documentation
   "apply function to the respective document node.")
  (:method ((node t) (function t)))
  (:method ((node setf.graph:child) function) (walk-root (root node) function))
  (:method ((node setf.graph:root) function) (walk-self node function)))

(defGeneric walk-root-and-descendants (node function)
  (:documentation
   "apply function to the respective document node.")
  (:method ((node t) (function t)))
  (:method ((node setf.graph:child) function) (walk-root-and-descendants (root node) function))
  (:method ((node setf.graph:root) function) (walk-self-and-descendants node function)))

(defGeneric walk-self (node function)
  (:documentation
   "apply a given function to a node.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:node) function) (funcall function node))
  (:method ((node string) function) (funcall function node)))

(defGeneric walk-self-and-ancestors (node function)
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:child) function)
           (walk-self node function)
           (walk-self-and-ancestors (parent node) function))
  (:method ((node setf.graph:root) (function t)) ))

(defGeneric walk-self-and-children (node function)
  (:documentation
   "iterate with walk-self over a document model node and its children.")
  (:method ((node t) (function t)) )
  (:method ((node setf.graph:parent) function)
           (walk-self node function)
           (walk-children node function))
  (:method ((node setf.graph:leaf) function) (walk-self node function)))

(defGeneric walk-self-and-descendants (node function)
  (:documentation
   "iterate over the child closure of a node.")
  (:method ((node t) (function t)) (walk-self node function))
  (:method ((node setf.graph:parent) function)
           (walk-self node function)
           (walk-descendants node function))
  (:method ((node setf.graph:leaf) function) (walk-self node function)))

(defGeneric walk-self-and-descendants-reversed (node function)
  (:documentation
   "iterate with over the child closure of a node in reverse order.")
  (:method ((node t) (function t)) (walk-self node function))
  (:method ((node setf.graph:parent) function)
           (walk-descendants-reversed node function)
           (walk-self node function))
  (:method ((node setf.graph:leaf) function) (walk-self node function)))

(defgeneric walk-successor-siblings (node function)
  (:method ((node t) (function t)) nil)
  (:method ((node setf.graph:node) function &aux (found nil))
           (walk-children (parent node) #'(lambda (sibling)
                                            (if found
                                              (walk-self sibling function)
                                              (when (eq sibling node) (setf found t))))))
  (:method ((node setf.graph:leaf) (function t)) nil))

(defgeneric walk-successors (node function)
  (:method ((node t) (function t)) nil)
  (:method ((node setf.graph:child) function &aux (found nil) (parent (parent node)))
           (when parent
             (walk-children node #'(lambda (sibling)
                                     (if found
                                       (walk-self-and-descendants sibling function)
                                       (when (eq sibling node) (setf found t)))))
             (walk-successors parent function)))
  (:method ((node setf.graph:leaf) (function t)) ))


:EOF
