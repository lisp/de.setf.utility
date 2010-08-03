;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation  "generative-form model navigation operators for an abstract graph model"
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


(defVar *null-generator* #'(lambda () nil))

(defgeneric ancestor-count (node)
  (:method ((node sg:child) &aux (count 0))
           (loop (unless (sg:ancestor-p  (setf node (sg:parent node)))
                   (return count))
                 (incf count)))
  (:method ((node t)) 0))

(defgeneric generate-ancestors (node)
  (:documentation
   "return a function which returns the parent closure of a node.
    child implements this in terms of parent.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:child)) (generate-self-and-ancestors (sg:parent node)))
  (:method ((node sg:root)) *null-generator*))

(defgeneric generate-annotations (element)
  (:documentation
   "return a function which returns first the declarations and then the attributes of a node.
    element implementes this in terms of generate-declarations and generate-attributes")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:leaf)) *null-generator*)
  (:method ((node sg:root)) *null-generator*)
  (:method ((node sg:node)) (abstract-method-error #'generate-annotations node)))

(defgeneric generate-children (parent)
  (:documentation
   "each parent class implementation must include a method for <code>generate-children</CODE>.
    return a function which returns the children of a node in document order.
    an parent specialization must implement this function.")
  (:method generate-children ((node t)) *null-generator*)
  (:method ((node sg:parent)) (abstract-method-error #'generate-children node))
  (:method generate-children ((node sg:leaf)) *null-generator*))

(defgeneric generate-children-reversed (node)
  (:documentation
   "return a function which returns the children of a node in reverse document order.
    an parent specialization must implement this function.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:parent)) (abstract-method-error #'generate-children-reversed node))
  (:method ((node sg:leaf)) *null-generator*))

(defgeneric generate-descendants (node)
  (:documentation
   "return a function which returns the child closure of a node in document order.
    parent implements this in terms of generate-children.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:parent)) (generate-descendants (generate-children node)))
  (:method ((node sg:leaf)) *null-generator*)
  (:method ((child-generator function))
           (let ((child nil)
                 (descendant nil)
                 (descendant-generator nil))
              #'(lambda () (loop (if (and descendant-generator (setf descendant (funcall descendant-generator)))
                                   (return descendant)
                                   (if child-generator
                                     (if (setf child (funcall child-generator))
                                       (progn (setf descendant-generator (generate-descendants child))
                                              (return child))
                                       (return (setf descendant-generator (setf child-generator nil))))
                                     (return nil))))))))

(defgeneric generate-descendants-reversed (node)
  (:documentation
   "return a function which returns the child closure of a node in reverse-document order.
    parent implements this in terms of generate-children-reversed.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:parent)) (generate-descendants-reversed (generate-children-reversed node)))
  (:method ((node sg:leaf)) *null-generator*)
  (:method ((child-generator function))
           (let ((child nil)
                 (descendant nil)
                 (descendant-generator nil))
              #'(lambda () (loop (if (and descendant-generator (setf descendant (funcall descendant-generator)))
                                   (return descendant)
                                   (if child
                                     (return (shiftf child nil))
                                     (if child-generator
                                       (if (setf child (funcall child-generator))
                                         (setf descendant-generator (generate-descendants-reversed child))
                                         (return (setf descendant-generator (setf child-generator nil))))
                                       (return nil)))))))))

(defgeneric generate-parent (node)
  (:documentation
   "return a function which the parent of a node.
    child implements this in terms of parent.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:child)) (setf node (sg:parent node)) #'(lambda () (shiftf node nil)))
  (:method ((node sg:root)) *null-generator*))

(defgeneric generate-predecessor-siblings (node)
  (:documentation
   "return a function which returns the siblings which preceed a node.
    element implements this in terms of parent and generate-children-reversed.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:node) &aux sibling (generator (generate-children-reversed (sg:parent node))))
           (loop (when (or (null (setf sibling (funcall generator)))
                           (eq sibling node))
                   (return generator))))
  (:method ((node sg:root)) *null-generator*)
  (:method ((node sg:leaf)) *null-generator*))

(defgeneric generate-predecessors (node)
  (:documentation
   "return a function which returns the siblings which preceed a node, the predecessors of the nodes ancestors,
    and the siblings' and predecessors' descendants.
    element implements this in terms of parent, generate-children-reversed, and
    generate-self-and-descendants-reversed.")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:node) &aux sibling (parent (sg:parent node))
            (sibling-generator (generate-children-reversed parent))
            (parent-predecessor-generator (generate-predecessors parent))
            descendant-generator)
           (loop (when (null (setf sibling (funcall sibling-generator)))
                   (return parent-predecessor-generator))
                 (when (eq sibling node)
                   (return #'(lambda ()
                               (or (and descendant-generator (funcall descendant-generator))
                                   (when (and sibling (setf sibling (funcall sibling-generator)))
                                     (setf descendant-generator
                                           (generate-self-and-descendants-reversed sibling))
                                     (funcall descendant-generator))
                                   (funcall parent-predecessor-generator)))))))
  (:method ((node sg:leaf)) *null-generator*))

(defgeneric generate-root (node)
  (:documentation
   "return a function which returns the respective root node.
    child implements this in terms of parent")
  (:method ((node t)) *null-generator*)
  (:method ((node sg:root)) #'(lambda () (shiftf node nil)))
  (:method ((node sg:child)) (generate-root (sg:root node))))

(defgeneric generate-root-and-descendants (node)
  (:method ((node t)) *null-generator*)
  (:method ((node sg:root)) (generate-self-and-descendants node))
  (:method ((node sg:child)) (generate-root-and-descendants (sg:root node))))

(defgeneric generate-self (node)
  (:documentation
   "return a closure which returns a node only once only.")
  (:method ((node t)) #'(lambda () (shiftf node nil)))
  (:method ((node sg:node)) #'(lambda () (shiftf node nil)))
  (:method ((node string)) #'(lambda () (shiftf node nil))))

(defgeneric generate-self-and-ancestors (node)
  (:method ((node t)) (generate-self node))
  (:method ((node sg:child) &aux (self-or-ancestor node))
           #'(lambda () (when (or (eq self-or-ancestor node) (sg:ancestor-p self-or-ancestor))
                          (shiftf self-or-ancestor (sg:parent self-or-ancestor)))))
  (:method ((node sg:root)) *null-generator*))

(defgeneric generate-self-and-children (node)
  (:method ((node t)) (generate-self node))
  (:method ((node sg:parent) &aux (child-generator (generate-children node)))
           #'(lambda () (or (shiftf node nil)
                            (when child-generator (or (funcall child-generator)
                                                      (setf child-generator nil))))))
  (:method ((node sg:leaf)) #'(lambda () (shiftf node nil))))

(defgeneric generate-self-and-descendants (node)
  (:method ((node t)) (generate-self node))
  (:method ((node sg:parent) &aux (descendant-generator (generate-descendants node)))
           #'(lambda () (or (shiftf node nil)
                            (and descendant-generator
                                 (or (funcall descendant-generator)
                                     (setf descendant-generator nil))))))
  (:method ((node sg:leaf)) #'(lambda () (shiftf node nil))))

(defgeneric generate-self-and-descendants-reversed (node)
  (:method ((node t)) (generate-self node))
  (:method ((node sg:parent) &aux (descendant-generator (generate-descendants-reversed node)))
           #'(lambda () (or (and descendant-generator
                                 (or (funcall descendant-generator)
                                     (setf descendant-generator nil)))
                            (shiftf node nil))))
  (:method ((node sg:leaf)) #'(lambda () (shiftf node nil))))

(defgeneric generate-successor-siblings (node)
  (:method ((node t)) *null-generator*)
  (:method ((node sg:node) &aux sibling (generator (generate-children (sg:parent node))))
           (loop (when (or (null (setf sibling (funcall generator)))
                           (eq sibling node))
                   (return generator))))
  (:method ((node sg:leaf)) *null-generator*))

(defgeneric generate-successors (node)
  (:method ((node t)) *null-generator*)
  (:method ((node sg:node) &aux sibling (parent (sg:parent node))
            (sibling-generator (generate-children parent))
            (parent-successor-generator (generate-successors parent))
            descendant-generator)
           (loop (when (null (setf sibling (funcall sibling-generator)))
                   (return parent-successor-generator))
                 (when (eq sibling node)
                   (return #'(lambda ()
                               (or (and descendant-generator (funcall descendant-generator))
                                   (when (and sibling (setf sibling (funcall sibling-generator)))
                                     (setf descendant-generator (generate-self-and-descendants sibling))
                                     (funcall descendant-generator))
                                   (funcall parent-successor-generator)))))))
  (:method ((node sg:leaf)) *null-generator*))


:EOF
