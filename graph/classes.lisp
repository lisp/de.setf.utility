;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "Define class and predicate definitions for an abstract graph model
  
  The definitions bind very few methods and serve instead as abstract interfaces to
  indicate a class's support for the minimal x-path-like navigation interface.
  see xml:code;*;*-generate.lisp and xml:code;*;*-walk.lisp."

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
   (delta 20020719 "introduced in order to simplify delegation-based clos instance implementation")
   (copyright "COPYRIGHT 2003 james anderson")
   (delta 20020919 "added cloning methods")
   (delta 20030902 "factored out of xml library established as a utility")))

;;
;;
;; data model classes

(def-abstract-class sg:node ()
  ()
  (:documentation
   "the `node` abstract class indicates that a a concrete specialization
    models a vertex in a data graph.
    a concrete specialization must define a `name` method."))

(def-abstract-class sg:parent (sg:node)
  ()
  (:documentation
   "the `parent` abstract class indicates that a vertex has links to children.
    a concrete specialization must define
    `generate-children`,
    `generate-children-reversed`, `walk-children`,
    and `walk-children-reversed` methods."))

(def-abstract-class sg:child (sg:node)
  ()
  (:documentation
   "the `child` abstract class indicates that a class models a vertex
    with links to a parent. a concrete specialization must define a
    `parent` method."))

(def-abstract-class sg:ancestor ()
  ()
  (:documentation
   "the `ancestor` abstract class indicates that a vertex is accessible along the ancestor path axis.
    this differs from the `parent` closure, in that a `root` vertex is the `parent`
    of the top-most model vertex, but is not an `ancestor`."))

(def-abstract-class sg:root (sg:parent)
  ()
  (:documentation
   "the `root` abstract class indicates that a class models the root of the data model graph.
    the `root` acts as the `parent` to the uppermost traversable node(s), but is not
    an `ancestor`."))

(def-abstract-class sg:leaf (sg:child)
  ()
  (:documentation
   "the `leaf` abstract class indicates that a vertex is a at the bottom extreme of a data model graph
    and, as such, has no further children."))

;;
;;
;; predicates, including provisions for built-in data types.

(def-class-constructors

  sg:ancestor

  (sg:child
   (:method ((datum number) &key &allow-other-keys) t)
   (:method ((datum string) &key &allow-other-keys) t))

  (sg:leaf
   (:method ((datum number) &key &allow-other-keys) t)
   (:method ((datum string) &key &allow-other-keys) t))

  sg:node

  (sg:parent
   (:documentation
   "return the node's parent. concrete classes must provide a method.")
   (:method ((node sg:child) &key &allow-other-keys)))

  (sg:root
   (:documentation
    "return the node's respective parent. the default method invokes parent to traverse links to the root.
 concrete implementations may provide more direct methods.")
   (:method ((node sg:child) &key &allow-other-keys)
            (sg:root (sg:parent node)))
   (:method ((node sg:root)  &key &allow-other-keys)
            "the root of a root is an identity function."
            node)))

;;
;;
;; abstract interface

(def-abstract-generic sg:children (node)
  (:documentation
   "return the node's children. concrete parent specialization class must provide a method.")
  (:method ((node sg:parent))))

(def-abstract-generic sg:annotations (node)
  (:documentation
   "return the node's annotations. every concrete child specialization class must provide a method.")
  (:method ((node sg:child))))


:de.setf.utility.graph
