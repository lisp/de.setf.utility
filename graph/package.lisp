;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;  This file ist the package definition for graph module for the 'de.setf.utility' Common Lisp library.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; Copyright 2002 james anderson
;;; 20020121  internal to cl-xml
;;; Copyright 2003 james anderson
;;; 20030902  factored out as a utility
;;; Copyright 2009 james anderson
;;; 20090405  combined the model and operator packages asthe distinction had not made it easier to use/import.
;;;  it's better to just use a short prefix.


(modPackage :de.setf.utility.graph
  (:nicknames :setf.graph :sg)
  (:documentation
   "The package :de.setf.utility.graph owns the symbols which name terms (types , predicates, and operations)
 in the abstract graph node/path model. It exports the core of a graph modela and path navigation interface.
 It uses no packages. It is neither exported through nor used by any
 package. The names are often quite brief and best isolated from other packages to avoid conflicts.
 They appear in implementation source files _with_ the package prefix.")
  (:version "1.000")
  (:use-only )
  (:export-only
   :*null-generator*
   :ancestor
   :ancestor-count
   :ancestor-p
   :annotations
   :child
   :children
   :child-p
   :generate-ancestors
   :generate-annotations
   :generate-children
   :generate-children-reversed
   :generate-descendants
   :generate-descendants-reversed
   :generate-parent
   :generate-predecessor-siblings
   :generate-predecessors
   :generate-annotations
   :generate-root
   :generate-root-and-descendants
   :generate-self
   :generate-self-and-ancestors
   :generate-self-and-children
   :generate-self-and-descendants
   :generate-self-and-descendants-reversed
   :generate-successor-siblings
   :generate-successors
   :leaf
   :leaf-p
   :node
   :node-p
   :parent
   :parent-p
   :root
   :root-p
   :walk-ancestors
   :walk-annotations
   :walk-children
   :walk-children-reversed
   :walk-descendants
   :walk-descendants-reversed
   :walk-parent
   :walk-predecessor-siblings
   :walk-predecessors
   :walk-root
   :walk-root-and-descendants
   :walk-self
   :walk-self-and-ancestors
   :walk-self-and-children
   :walk-self-and-descendants
   :walk-self-and-descendants-reversed
   :walk-successor-siblings
   :walk-successors
   ))


:de.setf.utility.graph

