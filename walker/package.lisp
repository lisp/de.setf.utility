;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;;  This file is the package definition for the walker module for 'de.setf.utility'
;;;  Common Lisp library.
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


;;;
;;; content : packages for generic walker
;;;

(in-package :cl-user)

(de.setf.utility:modpackage :de.setf.utility.walker
  (:nicknames :dsw)
  (:use-only )
  (:use :de.setf.utility.dot)
  (:use :de.setf.utility)
  (:use-by :de.setf.utility.implementation)
  #+ccl
  (:import-from :ccl :function-name)
  (:export-only
   :*graph-rankdir*
   :*graph-size*
   :*graph-ratio*
   :*graph-margin*
   :*walker*
   :*walker-cache*
   :*walk-depth*
   :*walk-extent*
   :*walk-superclass-link*
   :*walk-subclass-link*
   :caching-walker
   :cyclic-walker
   :endorder-node-walker
   :find-definition
   :in-extent-p
   :link-navigator
   :link-relation-operator
   :make-walker-cache-entry
   :map-walk-link
   :node-navigator
   :node-operator
   :node-predecessor-navigator
   :node-successor-navigator
   :object-designator
   :object-source-information
   :preorder-link-walker
   :preorder-node-walker
   :symmetric-node-walker
   :function-walker
   :walk-function
   :graph-function
   :print-function
   :function-lambda-list
   :function-name
   :function-callers
   :function-calls
   :function-package
   :package-walker
   :graph-packages
   :print-packages
   :walk-classes
   :walk-functions
   :walk-image
   :walk-link
   :walk-link-qualifier
   :walk-link-relation
   :walk-model
   :walk-navigate-link
   :walk-navigator
   :walk-node
   :walk-node-constituents
   :walk-node-predecessors
   :walk-node-qualifier
   :walk-node-self
   :walk-node-successors
   :walk-operator
   :walk-packages
   :walk-qualifiers
   :walker
   :walker-clear-cache
   :walker-entry
   :walker-entry-node
   :walker-entry-properties
   :walker-entry-visited
   :walker-initialize-cache
   :walker-link-qualifiers
   :walker-node-cache-entry
   :walker-node-properties
   :walker-node-property
   :walker-node-qualifiers
   :walker-node-visited
   :walker-node-visited-p
   :walker-relations
   ))


:de.setf.utility.walker

