;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-
;;;

(in-package :de.setf.utility.implementation)

(:documentation "This file defines a parameters for the walking/graphic operators for the 'de.setf.utility'
 library."
  
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
   (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators and
 isolate the runtime-depencies.")))


(defparameter *graph-rankdir* "LR"
  "The graphviz graph orientation (rank direction) for function, packages, etc. graphs.")

(defparameter *graph-size* "8.5,11"
  "The graphviz page size for function, package, etc. graphs")

(defparameter  *graph-ratio* "compress"
  "The graphviz ratio for function, package, etc. graphs")

(defparameter *graph-margin* ".5"
  "The graphviz page margin for function, package, etc. graphs")


(defparameter *walk-extent* nil
  "The collection of objects within which navigition is to remain. Can be instances or packages.
 If *walk-extent-boundary* is :open, this restricts link operations as well. If the boundary is :closed,
 then the link operation is included, but the respective node is not.")

(defparameter *walk-extent-boundary* :open
  "See *walk-extent*.")

(defparameter *walk-depth* nil
  "bound when walking to permit applications to constrain depth.")

;; dot-graphing for classes

(defvar *walk-superclass-link* 'superclass
  "binds the link identifier for class-to-superclass links. the default value is superclass")

(defvar *walk-subclass-link* 'subclass
  "binds the link identifier for class-to-superclass links. the default value is subclass")

(defvar *walker* nil
  "binds the current walker within walk-model.")

(defvar *walker-cache* nil
  "bound to the the current walker's cache - or some variation thereof during documentation generation.")

(defparameter *walker-cycle* ())


:de.setf.utility.walker

