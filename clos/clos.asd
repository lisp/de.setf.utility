;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the clos module for the 'de.setf.utility' Common Lisp library.
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

(asdf:defsystem :de.setf.utility.clos
  :depends-on (:de.setf.utility)
  :description "CLOS utilities for the de.setf.utility library"
  :components ((:file "package")
               (:file "clos-classes" :depends-on ("package"))
               (:file "clos-methods" :depends-on ("package"))
               (:file "clone-instance" :depends-on ("package"))
               (:file "denominated" :depends-on ("package"))
               (:file "print-object-slots" :depends-on ("package")))

  :long-description
  "`de.setf.utility.clos` adds several utilities for CLOS models:

 - `abstract-standard-class`, `abstract-standard-method`, `abstract-standard-generic-function`
    define a protocol to require method definitions for concrete classes.
 - `clone-instance` generic function defines deep cloning protocol for objects
 - `denominated`  standard method combination variation permits a specializing class to elect and order
    the effective method's constituents
 - `denominated-progn` similar variation on the `progn` combination
 - `locked-standard` a `standard` method combination with a lock
 - `print-object-slots` defines a base method and a definition form (`def-print-object-slots`) to
    support printing objects.")

:de.setf.utility.clos
