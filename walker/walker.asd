;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the walker module for the 'de.setf.utility' Common Lisp library.
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

(asdf:defsystem :de.setf.utility.walker
  :depends-on (:de.setf.utility.dot
               :de.setf.utility.clos
               :net.common-lisp.closer-mop
               #+sbcl
               :sb-introspect
               #+mcl
               :de.setf.utility.bsd
               )
  :serial t
  :components ((:file "package")
               (:file "parameters")
               (:file "introspection")
               (:file "walker")
               (:file "introspective-walker")
               (:file "source-walker")
               (:file "class-graph")
               (:file "function-graph")
               (:file "package-graph")
               #+mcl
               (:file "mcl")))

:de.setf.utility.walker
