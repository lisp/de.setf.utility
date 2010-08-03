;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the 'de.setf.utility' Common Lisp library.
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

#+:de.setf.utility                      ; once it has been loaded, loading the system definition
(eval-when (:compile-toplevel :execute)
  (when (find-package :de.setf.utility) ; reroots the hosts
    (de.setf.utility::define-library-host (or *compile-file-pathname* *load-pathname*)))) 

(asdf:defsystem :de.setf.utility
  :version "20100214-1.0"
  ;;:pathname (when (ignore-errors (logical-pathname-translations "LIBRARY"))
  ;;            (make-pathname :host "LIBRARY"
  ;;                           :directory '(:absolute "de" "setf" "utility")))
  :serial t
  :components ((:file "package")
               (:file "pathnames")
               (:file "modpackage")
               (:file "documentation-stub"
                      :depends-on ("modpackage"))
               (:file "string")
               (:file "conditions")
               (:module "clos"          ; minimal clos utilities
                :depends-on ("string")
                :components ((:file "clos-classes")))
               (:module "test"          ; minimal test unit utilities
                :components ((:file "package")
                             (:file "test-unit" :depends-on ("package"))))
               (:file "date"
                      :depends-on ("modpackage"))
               (:file "list"
                      :depends-on ("modpackage"))))
