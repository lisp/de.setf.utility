;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(:documentation
  "This file is the system definition for tests for the `de.setf.utility.codecs` Connon Lisp library."

 (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
 'de.setf.utility.codecs' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility.codecs' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility.codecs, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (:history
  (20101028 "extractd and extended from amqp codec tests to match the independent library interface.")))


(asdf:defsystem :de.setf.utility.codecs.test
  :serial t
  :version 20101026
  :depends-on (:de.setf.utility.codecs
               :de.setf.utility.test)
  :components ((:file "types")
               (:file "utilities")
               (:file "vector-stream")
               (:file "byte-codecs")
               (:file "character-codecs")
               (:file "float-codecs"))

  :description
  "This is the sub-library for testing :de.setf.utility.codecs")



;;; (asdf:load-system :de.setf.utility.codecs.test)
;;; (test:execute-test :codecs.* :mode :report)


