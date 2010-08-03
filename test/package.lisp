;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It is the package definition for the test utilities.

;;;  Copyright 2002, 2009, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

(in-package :common-lisp-user)

;; this is already present for some load orders
;; for other this guarantees it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern #.(string :test) :de.setf.utility.implementation))

(defpackage :de.setf.utility.test
  (:use )
  (:nicknames :test)
  (:import-from :de.setf.utility.implementation
                :test)
  (:export
   :*class.test-unit*
   :*test-unit-mode*
   :*test-output*
   :*test-unit*
   :define-test
   :deftest
   :deftests
   :execute-test
   :execute-tests
   :find-monitor
   :find-tests
   :find-test
   :function-monitor
   :generic-function-monitor
   :ignored-error
   :initialize-monitor
   :make-test-unit
   :method-monitor
   :monitor
   :report-monitor
   :test-unit
   :test-unit-path
   :test-unit-status
   :test-unit-verbose-p
   :unintern-test
   :unmonitor
   :test
   :test-and
   :test-equal
   :test-unit-situation
   :time-and-memory
   :with-test-situation
   ))

(modpackage :de.setf.utility
  (:export-from :test))

(pushnew :de.setf.utility.test *features*)

