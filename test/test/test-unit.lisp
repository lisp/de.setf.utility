;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' library component.
;;;  (c) 2002, 2009 james anderson
;;;
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with 'de.setf.utility'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.

;;;  tests for :de.setf.utility.test

;;;  20090505 ja : new

(in-package :de.setf.utility.implementation)

(dsu:test test-unit-prerequisite
  (eq (test-unit-name *test-unit*) :test-unit-prerequisite))

(dsu:test test-unit.1
  (equal 1 1)
  :mode :verbose)

(dsu:test test-unit.1
  (equal 1 1)
  :mode :report)

(dsu:test test-unit.1
  (equal 1 1)
  :mode :silent)

(dsu:test test-unit.1
  (equal 1 1)
  :mode nil
  :prerequisites '(test-unit-prerequisite))

(dsu:test test-unit.2
  (equal 1 0))


(dsu:test execute-test.1
  "test with a wild-card test name, that passed/failed count is correct 
 and include the prerequisite,"
  (dsu:execute-test :test-unit.** :force-p t)
  :values '(:failed 2 1 0 0))

;;(dsu:execute-test :execute-test.1)

:de.setf.utility
