;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  This file is the system definition for the 'de.setf.utility.dot' (or 'setf.dot') library component.
  </DESCRIPTION>
 <COPYRIGHT YEAR='2009' AUTHOR='james adam anderson' href='mailto:james.anderson@setf.de'>
  'setf.dot' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'setf.dot' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'setf.dot'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.
  </COPYRIGHT>
 </DOCUMENTATION>
|#

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.dot
  :depends-on (:de.setf.utility)
  :serial t
  :components ((:file "package")
               (:file "dot"))
  :description
  "setf.dot implements various encoding interfaces and a model for graphvis graphic
 descriptions."
  :long-description
  "See index.html for a complete description.")

:de.setf.utility.dot
