;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;
;;;  This file is the system definition for the mime module for the 'de.setf.utility' Common Lisp library.
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


(asdf:defsystem :de.setf.utility.mime
  :nicknames (:setf.mime)
  :depends-on (:de.setf.utility)
  :description "de.setf.utility.mime defines singletons to designate mime content types and
 codecs for the related content encodings."
  :serial t
  :components ((:file "package")
               (:file "mime")
               (:file "content-encoding"))

  :long-description
  "See : (among others)

   - [wikipedia](http://en.wikipedia.org/wiki/MIME)
   - [rfc2046](http://tools.ietf.org/html/rfc2046) : (MIME) Part Two: Media Types
   - [rfc2049](http://tools.ietf.org/html/rfc2049) : (MIME) Part Five: Conformance Criteria and Examples

   Each type is defined as a singleton in a major/minor type lattice and bound to a
 global variable with the same name. The `text/*` types include a slot for a content encoding
 name.")



:de.setf.utility.mime
