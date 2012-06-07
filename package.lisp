;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

;;;  This file is the core package definition for the 'de.setf.utility' Common Lisp library.
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
;;; content : the utility package definition

;;; 20100210.janderson : establised as its own file

(in-package :common-lisp-user)

(defpackage :de.setf.utility
  (:use )
  (:nicknames :d.s.u :dsu)
  #+mcl
  (:import-from :ccl
                :stream-reader
                :stream-writer
                :stream-tyi
                :stream-tyo)
  (:export
   :*logical-source-type*
   :*logical-binary-type*
   :*package-host-name*
   :*package-operations*
   :*physical-source-type*
   :check-feature
   :clean-package
   :define-library-host
   :defvarconstant
   :edit-package
   :ensure-package
   :find-packages
   :load-package
   :make-binary-translation-target
   :modpackage
   :modify-package
   :modify-package-operation
   :package-not-found
   :package-pathname
   :package-version
   :purge-package
   :require-features
   :runtime-directory-name
   :set-relative-logical-pathname-translations
   :stream-reader
   :stream-writer
   :stream-tyi
   :stream-tyo))

(defpackage :de.setf.utility.implementation
  (:use #+:CCL :ccl
        :common-lisp
        :de.setf.utility)
  #+sbcl
  (:import-from :sb-gray
                :stream-line-column
                :stream-write-char
                :stream-write-sequence
                :stream-write-string)
  (:documentation "This is the package for source files in the :de.setf.utility library module."))


(pushnew :de.setf.utility *features*)
