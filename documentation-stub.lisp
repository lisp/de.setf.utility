;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-


(in-package :common-lisp-user)

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It  file defines a 'null' macro-expansion for the top-level documention operator.
;;;  It is loaded by the minimal utility module to render documentation forms invisible.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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


(in-package :cl-user)

(defmacro :documentation (&rest arguments)
  (declare (ignore arguments))
  (values))

#+mcl
(progn
  (setf (ccl:assq ':documentation *fred-special-indent-alist*) 1))
