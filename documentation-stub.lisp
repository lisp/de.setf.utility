;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

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


(in-package :de.setf.utility.implementation)

(defmacro :documentation (&rest arguments)
  (declare (ignore arguments))
  (values))

#+mcl
(progn
  (setf (ccl:assq ':documentation *fred-special-indent-alist*) 1))

(defgeneric test-features (specification)
  (:method ((spec symbol))
    (when (member spec *features*) t))
  (:method ((spec null))
    t)
  (:method ((spec cons))
    (ecase (pop spec)
      (and (every #'test-features spec))
      (or (some #'test-features spec))
      (not (not (test-features (first spec)))))))

(defmacro require-features ((&rest features) message &rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (test-features ',features)
       (cerror "Continue anyway." ,message ,@args))))
