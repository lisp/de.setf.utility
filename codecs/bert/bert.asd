;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the system definition for the BERT codec module for the 'de.setf.utility' Common Lisp library.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; `de.setf.utility` is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the the GNU Lesser General Public License as published by the Free Software Foundation.
;;;
;;; `de.setf.utility` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with `de.setf.utility`, as `lgpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).


(in-package :cl-user)

(asdf:defsystem :de.setf.utility.codecs.bert
  :version "0.1"
  :depends-on (:de.setf.utility.codecs)
  :serial t
  :description "BERT stream and buffer codecs."
  :components ((:file "bert"))
  :long-description)


