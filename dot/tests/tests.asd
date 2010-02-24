;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.dot.test
  :nicknames (:setf.dot.test)
  :serial t
  :depends-on (:de.setf.utility.dot)
  :components ((:file "dot")))

:de.setf.utility.dot.tests
