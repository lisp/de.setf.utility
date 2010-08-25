;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.test.rspec
  :depends-on (:de.setf.utility.test
               :de.setf.utility.codecs.etf)
  :serial t
  :components ((:file "package")
               (:file "rspec")))

