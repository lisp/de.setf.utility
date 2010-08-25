;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.test.test
  :depends-on (:de.setf.utility.test)
  :components ("test-unit"
               "monitor"))

:de.setf.utility.test
