;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.test
  :nicknames (:setf.test)
  :depends-on (:de.setf.utility.dot
               :de.setf.utility.walker
               :de.setf.utility.mime)
  :serial t
  :components ((:file "package")
               (:file "test-unit")
               #+(or digitool clozure) (:file "monitor")
               #+(or digitool clozure) (:file "profiler")))


:de.setf.utility.test
