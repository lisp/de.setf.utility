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

;;; needs to be in this file rather than its own.
;;; otherwise it shadows the .asd from the test module

(asdf:defsystem :de.setf.utility.test.test
  :depends-on (:de.setf.utility.test)
  :components ((:module "test"
                :components ((:file "test-unit")
                             (:file "monitor")))))


:de.setf.utility.test
