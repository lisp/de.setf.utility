;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.mime
  :nicknames (:setf.mime)
  :depends-on (:de.setf.utility)
  :serial t
  :components ((:file "package")
               (:file "mime")
               (:file "content-encoding")))


:de.setf.utility.mime
