;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.test.rspec
  :depends-on (:de.setf.utility.test
               :de.setf.utility.codecs.etf
               #+sbcl
               :sb-posix)
  :serial t
  :components ((:file "package")
               (:file "rspec"))
  :long-description
  "These files implement a primitive BERT-coded repl to permit to drive
 a LISP process from Ruby for rspec-driven tests. See `rspec-lisp.rb` for
 a sketch of the connection mechanism.")

