;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.utility.asdf
  :description "Patches and additions for ASDF"
  :long-description "Patches and additions for ASDF :
 hierarchical-names : interprets names such as that for this system as system location designators relative
                      to the registered search roots.
 contingent-on :      adds that dependency form to the operation process.
 dependency :         adds form-level definition dependency to asdf models post-facto. analyzes an active
                      image to compute operator dependency and impute effective modules. uses the results to
                      genereate the effective asdf system description.
 graph :              encode a system dependency model as a .dot graph.

 See graph.lisp for an example."

  ; use an explict logical to provoke .BIN mapping
  :pathname (when (ignore-errors (logical-pathname-translations "LIBRARY"))
              (make-pathname :host "LIBRARY"
                             :directory '(:absolute "de" "setf" "utility" "asdf")))
  :depends-on (:de.setf.utility.dot
               :de.setf.utility.walker)
  :serial t

  :components ((:file "patches")
               (:file "hierarchical-names")
               (:file "contingent-on")
               (:file "dependency")
               (:file "graph")))


