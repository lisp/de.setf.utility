;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the system definition for the ETF codec module for the 'de.setf.utility' Common Lisp library.
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

(asdf:defsystem :de.setf.utility.codecs.etf
  :version "0.1"
  :depends-on (:de.setf.utility.codecs)
  :serial t
  :description "ETF/BERT stream and buffer codecs."
  :components ((:file "package")
               (:file "etf"))
  :long-description "This module implements BERT[[1]] codecs based on the `de.setf.utility.codecs` operators
 for streams and byte buffer coding. It supports just those aspects of 'Erlang term format' required for to encode
 call requests and responses consistent with BERT, but does define operators for the entire RPC protocol.
 This means that it is, by intention, much less complete than cleric[[2]].
 ---
 [1] : [bert-rpc.org](http://bert-rpc.org/)
 [2] : [CLERIC](http://github.com/flambard/CLERIC)")
