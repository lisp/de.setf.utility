;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;; This file is the system definition for the codec module for the 'de.setf.utility' Common Lisp library.
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

(asdf:defsystem :de.setf.utility.codecs
  :version "0.1"
  :depends-on (:de.setf.utility.mime)
  :serial t
  :description "binary stream and buffer elementary codecs."
  :components ((:file "package")
               (:file "types")
               (:file "utilities")
               (:file "byte-codecs")
               (:file "character-codecs")
               (:file "float-codecs"))
  :long-description
  "`de.setf.utility.codecs` implements binary codecs for Lisp data, buffers, and streams.
 The primitive types are integers and float values, and character sequences. The operators combine the
 core capabilities of the AMQP[[1]] data-wire-coding and Apache Thrft[[2]] binary protocol to serve as the
 base implementation for further codecs, such as BERT[[3]] and Apache Avro[[4]].
 ----

 [1]: http://www.amqp.org/
 [2]: http://incubator.apache.org/thrift/
 [3]: http://bert-rpc.org/
 [4]: http://avro.apache.org/ ")


