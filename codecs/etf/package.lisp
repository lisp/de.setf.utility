;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

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



(defpackage :de.setf.utility.etf
  (:use )
  (:nicknames :etf)
  (:export :*intern-operator*
           :*package*
           :atom_cache_ref
           :atom_ext
           :binary_ext
           :bit_binary_ext
           :export_ext
           :float_ext
           :fun_ext
           :integer_ext
           :large_tuple_ext
           :new_float_ext
           :new_fun_ext
           :nil_ext
           :large_big_ext
           :list_ext
           :new_reference_ext
           :pid_ext
           :port_ext
           :reference_ext
           :small_atom_ext
           :small_big_ext
           :small_integer_ext
           :small_tuple_ext
           :string_ext

           :nil
           :true
           :false

           :decode-term
           :encode-term
           :term-to-binary              ; NYI - need to promote vector streams
           :binary-to-term              ; NYI 

           :stream-read-term
           :stream-write-term
           :buffer-set-term
           :buffer-get-term)

  (:documentation "the home package for the Erlang 'external term format' tag names, and interface
 operators names. It includes all standard names, even though not all are implemented."))
