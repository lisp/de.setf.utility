;;; -*- Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;;  This file extends the package definition for 'de.setf.utility' Common Lisp library
;;;  to incorporate CLOS-MOP symbols from various runtimes and re-export them for basic MOP
;;;  operations.
;;;
;;;  Copyright 2003,2009,2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).


;;; 2009-04-05  james.anderson@setf.de  factored out the package declaration from clos-classes.lisp


(de.setf.utility:modpackage :de.setf.utility
  (:use-only )
  (:use-by :de.setf.utility.implementation)
  #+Genera (:import-from "CLOS-INTERNALS" "FUNCALLABLE-STANDARD-CLASS" "VALIDATE-SUPERCLASS")
  #+ccl
  (:import-from :ccl
                :class-default-initargs
                :class-direct-subclasses
                :class-direct-superclasses
                :class-direct-slots
                :class-finalized-p
                :class-precedence-list
                :class-prototype
                :class-slots
                :direct-slot-definition-class
                :effective-slot-definition-class
                :finalize-inheritance
                :find-method-combination
                :funcallable-standard-class
                :generic-function-lambda-list
                :generic-function-methods
                :generic-function-method-class
                :generic-function-name
                :method-specializers
                :slot-definition
                :slot-definition-initargs
                :slot-definition-name
                :slot-definition-type
                :slot-definition-readers
                :slot-definition-writers
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :validate-superclass
                )
  #+allegro
  (:import-from :mop
                :class-default-initargs
                :class-direct-subclasses
                :class-direct-superclasses
                :class-direct-slots
                :class-finalized-p
                :class-precedence-list
                :class-prototype
                :class-slots
                :direct-slot-definition-class
                :effective-slot-definition-class
                :finalize-inheritance
                :find-method-combination
                :funcallable-standard-class
                :generic-function-lambda-list
                :generic-function-method-class
                :generic-function-methods
                :generic-function-name
                :method-specializers
                :slot-definition
                :slot-definition-initargs
                :slot-definition-name
                :slot-definition-readers
                :slot-definition-type
                :slot-definition-readers
                :slot-definition-writers
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :validate-superclass
                )
  #+lispworks
  (:import-from :clos
                :class-default-initargs
                :class-direct-subclasses
                :class-direct-superclasses
                :class-direct-slots
                :class-finalized-p
                :class-precedence-list
                :class-prototype
                :class-slots
                :direct-slot-definition-class
                :effective-slot-definition-class
                :finalize-inheritance
                :find-method-combination
                :funcallable-standard-class
                :generic-function-lambda-list
                :generic-function-name
                :generic-function-method-class
                :generic-function-methods
                :method-specializers
                :slot-definition
                :slot-definition-initargs
                :slot-definition-name
                :slot-definition-readers
                :slot-definition-type
                :slot-definition-readers
                :slot-definition-writers
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :validate-superclass
                )
  #+openmcl-partial-mop                 ; changed from 0.13.6 to 0.14
  (:import-from :ccl
                :generic-function-name
                :validate-superclass)
  #+mcl-mop-2
  (:import-from :ccl
                :class-default-initargs
                :class-direct-slots
                :class-direct-subclasses
                :class-direct-superclasses
                :class-finalized-p
                :class-precedence-list
                :class-prototype
                :class-slots
                :direct-slot-definition-class
                :effective-slot-definition-class
                :finalize-inheritance
                :funcallable-standard-class
                :generic-function-name
                :method-specializers
                :slot-definition
                :slot-definition-initargs
                :slot-definition-name
                :slot-definition-readers
                :slot-definition-writers
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :validate-superclass
                )
  #+sbcl
  (:import-from :sb-mop
                :class-default-initargs
                :class-direct-subclasses
                :class-direct-superclasses
                :class-direct-slots
                :class-finalized-p
                :class-precedence-list
                :class-prototype
                :class-slots
                :direct-slot-definition-class
                :effective-slot-definition-class
                :finalize-inheritance
                :find-method-combination
                :funcallable-standard-class
                :generic-function-lambda-list
                :generic-function-name
                :generic-function-method-class
                :generic-function-methods
                :method-specializers
                :slot-definition
                :slot-definition-initargs
                :slot-definition-name
                :slot-definition-readers
                :slot-definition-type
                :slot-definition-readers
                :slot-definition-writers
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :validate-superclass
                )
  (:export
   :abstract-standard-class
   :abstract-standard-generic-function
   :abstract-standard-method
   :abstract-method-error
   :abstract-method-p
   :class-default-initargs
   :class-direct-slots
   :class-direct-built-in-slots
   :class-direct-object-slots
   :class-direct-related-classes
   :class-direct-subclasses
   :class-direct-superclasses
   :class-related-classes
   :class-finalized-p
   :class-precedence-list
   :class-prototype
   :class-slot-names
   :class-slots
   :compute-effective-initargs
   :concrete-standard
   :copy-slots
   :def-abstract-class
   :def-abstract-generic
   :def-abstract-method
   :def-class-constructor
   :def-class-constructors
   :def-class-parameter
   :def-delegate-method
   :def-delegate-reader
   :def-type-predicate
   :def-type-predicates
   :denominated
   :denominated-progn
   :direct-slot-definition-class
   :effective-slot-definition-class
   :encode-instance-as
   :finalize-if-needed
   :finalize-inheritance
   :find-method-combination
   :funcallable-standard-class
   :generic-function-lambda-list
   :generic-function-method-class
   :generic-function-methods
   :generic-function-name
   :locked-generic-function
   :locked-standard
   :method-specializers
   :named-generic-function
   :required-initarg
   :required-initarg-error
   :short-standard
   :slot-definition
   :slot-definition-initargs
   :slot-definition-name
   :slot-definition-readers
   :slot-definition-type
   :slot-definition-writers
   :standard-direct-slot-definition
   :standard-effective-slot-definition
   :validate-superclass
   :with-special-readers
   :with-special-slots
   ))

#+(and :allegro-version>= (version>= 7 0))
(pushnew :documentation-ansi *features*)
