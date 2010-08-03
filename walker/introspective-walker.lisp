;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "This file defines image walking operators for the 'de.setf.utility' library."
  
  (copyright
   "Copyright 2003, 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (history
   (delta 20031101)
   (delta 20031210 "corrected qualifier slot name for walker class.
   changed function/function-name coercion and resolution to guard against macros, and to
   observe ccl callers constraints.")
   (delta 20060428 "janderson" "extended function calls to recognize apply")
   (delta 20090329 "janderson" "added class reference to function-calls")
   (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators, isolate the
 runtime-depencies, and separate the package/function/class graphing operators."))
  
  (long-description "Navigate the runtime object graph based on a combination of first-class data,
 such as package constituency, and runtime-specific data, such as function callers."))


;;;
;;; utilities

(defun coerce-to-class (datum)
  (etypecase datum
    (symbol (find-class datum))
    (class datum)))


;;;
;;; walking control classes

(defclass class-walker (preorder-node-walker
                        preorder-link-walker
                        cyclic-walker)
  ((class-qualifiers
    :initform '(subclass superclass binds relations other specializes)
    :allocation :class)))


(defclass function-walker (preorder-node-walker
                           preorder-link-walker
                           cyclic-walker)
  ((class-qualifiers
    :allocation :class
    :initform '(callers calls relations other method)
    :documentation
    "in general, a function walker combines methods for
     caller calls relations and other.")))


(defclass package-walker (preorder-node-walker
                          preorder-link-walker
                          cyclic-walker)
  ((class-qualifiers
    :initform '(used-by imports uses relations other internal external)
    :allocation :class)))


(defclass symbol-walker (node-walker cyclic-walker)
  ((class-qualifiers
    :initform '(function macro class)
    ;; NYI:
    ;; condition compiler-macro constant method-combination symbol-macro type variable 
    :allocation :class)))
                         


(defclass introspective-walker (package-walker class-walker function-walker symbol-walker)
  ())

;;;
;;; traversal operations

(defmethod walk-node constituents ((walker function-walker) (function function) (op t))
  ;; there are none
  function)


;;!!! these links should be in an external store, not in the cache
(defmethod walk-node-predecessors calls
           ((walker function-walker) (function function) (op t))
  (dolist (calls (function-calls function))
    (when (setf calls (coerce-to-function calls))
      (unless (find function (getf (walker-node-properties walker calls) 'callers))
        (push calls (getf (walker-node-properties walker function) 'calls))
        (walk-link walker 'calls function calls op))))
  function)

(defmethod walk-node-successors callers
           ((walker function-walker) (function function) (op t))
  "Iterate over the function's callers, normalize to a function object, and navigate through
   the link."
  (dolist (caller (function-callers function))
    (when (setf caller (coerce-to-function caller))
      (unless (find function (getf (walker-node-properties walker caller) 'calls))
        (push caller (getf (walker-node-properties walker function) 'callers))
        (walk-link walker 'caller function caller op))))
  function)

(defmethod in-extent-p ((object function))
  (when (or (find object *walk-extent*) (find (function-package object) *walk-extent*))
    t))




(defmethod walk-node-predecessors uses
           ((walker package-walker) (package package) (op t))
  (dolist (uses (package-use-list package))
    (unless (find package (getf (walker-node-properties walker uses) 'used-by))
      (push uses (getf (walker-node-properties walker package) 'uses))
      (walk-link walker 'uses package uses op)))
  package)

(defmethod walk-node-predecessors imports
           ((walker package-walker) (package package) (op t))
  (let ((imports nil))
    (with-package-iterator (next-symbol package :internal :external)
      (loop (multiple-value-bind (more symbol) (next-symbol)
              (unless more (return))
              (unless (eq (symbol-package symbol) package)
                (pushnew (symbol-package symbol) imports)))))
    (when imports (map-walk-link imports walker 'imports package op)))
  package)

(defmethod walk-node-successors used-by
           ((walker package-walker) (package package) (op t))
  (dolist (used-by (package-used-by-list package))
    (unless (find package (getf (walker-node-properties walker used-by) 'uses))
      (push used-by (getf (walker-node-properties walker package) 'used-by))
      (walk-link walker 'used-by package used-by op)))
  package)

(defmethod walk-node-constituents internal
           ((walker package-walker) (package package) (op t))
  "walk the packages owned, internal symbols"
  (with-package-iterator (next package :internal)
    (loop (multiple-value-bind (next-p symbol) (next)
            (unless next-p (return))
            (when (eq package (symbol-package symbol))
              (walk-link walker 'internal package symbol op)))))
  package)

(defmethod walk-node-constituents external
           ((walker package-walker) (package package) (op t))
  "walk the packages owned, internal symbols"
  (with-package-iterator (next package :external)
    (loop (multiple-value-bind (next-p symbol) (next)
            (unless next-p (return))
            (when (eq package (symbol-package symbol))
              (walk-link walker 'external package symbol op)))))
  package)


(defmethod in-extent-p ((object package))
  (when (find object *walk-extent*) t))


(defmethod walk-node-successors subclass
           ((walker class-walker) (class class) (op t))
  (dolist (subclass  (class-direct-subclasses class))
    (unless (find class (getf (walker-node-properties walker subclass) 'superclass))
      (push subclass (getf (walker-node-properties walker class) 'subclass))
      (walk-link walker *walk-subclass-link* class subclass op)))
  class)

(defmethod walk-node-predecessors superclass
           ((walker class-walker) (class class) (op t))
  (dolist (superclass  (class-direct-superclasses class))
    (setf superclass (coerce-to-class superclass))
    (unless (find class (getf (walker-node-properties walker superclass) 'subclass))
      (push superclass (getf (walker-node-properties walker class) 'superclass))
      (walk-link walker *walk-superclass-link* class superclass op)))
  class)

(defmethod walk-node-constituents binds
           ((walker class-walker) (class standard-class) (op t))
  (flet ((walk-slot (slot)
           (let ((type (slot-definition-type slot))
                 (name (slot-definition-name slot))
                 (slot-class nil))
             (when (and type (not (eq type t)) (symbolp type)
                        (setf slot-class (find-class type nil)))
               (walk-link walker name class slot-class op)))))
    (declare (dynamic-extent #'walk-slot))
    (map nil #'walk-slot (c2mop:class-direct-slots class)))
  class)

(defmethod walk-node-constituents specializes
           ((walker class-walker) (class standard-class) (op t))
  (flet ((walk-specialized-method (method)
               (walk-link walker 'specializes class method op)))
    (declare (dynamic-extent #'walk-specialized-method))
    (map nil #'walk-specialized-method (c2mop:specializer-direct-methods class)))
  class)


(defmethod in-extent-p ((object class))
  (when (or (find object *walk-extent*) (find (symbol-package (class-name object)) *walk-extent*))
    t))


(defmethod walk-node predecessors ((walker symbol-walker) (symbol symbol) (op t))
  ;; there are none
  (or symbol t))

(defmethod walk-node successors ((walker symbol-walker) (symbol symbol) (op t))
  ;; there are none
  (or symbol t))


(defmethod walk-node-constituents function
           ((walker symbol-walker) (symbol symbol) (op t))
  "walk function values, but skip any which are aliased."
  (when (fboundp symbol)
    (let ((function (fdefinition symbol)))
      (when (eq symbol (dsw:function-name function))
        (walk-link walker 'function symbol function op))))
  (let ((setf-name `(setf ,symbol)))
    (when (fboundp setf-name)
      (let ((function (fdefinition setf-name)))
        (when (equal setf-name (dsw:function-name function))
          (walk-link walker 'setf-function symbol function op)))))
  (or symbol t))


(defmethod walk-node-constituents macro
           ((walker symbol-walker) (symbol symbol) (op t))
  "walk any bound macro."
  (let ((macro-function (macro-function symbol)))
    (when macro-function
      (walk-link walker 'macro symbol (coerce-to-function macro-function) op)))
  (or symbol t))


(defmethod walk-node-constituents class
           ((walker symbol-walker) (symbol symbol) (op t))
  "walk any named class, but skip aliases."
  (let ((class (find-class symbol nil)))
    (when (and class (eq (class-name class) symbol))
      (walk-link walker 'class symbol class op)))
  (or symbol t))


;;;
;;; interface

(defgeneric walk-classes (root classes op &key &allow-other-keys)
  (:argument-precedence-order classes root op)
  
  (:method ((root class) (classes null) op &rest options)
    (apply #'walk-classes root (list (symbol-package (class-name root))) op options))
  
  (:method ((root t) (package-predicate function) op &rest options)
    (apply #'walk-classes root (remove-if-not package-predicate (list-all-packages)) op options))
  
  (:method ((root symbol) (packages t) (op t) &rest options)
    (apply #'walk-classes (find-class root) packages op options))
  
  (:method ((root class) (extent cons) op &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'class-walker options) root op))))
            

(defgeneric walk-packages (root packages op &key &allow-other-keys)
  (:argument-precedence-order packages root op)
  
  (:method ((root t) (packages t) op &rest options)
    (apply #'walk-packages (coerce-to-package root) packages op options))
  
  (:method ((root null) (packages t) (op t) &rest options)
    (apply #'walk-packages (find-package :common-lisp) packages op options))
  
  (:method ((root t) (packages null) op &rest options)
    (apply #'walk-packages root (list-all-packages) op options))
  
  (:method ((root t) (package-predicate function) op &rest options)
    (apply #'walk-packages root (remove-if-not package-predicate (list-all-packages)) op options))
  
  (:method ((root package) (extent cons) op &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'package-walker options) root op))))


(defgeneric walk-functions (root functions op &key &allow-other-keys)
  (:argument-precedence-order functions root op)
  
  (:method ((root function) (functions null) op &rest options)
    (apply #'walk-functions root (list (function-package root)) op options))
  
  (:method ((root t) (package-predicate function) op &rest options)
    (apply #'walk-functions root (remove-if-not package-predicate (list-all-packages)) op options))
  
  (:method ((root symbol) (packages t) (op t) &rest options)
    (apply #'walk-functions (fdefinition root) packages op options))
  
  (:method ((root cons) (extent t) (op t) &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'function-walker options) root op)))
  
  (:method ((root function) (extent cons) op &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'function-walker options) root op))))


(defgeneric walk-image (root extent op &key &allow-other-keys)
  (:argument-precedence-order extent root op)
  
  (:method ((root cons) (extent t) (op t) &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'introspective-walker options) root op)))
  
  (:method ((root function) (extent cons) op &rest options)
    (let ((*walk-extent* (mapcar #'coerce-to-package extent)))
      (walk-model (apply #'make-instance 'introspective-walker options) root op))))


:de.setf.utility.walker

