;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;  This file part of the 'de.setf.utility' Common Lisp library.
;;;  It defines utility functions for clos

;;;  Copyright 2003, 2009, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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


;;;
;;; content : clos utilities 
;;;
;;;  def-class-constructor (name . methods)
;;;    define a generic operator named for the class to coerce the arguemtns to
;;;    an instance. Accepts instances, initarg lists, and a class togetehr with
;;;    the initargs
;;;  protected-print-unreadable-object (object stream)
;;;    print-unreadable-object with error protection
;;;  def-delegate-method ((instance-variable class-variale) parameters . options)
;;;

;;; 20030902 jaa : incorporated the class definition macros from cl-xml
;;; 20090103 jaa : minimum clos utilities factored out of full clos module
;;; 20100125 jaa : bound-slot-value (aka _slot-value)

(in-package :de.setf.utility.implementation)

(modpackage :de.setf.utility
  (:export 
   :_slot-value
   :bound-slot-value
   :def-class-constructor
   :def-class-constructors
   :def-class-parameter
   :def-delegate-method
   :def-type-predicate
   :def-type-predicates
   :protected-print-unreadable-object))


(defparameter *defclass-output* nil
  "When non-null, macros trace to this stream.")

(defgeneric bound-slot-value (object slot &optional default)
  (:documentation
   "if the object binds the designated slot, return the value.
    otherwise return the given default.")

  (:method ((object standard-object) slot-designator &optional (default nil))
    ;; observed to be faster to check than to muzzle
    (if (slot-boundp object slot-designator)
      (slot-value object slot-designator)
      default)))

(defmacro _slot-value (object slot &rest args)
  `(bound-slot-value ,object ,slot ,@args))

;;
;; class definition macros


(defmacro def-class-constructor (class-name &rest options)
  "Given a class-name, define a generic function which comprises constructors for the named class.
 The options can include clauses for
 - :method : method definitons in additon to or instead of the default definitions
 _ :package : to specific a package for symbols - default `*package*.
 - :constructor : the name for the constructor. Can supplant the `make-` constructor.
 - : 
 the same name as the class. Defines methods for
 - instances : return the instance
 - a single designator list : spread and recurse
 - a spread initarg list : distinguish an initial keyword from a class name and apply `make-instance`
   accordingly. an initial keyword indicates to use the default class
 - t : use the default class
 Defines a global variable named `*class.`_`class-name`_`*` bound to the class name, to be used as
 the default class.  
 Defines a predicate named `_class-name_-p`.
 Defines an additional constructor named `make-`_`class-name`_ which accepts a spread initialization argument
 list.
 NB. Requires that source code is ordered to such that the class has been defined."

  (let* ((lambda-list (if (and options 
                               (not (or (keywordp (caar options))
                                        (eq (caar options) 'declare))))
                        (pop options)
                        '(designator &rest args)))
         (package (or (second (assoc :package options)) (symbol-package class-name)))
         (package-form (or (second (assoc :package options))
                           `(symbol-package ',class-name))) ;; *package*))
         (constructor-name (or (second (assoc :constructor options)) class-name))
         (make-function-name (cons-symbol package :make- class-name))
         (class-variable-name (cons-symbol package :*class. class-name "*"))
         (predicate-name (or (second (assoc :predicate options))
                             (cons-symbol package class-name :-p)))
         (rest-arg (second (member '&rest lambda-list))))
    ;; iff the keyword parameters are specified, (at least for mcl) one must specify
    ;; &allow-other-keys in order to permit varied argument forms to the symbol
    ;; method
    (when (and (member '&key lambda-list) (not (member '&allow-other-keys lambda-list)))
      (warn "lambda list ~a augmented with &allow-other-keys." lambda-list)
      (setf lambda-list (append lambda-list '(&allow-other-keys))))
    (proclaim `(special ,class-variable-name))
    `(progn
       (eval-when (:execute :compile-toplevel :load-toplevel)
         (export ',class-name ,package-form)
         (export ',predicate-name ,package-form)
         (export ',make-function-name ,package-form)
         (export ',class-variable-name ,package-form))
       (defparameter ,class-variable-name ',class-name
         "binds the concrete class designator to use when instantiating
          these, but specification is initargs only.
          this must be a symbol as it may be used as the constructor
          function designator")
       (defgeneric ,predicate-name (datum)
         (:method ((datum t)) nil)
         (:method ((datum ,class-name)) t))
       (defgeneric ,constructor-name ,lambda-list
         ,(or (assoc :documentation options)
             `(:documentation
               ,(format nil "Construct a ~a given initialization arguments. The
 first argument may be a keyword, in which case the value of ~a designate the
 class, or it may be a class designator. In other cases, the first argument
 is interpreted as a context to dereference the respective ~a."
                        class-name class-variable-name class-name)))
         ,@(remove-duplicates 
          (append (remove :method options :key #'first :test-not #'eq)
                  `((:method ((class-designator (eql t)) &rest initargs)
                             (apply ',constructor-name ,class-variable-name
                                    initargs))
                    (:method ((instance ,class-name) &key &allow-other-keys)
                             instance)
                    (:method ((initargs list) &rest args)
                             "given a list, spread it and recurse."
                             #+ignore ;; allow args to override a static definition
                             (when args
                               (error "arguments not allowed: ~s." args))
                             (apply ',constructor-name (first initargs)
                                    (nconc args (rest initargs))))
                    (:method ((keyword symbol) &rest rest-initargs)
                             (if (keywordp keyword)
                               ;; if the first thing is a keyword, interpose the default class
                               (apply #'make-instance ,class-variable-name
                                      keyword rest-initargs)
                               ;; otherwise, the first thing is a class designator, but constrain it
                               (,constructor-name (apply #'make-instance keyword rest-initargs))))))
          :from-end t :test #'equal :key #'(lambda (m) (second (first (second m))))))
       ,@(unless (eq make-function-name constructor-name)
           `((defun ,make-function-name (&rest initargs)
               (declare (dynamic-extent initargs))
               (apply #',constructor-name ,class-variable-name initargs)))))))

(defmacro def-class-constructors (&rest forms)
  `(progn ,@(mapcar #'(lambda (form) `(def-class-constructor ,@(if (consp form) form (list form))))
                    forms)))

(defmacro def-class-parameter (name &optional documentation)
  (let* ((parameter (cons-symbol (symbol-package name) :*class. name "*"))
         (form `(defParameter ,parameter ,@(when documentation (list documentation)) (find-class ',name))))
    form))


(defmacro def-type-predicate (spec &aux (name (if (consp spec) (first spec) spec))
                                 (type-names (when (consp spec) (rest spec))))
  (let* ((p-predicate (cons-symbol (symbol-package name) name :-p))
         (function (when (fboundp p-predicate) (symbol-function p-predicate)))
         (unspecialized-p (or (not function)
                              (null (compute-applicable-methods function '(t)))))
         ;; use individual defmethods so as to not delete defgeneric default constituent
         (form `(progn
                  (defMethod ,p-predicate ((x ,name)) t)
                  ,@(when unspecialized-p `((defMethod ,p-predicate ((x t)) nil)))
                  ,@(mapcar #'(lambda (type) `(defMethod ,p-predicate ((datum ,type)) t)) type-names))))
    (when *defclass-output*
      (pprint form *defclass-output*))
    form))


(defmacro def-type-predicates (&rest specs)
  `(progn ,@(mapcar #'(lambda (spec) `(def-type-predicate ,spec)) specs)))




(defmacro protected-print-unreadable-object ((object stream &rest args) &rest body)
  "combine PRINT-UNREADABLE-OBJECT with an error handler"
  `(handler-case
     (print-unreadable-object (,object ,stream ,@args) ,@body)
     (error (c)
      (handler-case
        (progn (write-string  "Error printing " ,stream)
               (print-unreadable-object (,object ,stream :identity t))
               (format stream " : ~a" c))
        (error (c)
          (declare (ignore c))
          (warn "#<Recursive printing error>"))))))


(defmacro def-delegate-method (designators ((instance class) . rest-parameters)
                                           &key (slot nil)
                                           (accessors nil)
                                           (reference-methods designators)
                                           (if-does-not-exist :error)
                                           (if-null if-does-not-exist)
                                           (if-unbound nil)
                                           (default nil))
  "generate a delegate method.
 :if-null specifies what to do when the slot is null.
 this can be null, be the keyword :error, a plist of the form (:read read-option :write write-option)
 or a function designator. if null, then the delegation happens only if the slot is non-null.
 if :error or t no check is performed and the reference proceeds for null referent as well.
 if a function designator, then that is invoked to supply a value for the
 referent, which is also bound as a side-effect.
 :if-unbound specifies what to do if the slot is not bound."

  (flet ((read-operator (designators)
           (if (consp designators) (first designators) designators))
         (write-operator (designators)
           (if (consp designators) (second designators) `(setf ,designators)))
         (arguments (parameters)
           (mapcar #'(lambda (arg) (if (listp arg) (first arg) arg)) parameters)))
  (let ((reader-method (read-operator designators))
        (writer-method (write-operator designators))
        (reader-accessor (read-operator accessors))
        (writer-accessor (write-operator accessors))
        (reference-reader-method (read-operator reference-methods))
        (reference-writer-method (write-operator reference-methods))
        (option nil)
        (value (gensym)))
    (when reader-method
      (let ((body (case (setf option (if (consp if-null) (getf if-null :read :error) if-null))
                    (:default `(if ,value (,reference-reader-method ,value) ,default))
                    ((t :error) `(,reference-reader-method ,value))
                    ((nil) `(when ,value (,reference-reader-method ,value)))
                    (t `(,reference-reader-method
                         (or ,value (setf ,(cond (writer-accessor (second writer-accessor))
                                                 (reader-accessor `(slot-value ,instance ',slot))
                                                 (t slot))
                                          (,option ,instance))))))))
        (setf body
              (if reader-accessor
                `(let ((,value (,reader-accessor ,instance ,@(arguments rest-parameters)))) ,body)
                `(with-slots ((,value ,slot)) ,body)))
        (when if-unbound
          (setf body
                `(progn (unless (slot-boundp ,instance ',slot) (,if-unbound ,instance))
                        ,body)))
        (setf reader-method
            `(defMethod ,reader-method ((,instance ,class) ,@rest-parameters)
               ,(format nil "delegated to value of ~a property." slot)
               ,body))))
    (when writer-method
      (let ((body
             `(macrolet ((write-form (value)
                           (if (consp reference-writer-method)
                             `(setf (,,(second reference-writer-method) ,value) new-value)
                             `(,,reference-writer-method ,value new-value))))
                ,(case (setf option (if (consp if-null) (getf if-null :write :error) if-null))
                   (:default `(if ,value (write-form ,value) ,default))
                   ((t :error) `(write-form ,value))
                   ((nil) `(when ,value (write-form ,value)))
                   (t `(write-form (or ,value (setf ,(cond (writer-accessor (second writer-accessor))
                                                           (reader-accessor `(slot-value ,instance ',slot))
                                                           (t slot))
                                                    (,option ,instance)))))))))
        (setf body
              (if reader-accessor
                `(let ((,value (,reader-accessor ,instance ,@(arguments rest-parameters)))) ,body)
                `(with-slots ((,value ,slot)) ,body)))
        (when if-unbound
          (setf body
                `(progn (unless (slot-boundp ,instance ',slot) (,if-unbound ,instance))
                        ,body)))
        (setf writer-method
              `(defMethod ,writer-method (new-value (,instance ,class) ,@rest-parameters)
                 ,(format nil "delegated to value of ~a property." slot)
                 ,body))))

    (if reader-method
      (if writer-method
        `(progn ,reader-method ,writer-method)
        reader-method)
      (if writer-method
        writer-method
        (values))))))

:de.setf.utility

