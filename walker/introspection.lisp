;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (or (intersection '(:digitool :sbcl :allegro) *features*)
              (and (member :clozure *features*)
                   (intersection '(:ppc-target :x86-target) *features*)))
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))


(:documentation "This file defines introspective operators for the image walking module of the 'de.setf.utility'
 library."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (history
  (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators and
 isolate the runtime-depencies.")))


(defun coerce-to-function (datum)
  (typecase datum
    (null nil)
    (symbol (when (fboundp datum) (coerce-to-function (fdefinition datum))))
    (cons
     (cond ((eq (first datum) 'setf) (when (fboundp datum) (fdefinition datum)))
           ((eq (first datum) 'quote) (coerce-to-function (second datum)))))
    (vector (some #'functionp datum))   ; allow for some macro representations
    (function datum)))


#+sbcl
(defun dsw:function-name (function) (sb-impl::fun-name function))

#+allegro
(defun dsw:function-name (function) (xref::object-to-function-name function))


#+ccl                                   ; internals accept a name only
(defgeneric dsw:function-callers (x)
  (:documentation "mcl accepts a function name, but not a function")
  (:method ((x function))
    (dsw:function-callers (dsw:function-name x)))
  (:method ((x symbol))
    (ccl::callers x))
  (:method ((x cons))
    (when (and (eq (first x) 'setf) (fboundp x)) (ccl::callers x))))

#+sbcl                                  ; internals accepts either a function object or a name
(defgeneric dsw:function-callers (x)
  (:method ((x function))
    (mapcar #'sb-impl::fun-name (sb-introspect:find-function-callers x)))
  (:method ((x symbol))
    (remove-duplicates (mapcar #'sb-impl::fun-name (sb-introspect:find-function-callers x))))
  (:method ((x cons))
    (when (and (eq (first x) 'setf) (fboundp x))
      (remove-duplicates (mapcar #'sb-impl::fun-name (sb-introspect:find-function-callers x))))))

#+allegro
(defgeneric dsw:function-callers (x)
  (:method ((x function))
    (dsw:function-callers (dsw:function-name x)))
  (:method ((x symbol))
    (xref:get-relation :direct-calls :wild x))
  (:method ((x cons))
    (when (and (eq (first x) 'setf) (fboundp x))
      (xref:get-relation :direct-calls :wild x))))

(defgeneric dsw:function-calls (function)
  #+(or digitool (and clozure ppc-target))
  (:method ((function function))
    (let ((disassembly (ccl::disassemble-list function))
          (calls nil)
          (arg-form nil))
      (labels ((token-equal (x y)
                 (cond ((consp x)
                        (when (eq (first x) 'quote) (token-equal (second x) y)))
                       ((consp y)
                        (when (eq (first y) 'quote) (token-equal x (second y))))
                       ((and (or (stringp x) (symbolp x)) (or (stringp y) (symbolp y)))
                        (string-equal x y))))
               (push-function-name (x)
                 (when (and (consp x) (eq (first x) 'quote)) (setf x (second x)))
                 (when (functionp x) (setf x (function-name x)))
                 (when (and (symbolp x) (fboundp x)) (pushnew x calls))
                 x)
               (push-class-name (x)
                 (when (and (consp x) (eq (first x) 'quote)) (setf x (second x)))
                 (when (consp x) (setf x (first x)))
                 (when (symbolp x) (pushnew x calls))))
        #+(or)                          ; this is sensitive to  simple calls only
        (loop (unless (rest disassembly) (return))
              (destructuring-bind ((o1 set-nargs? &rest r1) (o2 &optional lwz? reg name fn? &rest r2) &rest r3)
                                  disassembly
                (declare (ignore o1 o2 reg r2 r3))
                ;; (print disassembly)
                (when (and (or (token-equal "set-nargs" set-nargs?)
                               (and (token-equal "bla" set-nargs?) (token-equal ".SPSPREADARGZ" (first r1))))
                           (token-equal "lwz" lwz?)
                           (token-equal "fn" fn?))
                  ;; if it's a function call, save the function name
                  (setf name (push-function-name name))          ; cache the actual name
                  (when (eql name 'ccl::%make-instance)
                    ;; if the call is to make-instance with a constant class, save that
                    (destructuring-bind (&optional o0 lwz? reg? class fn?) arg-form
                      (when (and (numberp o0)
                                 (token-equal "lwz" lwz?)
                                 (token-equal "arg_z" reg?)
                                 (consp class)
                                 (token-equal "fn" fn?))
                        (push-class-name class))))))
              ;; save the possible argument form in passing
              (setf arg-form (pop disassembly)))
        (dolist (form disassembly)
          (destructuring-bind (o2 &optional lwz? reg name fn? &rest r2) form
                (declare (ignore o2 reg r2))
                ;; (print form)
                (when (and (token-equal "lwz" lwz?)
                           (token-equal "fn" fn?))
                  ;; if it's a function call, save the function name
                  (setf name (push-function-name name))          ; cache the actual name
                  (when (eql name 'ccl::%make-instance)
                    ;; if the call is to make-instance with a constant class, save that
                    (destructuring-bind (&optional o0 lwz? reg? class fn?) arg-form
                      (when (and (numberp o0)
                                 (token-equal "lwz" lwz?)
                                 (token-equal "arg_z" reg?)
                                 (consp class)
                                 (token-equal "fn" fn?))
                        (push-class-name class)))))
                ;; save the possible argument form in passing
                (setf arg-form form))))
      calls))

  #+(and clozure x86-target)
  (:method ((function function))
    (let ((disassembly (ccl::disassemble-list function))
          (calls nil))
      (handler-case (loop for (nil op . args) in disassembly
                          when (and (equal op "movl") (consp args) (consp (first args)))
                          do (destructuring-bind ((@ &optional name tag) . rest) args
                               (declare (ignore rest))
                               (when (and (eq @ '@) (consp name) (equal '(ccl::% "fn") tag))
                                 (destructuring-bind (quote name) name
                                   (declare (ignore quote))
                                   (typecase name
                                     (symbol (pushnew name calls))
                                     (ccl::class-cell (pushnew (ccl::class-cell-name name) calls)))))))
        (error (c) (warn "Call trace error: ~a~%~:W." c disassembly)))
      calls))

  #+sbcl
  (:method ((function function))
    (remove-duplicates (mapcar #'sb-impl::fun-name (sb-introspect:find-function-callees function))))

  #-allegro
  (:method ((x cons))
    (when (and (eq (first x) 'setf) (fboundp x))
      (dsw:function-calls (fdefinition x))))

  #-allegro
  (:method ((method method))
    (function-calls (c2mop:method-function method)))
  
  (:method ((function generic-function))
    (reduce #'union (c2mop:generic-function-methods function)
            :key #'dsw:function-calls
            :initial-value nil))

  #-allegro
  (:method ((function symbol))
    (when (fboundp function)
      (let ((definition (fdefinition function)))
        (typecase definition
          (function (function-calls definition))
          (vector (reduce #'union definition
                          :key #'(lambda (x) (when (functionp x) (dsw:function-calls x)))
                          :initial-value nil))
          (t (warn "unexpected function definition: ~s, ~s." function definition))))))

  #+allegro
  (:method ((function function))
    (xref:get-relation :direct-calls (dsw:function-name function) :wild))

  #+allegro
  (:method ((function cons))
    (when (and (eq (first function) 'setf) (fboundp function))
      (xref:get-relation :direct-calls function :wild)))

  #+allegro
  (:method ((method method))
    (flet ((designator (specializer)
             (typecase specializer
               (class (class-name specializer))
               (t `(eql ,(mop:eql-specializer-object specializer))))))
      (xref:get-relation :direct-calls
                        `(method ,(aclmop:generic-function-name (c2mop:method-generic-function method))
                           ,(mapcar #'specializer-designator (c2mop:method-specializers method)))
                        :wild)))

  #+allegro
  (:method ((function generic-function))
    (xref:get-relation :direct-calls (aclmop:generic-function-name function) :wild))

  #+allegro
  (:method ((function symbol))
    (xref:get-relation :direct-calls function :wild))
)

;(function-calls 'print) (function-calls 'function-calls)

(defun dsw:function-package (datum)
  (typecase datum
    (symbol (symbol-package datum))
    (cons (dsw:function-package (second datum)))
    (function (dsw:function-package (dsw:function-name datum)))
    (method (dsw:function-package (dsw:function-name datum)))))

(defgeneric dsw:function-lambda-list (function)
  (:method ((function generic-function))
    (generic-function-lambda-list function))
  (:method ((function function))
    (second (function-lambda-expression function)))
  (:method ((method method))
    (c2mop:generic-function-lambda-list (c2mop:method-generic-function method)))
  (:method ((form cons))
    (ecase (first form)
      (lambda (second form))
      ((defun defgeneric defmethod) (third form)))))


#+ccl
(defgeneric dsw:object-source-information (subject type-specification)
  (:documentation 
   "Given a symbol return the source information for the given definition type.
 The keys can be objects or type names. The values should be pathnames.
 ccl implements it in terms of a collection bound to the symbol.
 sbcl bind the individual location to the respective instance.")

  (:method ((symbol symbol) (type-specification t))
    (let ((information (ccl::get-source-files-with-types&classes symbol)))
      (rest (assoc-if (etypecase type-specification
                        (symbol #'(lambda (key) (or (eq key type-specification)
                                                    (typep key type-specification))))
                        (cons #'(lambda (key) (member key type-specification))))
                      information))))

  (:method ((object class) (type-specification (eql 'class)))
    (rest (assoc 'class (ccl::get-source-files-with-types&classes (class-name object)))))


  (:method ((object generic-function) (type-specification (eql 'function)))
    (rest (assoc 'function (ccl::get-source-files-with-types&classes (generic-function-name object)))))

  (:method ((object method) (type-specification (eql 'method)))
    (rest (assoc object (ccl::get-source-files-with-types&classes
                         (generic-function-name (c2mop:method-generic-function object))))))

  (:method ((object function) (type-specification (eql 'function)))
    (rest (assoc 'function (ccl::get-source-files-with-types&classes (ccl:function-name object)))))

  (:method ((object package) (type-specification (eql 'package)))
    (rest (assoc 'package
                 (ccl::get-source-files-with-types&classes (intern (package-name object) :cl-user))))))


#+sbcl
(defgeneric dsw:object-source-information (subject type-specification)
  (:documentation 
   "Given a symbol return the source information for the given definition type.
 The keys can be objects or type names. The values should be pathnames.
 ccl implements it in terms of a collection bound to the symbol.
 sbcl bind the individual location to the respective instance.")

  (:method ((name symbol) (type-specification (eql 'class)))
    (dsw:object-source-information (find-class name) type-specification))

  (:method ((name symbol) (type-specification (eql 'function)))
    (dsw:object-source-information (fdefinition name) type-specification))

  (:method ((name symbol) (type-specification (eql 'method-combination)))
    ;; if this fails, then it's too late
    (dsw:object-source-information (find-method-combination nil name nil) type-specification))

  (:method ((method method) (type-specification (eql 'method)))
    (error "must define process for methods."))

  (:method ((function function) (type-specification (eql 'function)))
    ;;; this is what describe does
    (let* ((info (sb-kernel:%code-debug-info (sb-kernel:fun-code-header (sb-kernel:%fun-fun function))))
           (source (when info (sb-c::debug-info-source info))))
      (when source (sb-c::debug-source-namestring source))))

  ;; this comprises class, generic-function, and (if they get here) method and method-combination
  (:method ((object sb-pcl::definition-source-mixin) (type-specification t))
    (sb-pcl::definition-source object)))

#+allegro
(defgeneric dsw:object-source-information (object type-specification)
  (:documentation 
   "Given a symbol return the source information for the given definition type.
 The keys can be objects or type names. The values should be pathnames.
 ccl implements it in terms of a collection bound to the symbol.
 sbcl bind the individual location to the respective instance.")

  (:method ((object symbol) (type-specification t))
    (case type-specification
      (function (excl:source-file object :function))
      (class nil)
      (package nil)))

  (:method ((object class) (type-specification (eql 'class)))
    ;; as of 8.0 this is null
    (excl:source-file object))

  (:method ((object generic-function) (type-specification (eql 'function)))
    (excl:source-file object))

  (:method ((object method) (type-specification (eql 'method)))
    (excl:source-file object))

  (:method ((object function) (type-specification (eql 'function)))
    (excl:source-file object))

  (:method ((object package) (type-specification (eql 'package)))
    (excl:source-file object)))

    
(assert (dsw:object-source-information 'de.setf.utility:bound-slot-value 'function)
        ()
        "No source information.")


(defgeneric dsw:object-designator (object)
  (:documentation "Given a distinguishable object, return a stable designator. This includes the type and
 some identifier which can be used context-free to locate the object.")

  (:method ((object class))
    `(class ,(class-name object)))

  (:method ((object function))
    `(function ,(dsw:function-name object)))

  (:method ((object method))
    `(method ,(dsw:function-name object)
       ,@(method-qualifiers object)
       ,(c2mop:method-specializers object)))

  (:method ((object package))
    `(package ,(package-name object)))

  (:method ((object t))
    object))

:de.setf.utility.walker

