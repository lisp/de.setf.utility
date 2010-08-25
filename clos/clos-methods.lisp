;;; -*- Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;  This file part of the 'de.setf.utility' Common Lisp library.
;;;  It defines utility functions for clos

;;;  Copyright 2003-2005, 2009, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; 20030902 incorporated the class definition macros from cl-xml
;;; 20031101 denominated combination requires qualifiers declarations and where that is a function
;;;          invokes it with the specializers, not the prototypes.
;;; 20040115.jaa eliminated clone code from clos.lisp
;;; 20040131.jaa differentiated read and write function designator for defDelegateMethod
;;; 20040226.jaa renamed locked-generic-function lock slot to function-lock;
;;;          added class-slot-names
;;; 20041027.janderson added short-standard
;;; 20040503.janderson exports for lispworks :clos package
;;; 20050628.janderson ccl imports
;;; 20050701.janderson compute-effective-initargs
;;; 20090405.janderson  factored out the package declaration; change from
;;;  def* to def-* macro form; eliminated separate delegate reader macro
;;; 20091005.janderson  added class-direct-object-slots and class-direct-builtin-slots
;;;  to support uml diagram generation
;;; 20100212.janderson renamed classes and combination to reduce export conflicts:
;;;  concrete -> concrete-standard
;;;  abstract-* -> abstract-standard-*
;;; 20100315.janderson  consolidaed denominated combination with denominated-progn in their own file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; contents
;;;
;;; macros
;;;  required-initarg-error
;;;  with-special-readers
;;;  with-special-slots
;;;
;;; classes
;;;  abstract-standard-class
;;;  abstract-standard-generic-function
;;;  abstract-standard-method
;;;  locked-generic-function
;;;
;;; method combinations
;;;  concrete-standard
;;;  locked-standard
;;;  named-locked-standard
;;;  short-standard
;;;
;;; mop extension operators
;;;  compute-effective-initargs
;;;  encode-instance-as
;;;  finaize-if-needed
;;;  specializer-prototype
;;;  class-direct-built-in-slots
;;;  class-direct-object-slots
;;;
;;; conditions
;;;  abstract-method-error

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (intersection '(:allegro :clozure :digitool :lispworks :sbcl) *features*)
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An encoding framework

(defgeneric encode-instance-as (instance stream as &rest arguments)
  (declare (dynamic-extent arguments))
  (:documentation
   "collects a set of functions which encode instances for specific purposes.")

  (:method ((Instance t) (destination pathname) (as t) &rest arguments)
    (ensure-directories-exist destination)
    (with-open-file (stream destination :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (apply #'encode-instance-as instance stream as arguments))))
    
;;
;; macros

(defMacro with-special-readers (slots instance &rest body &aux (instance-sym (gensym)))
  "the macro with-special-readers creates a dynamic context in which the values available through specified accessors are available through a special binding. the bindings are read-only."
  `(let ((,instance-sym ,instance))
     (let ,(mapcar #'(lambda (slot-entry) (if (consp slot-entry)
                                             `(,(first slot-entry) (,(second slot-entry) ,instance-sym))
                                             `(,slot-entry (,slot-entry ,instance-sym))))
                   slots)
       (declare (special ,@(mapcar #'(lambda (slot-entry) (if (consp slot-entry) (first slot-entry) slot-entry))
                                   slots)))
       ,@body)))

(defMacro with-special-slots (slots instance &rest body &aux (instance-sym (gensym)))
  "the macro with-special-slots creates a dynamic context in which the values of specified slots are available through a special binding. the bindings are read-only."
  `(let ((,instance-sym ,instance))
     (let ,(mapcar #'(lambda (slot-entry) (if (consp slot-entry)
                                             `(,(first slot-entry) (slot-value ,instance-sym ',(second slot-entry)))
                                             `(,slot-entry (slot-value ,instance-sym ',slot-entry))))
                   slots)
       (declare (special ,@(mapcar #'(lambda (slot-entry) (if (consp slot-entry) (first slot-entry) slot-entry))
                                   slots)))
       ,@body)))

(defun required-initarg-error (name &optional control &rest args)
  (error "no value for initarg ~s is present.~@[ ~?~]" name control args))

(defmacro required-initarg (name)
  `(required-initarg-error ',name))

;;
;; accessors

#+(and digitool (not ccl-5.2))          ; no longer necessary
(defgeneric generic-function-name (function)
  (:method ((function generic-function))
           (function-name function)))


;; eventually reduce to a single function
#+digitool
(defun class-slot-names (class)
  (mapcar #'slot-definition-name
          (append (ccl::class-instance-slots class)
                  (ccl::class-class-slots class))))

#+(or allegro openmcl lispworks sbcl)
(defun class-slot-names (class)
  (mapcar #'slot-definition-name
          (class-slots class)))


(defgeneric finalize-if-needed (class)
  (:documentation
   "finalize a class if necessary. return the class meta-object.
    mad a class designator to the class.")
  (:method ((designator symbol))
           (finalize-if-needed (find-class designator)))
  (:method ((class class))
           (unless (class-finalized-p class)
             (finalize-inheritance class))
           class))


;;;
;;; class metadata

(defgeneric compute-effective-initargs (class initargs)
  (:documentation
   "given a class and instantial initargs, compute the effective initargs
    by integrating the class's default initargs.")
  (:method ((class-designator symbol) initargs)
           (compute-effective-initargs (find-class class-designator) initargs))
  (:method ((class standard-class) initargs)
           ;; allow the provided to be a constant.
           ;; expect the value to be the result of a future which is
           ;; the last element in the respetive metadata.
           (let ((defaulted-initargs initargs))
             (finalize-if-needed class)         ; otherwise sbcl may have direct defaults only
             (dolist (initarg-default (class-default-initargs class))
               (let ((key (first initarg-default))
                     (future #+allegro (second initarg-default)
                             #+(or clozure digitool) (third initarg-default)
                             #+lispworks (progn (warn "uncertain initarg default.") (second initarg-default))
                             #+sbcl (third initarg-default)))
                 (unless (getf defaulted-initargs key)
                   (when (eq defaulted-initargs initargs)
                     (setf defaulted-initargs (copy-list initargs)))
                   (setf (getf defaulted-initargs key)
                     ;; allow either a function or an immediate object
                     (typecase future
                       (function (funcall future))
                       (t future))))))
             defaulted-initargs)))

;; see clone.lisp for clone code

;;
;;
;; abstract classes, functions, methods:
;; this comprises the class, macros to define abstract classes, abstract
;; functions, and abstract methods, a cndition to signal when an abstract method
;; is somehow invoked, and a method combination which signals an error if
;; no concrete method are present in an effective method.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass abstract-standard-class (standard-class) ()
    (:documentation
"an abstract class stipulates the implementation of various functions.
it proscribes direct instantiation. a specialization must be defined, for which
the respective functions are implemented. the macro `defabstractclass`
defines an abstract class. the macros `defabstractgeneric` defines a
generic function which employes the `concrete` method combination,
through which proscribes effective methods comprising exclusively abstract
methods, which it removes from the effective method."))
  (defMethod validate-superclass
             ((class abstract-standard-class) (superclass standard-class))
    t)
  (defMethod validate-superclass
             ((class standard-class) (superclass abstract-standard-class))
    t)
  (defMethod make-instance ((class abstract-standard-class) &key)
    (error "instantiation precluded for class: ~s." class)))

#|(defmacro def-abstract-class (name supers slots &rest options &aux (metaclass (second (assoc :metaclass options))))
  "define an abstract class.
   if no meta-class is provided the class abstract-standard-class is incorporated in the definition.
   if a meta-class is provided, it must exist and must specialize the class abstract-standard-class."
  (let ((accessors (assoc :accessors options))
        (functions (assoc :functions options))
        (defclass nil))
    (when accessors (setf options (remove accessors options)))
    (when functions (setf options (remove functions options)))
    (case metaclass
      ((nil) (push '(:metaclass abstract-standard-class) options))
      (abstract-standard-class )
      (t (unless (subtypep metaclass 'abstract-standard-class)
           (error "metaclass not an abstract class: ~s." metaclass))))
    (setf defclass `(defclass ,name ,supers ,slots ,@options))
    (if (or functions accessors)
      `(prog1 ,defclass
         #| don't define the generic functions
           ,@(mapcar #'(lambda (function)
                       `(defAbstractGeneric ,function (instance)))
                   (rest functions))|#
         ,@(mapcar #'(lambda (accessor)
                      `(progn (defAbstractMethod ,accessor ((instance ,name)))
                              (defAbstractMethod (setf ,accessor) ((datum t) (instance ,name)))))
                  (append (rest functions)(rest accessors))))
      defclass)))|#

(defmacro def-abstract-class (name supers slots &rest options &aux (metaclass (second (assoc :metaclass options))))
  "define an abstract class.
   if no meta-class is provided the class abstract-standard-class is incorporated in the definition.
   if a meta-class is provided, it must exist and must specialize the class abstract-standard-class."
  (case metaclass
    ((nil) (push '(:metaclass abstract-standard-class) options))
    (abstract-standard-class )
    (t (unless (subtypep metaclass 'abstract-standard-class)
         (error "metaclass not an abstract class: ~s." metaclass))))
  `(defclass ,name ,supers ,slots ,@options))

#+source-analysis
(defSourceForm (def-abstract-class :class class-source :category class))


(define-condition abstract-method-error (error)
  ((function :initarg :function :initform nil)
   (arguments :initarg :arguments :initform nil))
  (:documentation
   "an abstract-method-error condition is signaled when an abstract method is invoked.
    an abstract method may be defined for an abstract-standard-generic-function only, as, in that case, the
    method combination eliminates abstract methods from the effective method and ensure that concrete
    methods are present. where an abstract method is erroneously idefined for a standard-generic-function
    this error may result.")
  (:report (lambda (condition stream)
             (with-slots (function arguments) condition
               (format stream
                       "abstract method invoked on arguments: ~s: ~s."
                       function arguments)))))

(defun abstract-method-error (generic-function &rest arguments)
  (error 'abstract-method-error :function generic-function
         :arguments arguments))

#|
(defgeneric abstract-method-p (method)
  (:documentation
   "return t if all specializers are subtypes of abstract classes.")
  (:method ((datum t))
           nil)
  (:method ((method standard-method))
           (let ((abstract-specializer-p nil)
                 (built-in-specializer-p nil)
                 (concrete-specializer-p nil))
             (dolist (specializer (method-specializers method))
               (typecase specializer
                 (built-in-class (unless (eq (class-name specializer) t)
                                   (setf built-in-specializer-p t)))
                 (abstract-standard-class (setf abstract-specializer-p t))
                 (class (setf concrete-specializer-p t))))
             (and abstract-specializer-p
                  (not (or concrete-specializer-p built-in-specializer-p))))))
|#

(defclass abstract-standard-method (standard-method) ())

(defgeneric abstract-method-p (method)
  (:documentation
   "return t if all specializers are subtypes of abstract classes.")
  (:method ((datum t)) nil)
  (:method ((method abstract-standard-method)) t))

(define-method-combination concrete-standard ()
                           ((around (:around))
                            (before (:before))
                            (primary () :required t)
                            (after (:after)))
  "combine all qualified methods and all non-abstract primary methods as for standard method combination.
   require at least one non-abstract primary method to remain."
  (let ((all-primary primary)) 
    (labels ((call-methods (methods)
               (mapcar #'(lambda (method)
                           `(call-method ,method ()))
                       methods)))
      (cond ((setf primary (remove-if #'abstract-method-p primary))
             (let ((form (if (or before after (rest primary))
                           `(multiple-value-prog1
                              (progn ,@(call-methods before)
                                     (call-method ,(first primary)
                                                  ,(rest primary)))
                              ,@(call-methods (reverse after)))
                           `(call-method ,(first primary) ()))))
               (if around
                 `(call-method ,(first around)
                               (,@(rest around)
                                (make-method ,form)))
                 form)))
            (t
             (method-combination-error "No concrete primary method present among the applicable methods: ~s."
                                       `(,all-primary
                                         :around ,around
                                         :before ,before
                                         :after ,after)))))))

(defclass abstract-standard-generic-function (standard-generic-function)
  ()
  (:documentation
   "the abstract-standard-generic-function class comprises those generic functions for which
    abstract methods can be defined.")
  (:metaclass funcallable-standard-class))

(defmacro def-abstract-generic (name parameters &rest options)
  "define a generic function which uses the `concrete-standard` method combination."
  (when (assoc :method-combination options)
    (error "method combination option not permitted."))
  `(defgeneric ,name ,parameters
     (:method-combination concrete-standard)
     (:generic-function-class abstract-standard-generic-function)
     ,@options))

#+source-analysis
(defannotation def-abstract-generic (defgeneric) ())

(defparameter *class.abstract-standard-method* 'abstract-standard-method)

(defmacro def-abstract-method (name lambda-list &optional documentation &aux (binding (gensym)))
  "generate a method which signals an abstract-method-error if called"
  (unless (consp lambda-list)
    (error "method qualifiers are not permitted: ~s." lambda-list))
  `(let ((,binding (defmethod ,name ,lambda-list
                             ,(or documentation
                                  (format nil "the abstract method for ~a signals an abstract-method-error."
                                          name))
                             (abstract-method-error #',name
                                                    ,@(mapcar #'(lambda (parameter)
                                                                  (etypecase parameter
                                                                    (cons (first parameter))
                                                                    (symbol parameter)))
                                                              (remove-if #'(lambda (param)
                                                                             (and (symbolp param)
                                                                                  (char= #\& (char (symbol-name param) 0))))
                                                                         lambda-list))))))
     (change-class ,binding *class.abstract-standard-method*)
     ,binding))

#+source-analysis
(defAnnotation def-abstract-method (defmethod) ())

;; 
;;

(defParameter *type-prototype-alist*
  `((t . t) (null . nil) (symbol . symbol) (string . " ") (array . #())
    (function . ,#'identity) (character . #\null) (integer . 0) (package . (find-package :common-lisp-user))
    (fixnum . 1) (number . 1) (float . 1.0)))

(defgeneric specializer-prototype (class)
  #+CCL (:method ((class ccl::std-class)) (class-prototype class))
  (:method ((class standard-class)) (allocate-instance class))
  (:method ((class structure-class)) (allocate-instance class))
  (:method ((class built-in-class)) (specializer-prototype (class-name class)))
  (:method ((type symbol)) (or (rest (assoc type *type-prototype-alist*))
                                   (no-applicable-method #'specializer-prototype type)))
  (:method ((specializer cons)) (second specializer)))
  


;;
;; locked functions

(defclass locked-generic-function (standard-generic-function)
  ((function-lock :reader function-lock))
  (:documentation
   "a locked generic function binds a lock for use with a locked-standard method combination.
    the lock applies to all efffective methods")
  (:metaclass funcallable-standard-class))


(defmethod shared-initialize :after ((instance locked-generic-function) (slots t) &key)
  (with-slots (function-lock) instance
    (unless (slot-boundp instance 'function-lock)
      (setf function-lock
            #+allegro (mp:make-process-lock :name (generic-function-name instance))
            #+(or clozure digitool) (ccl::make-lock (generic-function-name instance))
            #+lispworks (mp:make-lock :name (generic-function-name instance))
            #+sbcl (sb-thread:make-mutex :name (generic-function-name instance))
            ))))


(define-method-combination locked-standard ()
                           ((around (:around))
                            (before (:before))
                            (primary () :required t)
                            (after (:after)))
  (:generic-function function)
  "combine as for standard method combination. but wrap the whole thing in a lock."
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method ()))
                   methods)))
    (let ((form (if (or before after (rest primary))
                  `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                     ,@(call-methods (reverse after)))
                  `(call-method ,(first primary) ()))))
      (when around
        (setf form
              `(call-method ,(first around)
                            (,@(rest around)
                             (make-method ,form)))))
      `(#+(or digitool clozure) with-lock-grabbed
        #+allegro mp:with-process-lock
        #+lispworks mp:with-lock
        #+sbcl sb-thread:with-mutex
        (,(function-lock function))
         ,form))))


(define-method-combination named-locked-standard (&optional lock-designator)
                           ((around (:around))
                            (before (:before))
                            (primary () :required t)
                            (after (:after)))
  (:generic-function function)
  "combine as for standard method combination. but wrap the whole thing in a named lock."
  (unless lock-designator (setf lock-designator (generic-function-name function)))
  (setf lock-designator (or (get lock-designator 'named-locked-standard-lock)
                            (setf (get lock-designator 'named-locked-standard-lock)
                                  #+allegro (mp:make-process-lock :name (string lock-designator))
                                  #+(or clozure digitool) (ccl::make-lock (string lock-designator))
                                  #+lispworks (mp:make-lock :name (string lock-designator))
                                  #+sbcl (sb-thread:make-mutex :name (string lock-designator)))))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method ()))
                   methods)))
    (let ((form (if (or before after (rest primary))
                  `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                     ,@(call-methods (reverse after)))
                  `(call-method ,(first primary) ()))))
      (when around
        (setf form
              `(call-method ,(first around)
                            (,@(rest around)
                             (make-method ,form)))))
      `(#+allegro mp:with-process-lock
        #+(or clozure digitool) ccl:with-lock-grabbed
        #+lispworks mp:with-lock
        #+sbcl sb-thread:with-mutex
        (,lock-designator)
         ,form))))


#|
(defgeneric locked-get-string-from-user (&rest args)
  (:method (&rest args) (apply #'get-string-from-user args)))

(defgeneric locked-get-string-from-user (&rest args)
  (:method (&rest args) (apply #'get-string-from-user args))
  (:generic-function-class locked-generic-function)
  (:method-combination named-locked-standard))

(dotimes (x 5)
  (eval-enqueue `(locked-get-string-from-user ,(format nil "dialog # ~d" x)))).
(locked-get-string-from-user "test")
|#


(define-method-combination short-standard (operator &key verbose)
                           ((around (:around))
                            (before (:before))
                            (after (:after))
                            (primary * :required t))
  "combine as for short method combination. but use :around :before :after"
  (setf primary (remove-if (lambda (method)
                             (let ((qualifiers  (method-qualifiers method)))
                               (unless (and (null (rest qualifiers))
                                            (or (eq (first qualifiers) operator)
                                                (member (first qualifiers)
                                                        '(:before :around :after))))
                                 (invalid-method-error method "method qualifier not ~s."
                                                       operator))
                               (not (eq (first qualifiers) operator))))
                           primary))

  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method ()))
                   methods)))
    (let ((form `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (,operator ,@(call-methods primary)))
                     ,@(call-methods (reverse after)))))
      (when around
        (setf form
              `(call-method ,(first around)
                            (,@(rest around)
                               (make-method ,form)))))
      (when verbose (pprint form))
      form)))
#|
(defgeneric test-short-standard (arg)
  (:method-combination short-standard list)
  (:method :around ((arg t)) (list (list :around t arg)
                                   (call-next-method)))
  (:method list ((arg t)) (list 'list t arg))
  (:method list ((arg number)) (list 'list :number arg))
  (:method :before ((arg t)) (print (list :before t arg)))
  (:method :after ((arg t)) (print (list :after t arg)))
  (:method :after ((arg number)) (print (list :number t arg))))
(test-short-standard 1)
|#

(defgeneric built-in-type-p (type)
  (:method ((type null)) nil)

  (:method ((type symbol))
   (let ((class (find-class type nil)))
     (and class (built-in-type-p class))))

  (:method ((type built-in-class)) t)
  (:method ((type class)) nil)

  (:method ((type cons))
   (or (and (member (first type) '(and or not))
            (every #'built-in-type-p (rest type)))
       (let ((type (first type)))
         (and (symbolp type) (built-in-type-p type))))))

(labels ((slot-built-in-p (definition)
         (let ((type (slot-definition-type definition)))
           (case type
             ((nil t) t)
             (t (built-in-type-p type))))))

  (defgeneric class-direct-built-in-slots (class)
    (:documentation "Given standard-class CLASS, returns a list of those slot definitions typed to be built-in.")
    (:method ((class class)) '())
    (:method ((class standard-class))
      (remove-if-not #'slot-built-in-p (class-direct-slots class))))

  (defgeneric class-direct-object-slots (class)
    (:documentation "Given standard-class CLASS, returns a list of those slot definitions typed to be built-in.")
    (:method ((class class)) '())
    (:method ((class standard-class))
      (remove-if #'slot-built-in-p (class-direct-slots class)))))
 
;; (mapcar #'slot-definition-name (class-direct-built-in-slots (find-class 'ccl:window)))
;; (mapcar #'slot-definition-name (class-direct-object-slots (find-class 'ccl:window)))

(defgeneric class-related-classes (class)
    (:documentation "Return a list of class closure related to the argument class by
 virtue of its slot types. See class-direct-related-classes for the direct set.")

  (:method ((class-designator symbol))
    (class-related-classes (find-class class-designator)))
  (:method ((class class))
    nil)
  (:method ((class standard-class))
    (labels ((flatten-closure (class closure)
             (dolist (related-class (class-direct-related-classes class))
               (unless (find related-class closure)
                 (setf closure (flatten-closure related-class (cons related-class closure)))))
             closure))
      (flatten-closure class nil))))

(labels ((flatten-classes (type)
           ;; return list of class instances
           (etypecase type
             ((eql null) nil)           ; suppress null class
             (symbol (let ((class (find-class type nil)))
                       (when class (list class))))
             (cons (destructuring-bind (op . rest) type
                     (case op
                      ((and or)
                       (mapcan #'flatten-classes rest))
                      (not
                       (flatten-classes (first rest)))
                      ((member eql) nil)
                      (t
                       (let (class classes)
                         (cond ((setf class (find-class op nil))
                                (list class))
                               ((setf classes (mapcan #'flatten-classes rest))
                                classes))))))))))
  (defgeneric class-direct-related-classes (class)
    (:documentation "Return a list of classes related to the argument class by
 virtue of its slot types. See class-related-classes for the closure.")

    (:method ((class-designator symbol))
      (class-direct-related-classes (find-class class-designator)))
    (:method ((class class))
      nil)
    (:method ((class standard-class))
      (remove-duplicates (reduce #'nconc (class-direct-object-slots class)
                                 :key #'(lambda (slot-definition)
                                          (flatten-classes (slot-definition-type slot-definition))))))))

             
;; (class-direct-related-classes 'delegate-stream)
;; (class-related-classes 'delegate-stream) (class-related-classes 'gee:seller)


:de.setf.utility
