;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (intersection '(:allegro :clozure :digitool :lispworks :sbcl) *features*)
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))

;;;  This file part of the 'de.setf.utility' Common Lisp library.
;;;  It defines cloning functions for clos objects

;;;  Copyright 2003,2006,2009,2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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
;;;
;;; instance copy/clone implementation
;;;
;;;  clone-instance
;;;  clone-instance-as
;;;  initialize-clone
;;;  copy-instance-slots
;;;  clone-array
;;;
;;; Author:   janderson
;;; Copyright 2004, 2005 RavenPack International
;;; 2004-09-01
;;; 2004-11-10  janderson  dynamic-extent declaration in initialize-clone
;;; 2004-11-16  janderson  both single symbol and specialized parameter list
;;;   for def-initialize-clone
;;; 2005-01-06  janderson  added check on clone initarg correctness
;;; 2005-03-20  janderson  incorporated clone-array for fft computations
;;; 2005-05-17  janderson  migrated back to library
;;; 2006-02-06  janderson check-clone-slots returns missing list and recognizes
;;;   non-keyword parameter keys
;;; 2006-05-17  janderson  base method for simple-condition
;;; 2006-06-05  janderson  changed shared-initialize slot argument to t for
;;;   cloning
;;; 2006-09-27  janderson  clone-instance for symbols


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(modpackage :de.setf.utility
  (:export
   :check-clone-slots
   :clone-array
   :clone-instance
   :clone-instance-as
   :clone-p
   :copy-instance-slots
   :def-copy-instance-slots
   :def-initialize-clone
   :initialize-clone
   ))

;;;
;;;

(defgeneric clone-instance (instance &rest initargs)
  (:documentation 
      "reproduce a given instance.")

  (:method ((instance standard-object) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'clone-instance-as instance (class-of instance) initargs))
  (:method ((instance t) &key &allow-other-keys)
    (error "cannot clone datum: ~s." instance))
  (:method ((instance string) &key) instance)
  (:method ((instance symbol) &key) instance)
  (:method ((instance number) &key) instance)
  (:method ((nodes list) &rest initargs)
    (mapcar #'(lambda (x) (apply #'clone-instance x initargs)) nodes)))

(defgeneric clone-instance-as (instance class &rest initargs)
  (:method ((instance standard-object) (class symbol) &rest initargs)
           (apply #'clone-instance-as instance (find-class class) initargs))
  (:method ((instance standard-object) (class class)
                                       &rest initargs &aux new)
           "observing that both mcl and allegro support allocate-instance
            on all of {built-in,funcallable-standard,standard,structure}class
            the specialization for this function can be relaxed accordingly."
           (declare (dynamic-extent initargs))
           (setf new (allocate-instance class))
           ;; pass any initargs through and augment them on the
           ;; way to the ultimate shared-initialize call
           (apply #'initialize-clone instance new initargs)
           new))

(defgeneric initialize-clone (old new &rest initargs &key &allow-other-keys)
  (:documentation
      "invoke shared-initialize with initargs to initialize slots
       prior to copying from the instance to the clonein order to override the existing
       slot values and preclude unwanted deep cloning.")

  (:method ((old standard-object) (new standard-object) &rest initargs)
    (declare (dynamic-extent initargs))
    (apply #'shared-initialize new t initargs))         ;; must pass t to get default initform protocol

  (:method ((from simple-condition) (to simple-condition) &rest
            de.setf.utility.implementation::initargs &key
            (format-control (or (ignore-errors (simple-condition-format-control from)) ""))
            (format-arguments (ignore-errors (simple-condition-format-arguments from))))
    (apply #'call-next-method from to
           :format-control format-control
           :format-arguments format-arguments
           initargs)))


(defgeneric clone-p (object1 object2)
  (:documentation
   "return t if the arguments are clones.
    the base method tests for type identity only.")
  (:method ((d1 t) (d2 t))
           (eq (type-of d1) (type-of d2)))
  (:method ((d1 cons) (d2 cons))
           (and (clone-p (first d1) (first d2))
                (clone-p (rest d1) (rest d2))))
  (:method ((d1 string) (d2 string))
           (equal d1 d2))
  (:method ((d1 vector) (d2 vector))
           (and (= (length d1) (length d2))
                (every #'clone-p d1 d2))))

(defgeneric copy-instance-slots (from to)
  (:documentation
   "copy slot values from one node to another.
    slots with bound values are left unchanged in order to permit previous initarg
    based initialization (see clone-instance), which can then avoid unwanted deep
    cloning.")
  (:method-combination progn :most-specific-first))
    

(defmacro defcopy-instance-slots ((class &rest slot-names) &rest body)
  `(defmethod copy-instance-slots progn ((from ,class) (to ,class))
     ,@(mapcar #'(lambda (slot-name)
                   `(unless (slot-boundp to ',slot-name)
                      (setf (slot-value to ',slot-name)
                        (slot-value from ',slot-name))))
               slot-names)
     ,@body
     to))

(defgeneric check-clone-slots (class slots)
  (:documentation "check the given slot names against the class' defined slots to verify
    that some slot recognizes a name which will be coerced to a keyword.")

  (:method ((designator symbol) (slots list))
           (check-clone-slots (find-class designator) slots))

  (:method ((class class) (name-specs list))
           (finalize-if-needed class)
           (let* ((slot-definitions (class-slots class))
                  (missing (remove-if
                            #'(lambda (name-spec)
                                (etypecase name-spec
                                  ;; iff the keyword was specified explicitly, believe it
                                  ;; otherwise require its presence in some slot's definition
                                  (cons t)
                                  (symbol (let ((key (intern (string name-spec) :keyword)))
                                            (find-if #'(lambda (def)
                                                         (find key (slot-definition-initargs def)))
                                                     slot-definitions)))))
                            name-specs)))
             (values (null missing) missing))))


(defmacro def-initialize-clone (signature slot-specifications &rest body)
  "construct an initialization method for the respective class.
   if an argument is present in the initarg list, then that
   value takes precedence. otherwise use the respective slot
   value from the old instance.
   signature may be either a symbol - wihcih designates the class of
   both the original and the clone, or a specialized parameter list
   for the original and the clone.
   nb. must check slots at evel-time as at compile-time the class may not yet exist."
  (etypecase signature
    (symbol (setf signature `((,(gensym "FROM-") ,signature)
                              (,(gensym "TO-") ,signature))))
    (cons ))
  (let ((slot-names (mapcar #'(lambda (spec)
                                (if (consp spec) (first spec) spec))
                            slot-specifications))
        (slot-forms (mapcar #'(lambda (spec)
                                (if (consp spec) (second spec) nil))
                            slot-specifications))
        (slot-rests (mapcar #'(lambda (spec)
                                (if (consp spec) (cddr spec) nil))
                            slot-specifications)))
    (destructuring-bind ((from from-class) (to to-class)) signature
      `(progn
         (multiple-value-bind (ok missing) (check-clone-slots ',to-class ',slot-names)
           (unless ok
             (cerror "define operator anyway."
                     "clone target class does not recognize initargs for slots: ~s: ~s."
                     ',to-class missing)))
         (defmethod initialize-clone
             ((,from ,from-class) (,to ,to-class)
              &rest initargs
              &key ,@(mapcar #'(lambda (name-spec form rest)
                                 (list* name-spec
                                        (or form
                                            `(_slot-value ,from 
                                                          ;; allow alternative keyword
                                                          ',(etypecase name-spec
                                                              (symbol name-spec)
                                                              (cons (second name-spec)))))
                                         rest))
                             slot-names
                             slot-forms
                             slot-rests))
           ,@body
           (apply #'call-next-method ,from ,to
                  ,@(reduce #'append
                            (mapcar #'(lambda (name-spec)
                                        (etypecase name-spec
                                          (symbol
                                           (list (intern (string name-spec) :keyword)
                                                 name-spec))
                                          (cons
                                           (destructuring-bind (key variable) name-spec
                                             `(',key ,variable)))))
                                    slot-names))
                  initargs))))))


;;;
;;; array cloning

(defun clone-array (array &key (undisplace-array nil)
                               (initial-element nil initial-element-p))
  "Shallow copies the contents of any array into another array with
equivalent properties.  If array is displaced, then this function
will normally create another displaced array with similar properties,
unless UNDISPLACE-ARRAY is non-NIL, in which case the contents of the
array will be copied into a completely new, not displaced, array.
the general case is an array, but for fourier purposes, it is applied to
vectors"
  (check-type array array)
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement array)
    (let ((dimensions (array-dimensions array))
          (element-type (array-element-type array))
          (adjustable (adjustable-array-p array))
          (fill-pointer (when (array-has-fill-pointer-p array)
                          (fill-pointer array))))
      (when undisplace-array
        (setf displaced-to nil))
      (when (and initial-element-p displaced-to)
        (error "initial-element supplied to copy a displaced array."))
      (let ((new-array
             (apply #'make-array
                    dimensions
                    :element-type element-type
                    :adjustable adjustable
                    :fill-pointer fill-pointer
                    :displaced-to displaced-to
                    (when displaced-to
                      (list :displaced-index-offset
                            displaced-index-offset)))))
        (cond (displaced-to)
              (initial-element-p
               (dotimes (i (array-total-size array))
                 (setf (row-major-aref new-array i) initial-element)))
              (t
               (dotimes (i (array-total-size array))
                 (setf (row-major-aref new-array i)
                   (row-major-aref array i)))))
        new-array))))

:de.setf.utility

