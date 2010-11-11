;;; -*- Package: de.setf.utility.implementation; -*-

;;;  This file part of the 'de.setf.utility' Common Lisp library.
;;;  It defines utility operators and classes for work with conditions
;;;

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

;;;  content:
;;;    adds operators to the ANSI condition system for reporting and initializing
;;;    instantiated conditions
;;;
;;;  initialize-condition
;;;  make-initialized-condition
;;;  report-condition

;;;  20030602 ja : allow that scl does not implement method combinations
;;;  20030816 ja : allow that clisp does not implement method combinations
;;;

(in-package :de.setf.utility.implementation)

(modpackage :de.setf.utility
  (:export
   :assert-argument-type
   :assert-argument-types
   :assert-condition
   :continuable-error
   :report-condition
   :initialize-condition
   :make-initialized-condition
   :simple-continuable-condition
   :continue-format-control
   :continue-format-arguments
   :condition-continue-format-control
   :condition-continue-format-arguments
   :condition-report
   :condition-format-control
   :condition-format-arguments
   )
  (:documentation
   "the package :setf.conditions "))


(defgeneric condition-format-control (condition)
  (:method ((condition t))
    "")
  (:method ((condition simple-condition))
    (simple-condition-format-control condition)))

(defgeneric condition-format-arguments (condition)
  (:method ((condition t))
    ())
  (:method ((condition simple-condition))
    (simple-condition-format-arguments condition)))

;;

(define-condition continuable-error (error)
  ((continue-format-control :initform "attempt to continue."
                           :initarg :continue-format-control
                           :reader condition-continue-format-control)
   (continue-format-arguments :initform nil
                              :initarg :continue-format-arguments
                              :reader condition-continue-format-arguments)))

#-(or scl clisp) ; scl and clisp do not implement method combinations
(define-method-combination condition-report (&key (fresh-line-p t)) 
                           ((prefix (:prefix))
                            (primary () :required t)
                            (suffix (:suffix)))
  (:arguments condition stream)
  "combine primary methods as a progn with intervening calls to fresh-line. :prefix and :suffix methods are combined with allowance for call-next method, with order most-specific-first and least-specific-first, respectively."

  (let ((form (list* 'progn
                     condition     ; pacify compilers
                     (reduce #'nconc
                             (mapcar #'(lambda (method)
                                         (if fresh-line-p
                                           (list `(fresh-line ,stream)
                                                 `(call-method ,method ()))
                                           `((call-method ,method ()))))
                                     primary)))))
    (when prefix
      (setf form `(progn (call-method ,(first prefix) ,(rest prefix))
                         ,form)))
    (when suffix
      (setf suffix (nreverse suffix))
      (setf form
            `(multiple-value-prog1 ,form
               (call-method ,(first suffix) ,(rest suffix)))))
    form))
                           
(defGeneric report-condition (condition stream)
  (:documentation
   "the function report-condition accepts two arguments, a condition and an output stream.
 it is intended to be specified as the :report option to define-condition where it
 provides an specializable protocol for formatting condition descriptions.
 Its method combination condition-report, for which the primary are combined as a progn
 with intervening calls to fresh-line. each individual method should format those
 properties specific to the respective condition type only. the base method for
 simple-condition applies the function condition-format-control to the condition,
 and if the value is not null, it applies format to the control value and the result of
 applying condition-format-arguments to the condition.")
  #-(or scl clisp) (:method-combination condition-report)
  (:method #-(or scl clisp) :prefix #+(or scl clisp) :before ((condition condition) stream)
           (format stream "~a signaled:" (type-of condition)))
  (:method ((condition condition) stream)
           (let ((control (condition-format-control condition)))
             (when control
               (apply #'format stream control
                      (condition-format-arguments condition))))
           condition))


;;;
;;; conditions classes are _not_ guaranteed to be subclasses of standard-object.
;;; which means that make-condition need not use make-instance. whichmeans that
;;; neither iniialize-instance nor shared-initialize must be available.

(defGeneric initialize-condition (condition &rest args)
  (:documentation
   "the generic function initialize-condition is called by make-initialized-condition
 to initialize newly made conditions. the function initialize-condition is called with
 one argument, the new condition. note that condition instances themselves are not
 portably mutable; the initialization protocol is intended to afford an opportunity
 to augment delegated mutable state in connection with the condition.")
  (:method ((condition condition) &key &allow-other-keys)
           condition))

(defGeneric make-initialized-condition (condition-designator &rest args)
  (:documentation
   "the generic function make-initialized-condition creates and initializes a
 new condition. it accepts as arguments a condition designator and a list of
 alternating initialization argument names and values. the standard methods
 first resolve the designator to a condition class, delegate the instantiation
 to the function make-condition, apply initialize-condition to the new
 condition, and finally return it.")
  (:method ((designator class) &rest args)
           (apply #'make-initialized-condition (class-name designator) args))
  (:method ((designator condition) &rest args)
           "given a condition instance, treat it as a prototype by delegating to its type."
           ; use the class name rather than the class, in case make-condition is not conform.
           (apply #'make-initialized-condition (type-of designator) args))
  (:method ((designator symbol) &rest args &aux condition)
           (declare (dynamic-extent args))
           (setf condition (apply #'make-condition designator args))
           (initialize-condition condition)
           condition))

#+(and digitool)
(setf (ccl:assq 'define-condition ccl:*fred-special-indent-alist*) 2)

(defmacro assert-condition (form &rest args)
  (let ((format-control nil) (format-arguments nil) (operator nil))
    (when (or (typep (first args) '(and symbol (not keyword)))
              (and (consp (first args)) (eq (caar args) 'setf)))
      (setf operator (pop args)))
    ;; if there control is first, assume (control . args)
    (when (stringp (first args))
      (setf format-control (pop args)
            format-arguments (shiftf args nil)))
    (destructuring-bind (&key (operator operator)
                              (format-string format-control) (format-control format-string)
                              (format-arguments format-arguments)
                              (type (if (and (consp form) (eq (first form) 'typep)) (third form) `(satisfies ,form))))
                        args
      `(unless ,form
         (error 'simple-type-error
                :expected-type (quote ,type)
                :format-control ,(format nil "~@[~a: ~]condition failed: ~s~:[.~; ~~@?~]"
                                         operator form
                                         ;; if a control is present include the recursive format
                                         format-control)
                :format-arguments ,(when format-control `(list ,format-control ,@format-arguments)))))))


(defmacro assert-argument-type (operator variable type &optional (required-p t) (test `(typep ,variable ',type)))
  (let ((form `(unless ,test
                 (error 'simple-type-error :datum ,variable :expected-type ',type
                        :format-control ,(format nil "~s: the ~:[(optional) ~;~] ~a argument (~~s) must be of type ~s."
                                                 operator required-p variable type)
                        :format-arguments (list ,variable)))))
    (if required-p
      form
      `(when ,variable ,form))))


(defmacro assert-argument-types (operator &rest assertions)
  `(progn ,@(loop for assertion in assertions
                  collect `(assert-argument-type ,operator ,@assertion))))

#+mcl
(setf (ccl:assq 'assert-argument-types ccl:*fred-special-indent-alist*) 1)


#|
(define-condition test-condition (simple-error)
                  ((prop1 :initarg :prop1 :reader test-condition-prop1))
  (:report report-condition)
  (:default-initargs :format-control "static error message for test condition"))

(define-condition test-condition+ (test-condition)
                  ((prop2 :initarg :prop2 :reader test-condition-prop2))
  (:default-initargs :format-control "static error message for test condition +"))

(defMethod report-condition ((condition test-condition) stream)
  (format stream "test condition aspects: ~a." (test-condition-prop1 condition)))

(defMethod report-condition :prefix ((condition test-condition) stream)
  (format stream "(test prefix) ")
  (call-next-method))

(defMethod report-condition ((condition test-condition+) stream)
  (format stream "test condition + aspects: ~a." (test-condition-prop2 condition)))

(defMethod report-condition :prefix ((condition test-condition+) stream)
  (format stream "(test+ prefix)"))

(error 'test-condition+ :prop1 1 :prop2 2)
(error (make-initialized-condition 'test-condition :prop1 1))
(typep (make-condition 'test-condition) (find-class 'test-condition))
(typep (make-condition 'test-condition) 'test-condition)
(typep (make-condition (find-class 'test-condition)) 'test-condition)

|#

:de.setf.utility

