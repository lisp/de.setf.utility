;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)


(:documentation "This file defines call monitoring utilities for the test module of the 'de.setf.utility'
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

 (description "test coverage monitoring:

 record test coverage on a per-entry basis and generate reports in various forms.
 - a .dot graph of coverage status, which depicts immediate call dependency and the test dependency;
 - a .html tree, which depicts the coverage status with a source-file-per-page documents.
 - a list of tests to run when a source file changes

 there are three primary operators

 monitor (operator)
 unmonitor (operator)
 find-monitor (operator)
 initialize-monitor (monitor)
 report-monitor (monitor)"))



(defvar *monitor-registry* (make-hash-table :test 'equal))
(defvar *monitor-packages* ())
(defvar *monitor-verbose* nil)
(defvar *monitor* nil)
(defvar *style.no-call* "color: red;")
(defvar *color.no-call* "red")
(defvar *style.called* "color: green;")
(defvar *color.called* "green")
(defvar *style.passed* "color: green;")
(defvar *style.failed* "color: red;")
(defvar *style.unknown* "color: black;")

(defclass monitor ()
  ((object
    :initform (error "OBJECT required.")
    :initarg :object
    :type (or function method)
    :reader monitor-object
    :documentation "The object for which to monitor invocation.")
   (name
    :initform nil
    :documentation "The name of the function associated with the entry - for
     a function, the is the (generic-)function name; for a method that of the
     function. Derived lazily from the monitored object and used to locate the
     definition's source file.")
   (designator
    :initform nil :initarg :designator
    :documentation "The designator for the associated object - for a function
     the function name; for a method a method specifier which includes the
     qualifiers and specializers. Derived lazily from the monitored object and
     provided to advice to attach the monitor hook.")
   (called-p
    :initform nil
    :accessor monitor-called-p
    :documentation "When TRUE, the monitored object has been invoked.
     Manipulated by the monitor advice and reset by reinitialize-instance.")
   (calls-monitors
    :initform nil
    :documentation "A vector of the respective monitors for the vector/function
     objects which this monitor's function calls. Constructed lazily via
     disassemble and reset by shared-initialize.")
   (calls-called-p
    :initform nil
    :documentation "A boolean vector in which a TRUE entry indicates that the respective
     monitored call occurred. 
     Manipulated by the monitor advice and reset by reinitialize-instance.")
   (namestring
    :initform nil
    :documentation "A STRING to be used as a title/label for this monitor in reports.
     Derived lazily from the designator for the monitored object.")
   (pathname
    :initform nil
    :documentation "A PATHNAME which locates the source file for the monitored
     function. Derived lazily from the runtime source data.")
   (tests
    :initform nil
    :accessor monitor-tests
    :documentation "Caches the tests active when a monitored function was invoked.
     Reset by reinitialization-instance."))
  (:documentation "The abstract monitor comprises the naming and tracking information
 for a given function object. One name associates the monitor with its (generic) function,
 while the other serves do designate the specific funcion object (specific to methods)
 and thus the individual monitor a well. The tracking information includes whether the
 monitored function has been invoked, and whether the function it has invoked functions
 for which it contains calls."))

(defclass function-monitor (monitor)
  ()
  (:documentation "Concrete monitors for simple functions."))

(defclass generic-function-monitor (monitor)
  ()
  (:documentation "Concrete monitors for generic functions. These appear
 in other monitors' calls collections."))

(defclass method-monitor (monitor)
  ((method-function :initform nil
                    :accessor monitor-method-function)
   (:advice-function :initform nil
                     :accessor monitor-advice-function))
  (:documentation "Concrete monitors for individual methods."))

(defmethod shared-initialize ((monitor monitor) (slots t) &key)
  (call-next-method)
  (setf (slot-value monitor 'called-p) nil)
  (setf (slot-value monitor 'calls-monitors) nil)
  (setf (slot-value monitor 'calls-called-p) nil)
  (setf (slot-value monitor 'tests) nil))

(defgeneric monitor-name (monitor)
  (:documentation "Lazy computation for the monitor's abstract object's name.
 For a function object, this is the function name. For a method this is the
 respective generic function name.")
  (:method :around ((monitor monitor))
    (or (slot-value monitor 'name)
        (setf (slot-value monitor 'name) (call-next-method))))
  (:method ((monitor monitor))
    (monitor-name (monitor-object monitor)))
  #+ccl
  (:method ((entry generic-function))
    (let ((name (generic-function-name entry)))
      (etypecase name
        (symbol name)
        (cons (ecase (first name)
                (setf name)
                (ccl::advised (second (second name))))))))
  (:method ((entry function))
    (monitor-name (function-name entry)))
  (:method ((name symbol))
    name)
  #+ccl
  (:method ((name cons))
    (ecase (first name)
      (setf name)
      (ccl::advised (second (second name)))))
  #+ccl
  (:method ((entry method))
    (ccl::method-name entry))
  #-ccl
  (:method ((entry method))
    (monitor-name (method-generic-function entry))))


(defgeneric monitor-designator (monitor)
  (:documentation "Lazy computation of the value is used as the registry key and
 as the designator for advise. For a function object, this is the function name.
 For a method this takes the form of a list which combines the :METHOD tag, the
 function name, the qualfiers and the specializr names.")
  (:method :around ((monitor monitor))
    (or (slot-value monitor'designator)
        (setf (slot-value monitor'designator) (call-next-method))))
  (:method ((monitor monitor))
    (monitor-designator (monitor-object monitor)))
  (:method ((entry generic-function))
    (monitor-name entry))
  (:method ((entry function))
    (monitor-name entry))
  (:method ((entry method))
    ;; ? (function-name (method-function ...)) ?
    (flet ((specializer-name (specializer)
             (etypecase specializer
               (eql-specializer `(eql ,(eql-specializer-object specializer)))
               (class (class-name specializer)))))
      `(:method ,(generic-function-name (method-generic-function entry))
         ,@(method-qualifiers entry)
         ,(mapcar #'specializer-name (method-specializers entry))))))

(defgeneric monitor-calls-monitors (monitor)
  (:documentation "Lazy computation of the vector of monitors for the respective
 object's calls.")
  (:method :around ((monitor monitor))
    (or (slot-value monitor 'calls-monitors)
        (setf (slot-value monitor 'calls-monitors) (call-next-method))))
  (:method ((monitor t))
    (remove nil (map 'vector #'monitor (monitor-calls monitor)))))

(defgeneric monitor-calls (monitor)
  (:documentation "Return a list of calls from the monitor's respective
 function. !which must be the original, unencapsulated one!")

  (:method ((object null))
    nil)
  (:method ((object t))
    (monitor-calls (find-monitor object)))

  (:method ((monitor function-monitor))
    (monitor-calls (ccl::find-unencapsulated-definition (monitor-designator monitor))))
  (:method ((monitor generic-function-monitor))
    (monitor-calls (ccl::find-unencapsulated-definition (monitor-designator monitor))))
  (:method ((monitor method-monitor))
    (monitor-calls (ccl::find-unencapsulated-definition (monitor-object monitor))))

  (:method ((entry function))
    (dsw:function-calls entry)))
    

(defmethod monitor-calls-called-p ((monitor monitor))
  (or (slot-value monitor 'calls-called-p)
      (setf (slot-value monitor 'calls-called-p)
            (make-array (length (monitor-calls-monitors monitor)) :initial-element nil))))

(defgeneric monitor-namestring (monitor)
  (:method ((monitor monitor))
    (or (slot-value monitor 'namestring)
        (setf (slot-value monitor 'namestring)
              (format nil "~(~a~)" (monitor-designator monitor))))))

(defun monitor-package (monitor)
  (let ((name (monitor-name monitor)))
    (symbol-package (etypecase name
                      (symbol name)
                      (cons (second name))))))

#+ccl
(defgeneric monitor-pathname (monitor)
  (:method :around ((monitor monitor))
    (or (slot-value monitor 'pathname)
        (setf (slot-value monitor 'pathname) (call-next-method))))
  (:method ((monitor monitor))
    (monitor-pathname (monitor-object monitor)))

  (:method ((object function))
    (let ((name (function-name object)))
      (rest (assoc (if (consp name) 'setf 'function)
                   (ccl:edit-definition-p object)))))

  (:method ((object generic-function))
    (let ((name (generic-function-name object)))
      (rest (assoc (if (consp name) 'setf 'function)
                   (ccl:edit-definition-p object)))))

  (:method ((object method))
    (rest (first (ccl:edit-definition-p object))))

  (:method ((object ccl:method-function))
    (or (rest (first (ccl:edit-definition-p object)))
        (let* ((method (function-name object))
               (generic-function (method-generic-function method)))
          (or (rest (first (ccl:edit-definition-p method)))
              ;; guess
              (if generic-function
                (let ((name (generic-function-name generic-function)))
                  (rest (assoc (if (consp name) 'setf 'ccl::accessor)
                               (ccl:edit-definition-p generic-function))))))))))
              

(defgeneric monitor-called-p (object)
  (:method ((object null))
    nil)
  (:method ((object t))
    (monitor-called-p (find-monitor object)))
  (:method ((designator symbol))
    (and (fboundp designator)
         (monitor-called-p (fdefinition designator))))
  (:method ((designator cons))
    (case (first designator)
      (setf (and (fboundp designator)
                 (monitor-called-p (fdefinition designator))))
      (:method (monitor-called-p (find-monitor designator)))))
  (:method ((object generic-function))
    (some #'monitor-called-p (generic-function-methods object))))

;;;
;;; install / remove monitors

(defun clear-monitors ()
  (maphash #'(lambda (k m)
               (unmonitor (monitor-object m))
               (remhash k *monitor-registry*))
           *monitor-registry*))
;; (clear-monitors)

(defgeneric find-monitor (entry)
  (:method ((entry function))
    (find-monitor (monitor-designator entry)))
  (:method ((entry method))
    (find-monitor (monitor-designator entry)))

  (:method ((designator t))
    (gethash designator *monitor-registry*)))


(defgeneric (setf find-monitor) (monitor entry)
  ;; installing
  #+digitool
  (:method ((monitor monitor) (entry generic-function))
    (let* ((advice-name (gensym "MONITOR"))
           (designator (monitor-designator entry))
           (advice (ccl::advise-global-def designator
                                           advice-name
                                           :around
                                           `(progn (record-call ,monitor)
                                                   (let ((*monitor* ,monitor)) (:do-it)))
                                           nil)))
      (ccl::advise-2 (eval advice) advice-name nil designator :around 'call-monitor nil)
      (setf (find-monitor designator) monitor)))
  #+clozure
  (:method ((monitor monitor) (entry generic-function))
    (let* ((designator (monitor-designator entry)))
      (eval `(ccl:advise ,designator
                         (progn (record-call ,monitor) (let ((*monitor* ,monitor)) (:do-it)))
                         :when :around
                         :name test::monitor-advice))
      (setf (find-monitor designator) monitor)))

  #+digitool
  (:method ((monitor monitor) (entry function))
    (let* ((advice-name (gensym "MONITOR"))
           (designator (monitor-designator entry))
           (advice (ccl::advise-global-def designator
                                           advice-name
                                           :around
                                           `(progn (record-call ,monitor)
                                                   (let ((*monitor* ,monitor)) (:do-it)))
                                           nil)))
      (ccl::advise-2 (eval advice) advice-name nil designator :around 'call-monitor nil)
      (setf (find-monitor designator) monitor)))
  #+clozure
  (:method ((monitor monitor) (entry function))
    (let* ((designator (monitor-designator entry)))
      (eval `(ccl:advise ,designator
                         (progn (record-call ,monitor) (let ((*monitor* ,monitor)) (:do-it)))
                         :when :around
                         :name test::monitor-advice))
      (setf (find-monitor designator) monitor)))

  #+digitool
  (:method ((monitor method-monitor) (entry method))
    "NB. this leaves the method in a state where it cannot be added to the fg, as its lambda
     list is specified as &rest."
    (let* ((advice-name (gensym "MONITOR"))
           (designator (monitor-designator entry))
           (advice (ccl::advise-global-def designator
                                           advice-name
                                           :around
                                           `(progn (record-call ,monitor)
                                                   (let ((*monitor* ,monitor)) (:do-it)))
                                           t)))
      (ccl::advise-2 (eval advice) advice-name nil designator :around 'call-monitor nil)
      (setf (find-monitor designator) monitor)))
  #+clozure
  (:method ((monitor method-monitor) (entry method))
    "NB. this leaves the method in a state where it cannot be added to the fg, as its lambda
     list is specified as &rest."
    (let* ((designator (monitor-designator entry)))
      (eval `(ccl:advise ,designator
                         (progn (record-call ,monitor) (let ((*monitor* ,monitor)) (:do-it)))
                         :when :around
                         :name test::monitor-advice))
      (setf (find-monitor designator) monitor)))

  (:method ((monitor monitor) (designator symbol))
    (setf (gethash designator *monitor-registry*) monitor))
  (:method ((monitor monitor) (designator cons))
    (setf (gethash designator *monitor-registry*) monitor))


  ;; removing
  (:method :around ((monitor null) (object t))
    (handler-case (call-next-method)
      (error (c)
        (warn "Attempt to unmonitor failed: ~s, ~s:~%~a"
              object monitor c)
        nil)))

  #+digitool
  (:method ((monitor null) (entry function))
    ;; both generic and not
    (let ((monitor (find-monitor entry)))
      (when monitor
        (ccl::unadvise-1 (monitor-designator monitor) :around 'call-monitor)
        (setf (find-monitor (monitor-designator monitor)) nil))))
  #+digitool
  (:method ((monitor null) (entry method))
    (let* ((monitor (find-monitor entry)))
      (when monitor
            (ccl::unadvise-1 (monitor-designator monitor) :around 'call-monitor)
            (setf (find-monitor (monitor-designator monitor)) nil))))

  #+clozure
  (:method ((monitor null) (entry function))
    ;; both generic and not
    (let ((monitor (find-monitor entry)))
      (when monitor
        (eval `(ccl:unadvise ,(monitor-designator monitor) :when :around :name test::monitor-advice))
        (remhash (monitor-designator monitor) *monitor-registry*))))
  #+clozure
  (:method ((monitor null) (entry method))
    (let ((monitor (find-monitor entry)))
      (when monitor
        (eval `(ccl:unadvise ,(monitor-designator monitor) :when :around :name test::monitor-advice))
        (remhash (monitor-designator monitor) *monitor-registry*))))


  (:method ((monitor null) (designator symbol))
    (remhash designator *monitor-registry*)
    nil)
  (:method ((monitor null) (designator cons))
    (remhash designator *monitor-registry*)
    nil))


(defun record-call (monitor)
  (when *monitor*
    (let ((position (position monitor (monitor-calls-monitors *monitor*))))
      (cond (position
             (princ #\. *trace-output*)
             (setf (aref (monitor-calls-called-p *monitor*) position) t))
            (*monitor-verbose*
             (warn "monitor w/o entry: ~a/~a."
                   (monitor-designator *monitor*) (monitor-designator monitor))))))
  (when *test-unit*
    (pushnew *test-unit* (monitor-tests monitor)))
  (setf (monitor-called-p monitor) t))


(defgeneric monitor (object)
  (:method :around ((object t))
    (handler-bind
      ((error (lambda (c)
                (warn "Attempt to monitor failed: ~s:~%~a" object c)
                (return-from monitor nil))))
      (call-next-method)))

  (:method ((object function))
    (when (find (monitor-package object) *monitor-packages*)
      (let ((designator (monitor-designator object)))
        (or (and (ccl::advisedp-1 designator :around 'call-monitor)
                 (find-monitor designator))
            (let ((original (ccl::find-unencapsulated-definition object)))
              (setf (find-monitor object)
                    (make-instance 'function-monitor
                      :object original
                      :designator designator)))))))

  (:method ((object generic-function))
    "given a generic function, monitor the individual methods, as well as the
     function itself."
    (when (find (monitor-package object) *monitor-packages*)
      (let ((designator (monitor-designator object)))
        ;; always continue with the methods
        (map nil #'monitor (generic-function-methods object))
        (or (and (ccl::advisedp-1 designator :around 'call-monitor)
                 (find-monitor designator))
            (setf (find-monitor object)
                  (make-instance 'generic-function-monitor
                    :object object
                    :designator designator))))))

  (:method ((object method))
    (when (find (monitor-package object) *monitor-packages*)
      (let ((designator (monitor-designator object)))
        (or (and (ccl::advisedp-1 designator :around 'call-monitor)
                 (find-monitor designator))
            (let ((original (ccl::find-unencapsulated-definition object)))
              ;; ensure that the changes appear in effective methods
              (ccl::remove-obsoleted-combined-methods object)
              (setf (find-monitor object) (make-instance 'method-monitor
                                            :object original
                                            :designator designator)))))))

  (:method ((object package))
    (when (some #'(lambda (name) (eq object (find-package name)))
                '(:ccl :common-lisp))
      ;; monitors are guaranteed to fail on some internal functions
      ;; the likely effect is a hung run-time
      (cerror "Do it anyway." "You probably don't want to monitor this package: ~s." object))
    (pushnew object *monitor-packages*)
    (let ((count 0))
      (with-package-iterator (next object :internal :external)
        (loop (multiple-value-bind (sym-p sym) (next)
                (unless sym-p (return))
                (when (fboundp sym)
                  (monitor sym)
                  (incf count))
                (let ((setf `(setf ,sym)))
                  (when (fboundp setf)
                    (monitor setf)
                    (incf count))))))
      count))

  (:method ((object symbol))
    (cond ((keywordp object)
           (monitor (or (find-package object)
                        (error "Invalid package designator: ~s." object))))
          ((and (find (symbol-package object) *monitor-packages*)
                (fboundp object)
                ;; skip macros
                (not (macro-function object)))
           (monitor (fdefinition object)))))

  (:method ((object cons))
    (let ((op (first object)))
      (cond ((or (stringp op) (keywordp op))
             (dolist (op object) (monitor op)))
            ((and (eq (first object) 'setf)
                  (find (symbol-package (second object)) *monitor-packages*)
                  (fboundp object))
             (monitor (fdefinition object)))
            (t
             (error "Invalid monitor object: ~s." object))))))

(defgeneric unmonitor (object)
  (:method ((object function))
    (setf (find-monitor object) nil))

  (:method ((object generic-function))
    (map nil #'unmonitor (generic-function-methods object)))

  (:method ((object method))
    (setf (find-monitor object) nil))

  (:method ((object package))
    (with-package-iterator (next object :internal :external)
      (loop (multiple-value-bind (sym-p sym) (next)
              (unless sym-p (return))
              (unmonitor sym)
              (let ((setf `(setf ,sym)))
                (when (fboundp setf)
                  (unmonitor setf)))))))

  (:method ((object symbol))
    (if (keywordp object)
      (unmonitor (or (find-package object)
                     (error "Invalid package designator: ~s." object)))
      (when (and (fboundp object)
                 (not (macro-function object)))
        (unmonitor (fdefinition object)))))

  (:method ((object cons))
    (let ((op (first object)))
      (cond ((or (stringp op) (keywordp op))
             (dolist (op object) (unmonitor op)))
            ((and (eq (first object) 'setf)
                  (fboundp object))
             (unmonitor (fdefinition object)))
            (t
             (error "Invalid monitor object: ~s." object))))))


(defgeneric initialize-monitor (object)
  (:method ((monitor monitor))
    (reinitialize-instance monitor))
  (:method ((object null))
    nil)

  (:method ((object function))
    (initialize-monitor (find-monitor object)))

  (:method ((object generic-function))
    (map nil #'initialize-monitor (generic-function-methods object)))

  (:method ((object method))
    (initialize-monitor (find-monitor object)))

  (:method ((object package))
    (with-package-iterator (next object :internal :external)
      (loop (multiple-value-bind (sym-p sym) (next)
              (unless sym-p (return))
              (initialize-monitor sym)
              (let ((setf `(setf ,sym)))
                (when (fboundp setf)
                  (initialize-monitor setf)))))))

  (:method ((object symbol))
    (if (keywordp object)
      (initialize-monitor (or (find-package object)
                     (error "Invalid package designator: ~s." object)))
      (when (fboundp object)
        (initialize-monitor (fdefinition object)))))

  (:method ((object cons))
    (when (and (eq (first object) 'setf)
               (fboundp object))
      (initialize-monitor (fdefinition object)))))


;;;
;;; reports

(defgeneric report-monitor-pathname-entry (object registry)
  (:method ((object string) registry)
    (report-monitor-pathname-entry (pathname object) registry))
  (:method ((object pathname) registry)
    (pushnew object
             (rest (report-monitor-pathname-entry (cons (pathname-host object) (pathname-directory object)) registry))
             :test #'equalp))
  (:method ((object cons) registry)
    (cond ((gethash object registry))
          ((cddr object)
           (let ((entry (setf (gethash object registry) (list object)))
                 (parent-entry (report-monitor-pathname-entry (butlast object) registry)))
             (pushnew object (rest parent-entry) :test #'equalp)
             entry))
          (t
           (let ((host (string (first object))))
             (setf (gethash host registry) (list host)))))))
           

#|
(let ((registry (make-hash-table :test #'equalp)))
  (dolist (o '("TSL:lisp;test;test.lisp"
               "TSL:lisp;test;a.lisp"
               "TSL:lisp;b.lisp"))
    (report-monitor-pathname-entry o registry))
  (maphash #'(lambda (k v) (format t "~&~s -> ~s" k v))
           registry))
           

|#

(macrolet ((with-element (tag . body)
             (destructuring-bind (name . attributes) (if (consp tag) tag (list tag))
               `(progn (write-string ,(format nil "~%<~a" name) stream)
                       ,@(mapcar #'(lambda (attr)
                                     (destructuring-bind (name value) attr
                                       `(format stream ,(format nil " ~a='~~a'" name) ,value)))
                                 attributes)
                       (write-char #\> stream)
                       ,@(if (stringp (first body))
                           `((write-string ,(first body) stream))
                           body)
                       (write-string ,(format nil "~%</~a>" name) stream)))))

  (defgeneric report-monitor (monitor to as &key test name)

    (:method ((designator (eql t)) (to t) (as t) &rest args)
      (apply #'report-monitor *monitor-registry* to as args))

    (:method ((designator string) (to t) (as t) &rest args)
      (apply #'report-monitor (or (find-package designator) (error "Package not found: ~s." designator))
             to as args))

    (:method ((designator symbol) (to t) (as t) &rest args)
      (apply #'report-monitor (or (find-package designator) (error "Package not found: ~s." designator))
             to as args))

    (:method ((monitored-package package) (to t) (as t) &rest args)
      (apply #'report-monitor *monitor-registry* to as
             :test #'(lambda (monitor) (eq (monitor-package monitor) monitored-package))
             args))
    
    (:method ((monitors t) (to pathname) (as t) &rest args)
      (ensure-directories-exist to)
      (with-open-file (stream to :direction :output :if-exists :supersede :if-does-not-exist :create)
        (apply #'report-monitor monitors stream as args)))
    
    (:method ((monitors hash-table) (stream stream) (as mime:*/svg)
              &key (test #'identity) (name (gensym "monitors")))
      (setf.dot:with-context (make-instance 'setf.dot:stream :stream stream)
        (setf.dot:graph name nil
          (maphash #'(lambda (name monitor)
                       (declare (ignore name))
                       (when (funcall test monitor)
                         (setf.dot:node monitor :label (monitor-namestring monitor)
                                        :color (if (monitor-called-p monitor) *color.called* *color.no-call*))
                         (loop for calls-monitor across (monitor-calls-monitors monitor)
                               for called-p across (monitor-calls-called-p monitor)
                               do (setf.dot:edge monitor calls-monitor
                                    :color (if (zerop called-p) *color.no-call* *color.called*)))))
                   monitors))))
    
    (:method ((monitors hash-table) (to pathname) (as mime:*/html)
              &key (test #'identity) (name (pathname-name to)) (verbose nil)
              (root-pathname nil)
              &allow-other-keys)
      (let ((by-pathname (make-hash-table :test 'equal))
            (source-tree (make-hash-table :test 'equal))
            (total-count 0)
            (total-call-count 0)
            (total-out-count 0)
            (total-out-call-count 0))
        (ensure-directories-exist to)

        (loop for monitor being the hash-value of monitors
              when (funcall test monitor)
              do (let ((pathname (monitor-pathname monitor)))
                   (cond (pathname
                          (report-monitor-pathname-entry pathname source-tree)
                          (push monitor (gethash (pathname pathname) by-pathname)))
                         ((typep monitor 'generic-function-monitor)
                          )
                         (t
                          (when verbose
                            (warn "monitor lacks pathname: ~s" (monitor-designator monitor)))
                          (push monitor (gethash "unknown" by-pathname))))))

        (with-open-file (stream (merge-pathnames to (make-pathname :name "index" :type "html"))
                                :direction :output :if-exists :supersede :if-does-not-exist :create)
          (labels ((do-leaf (pathname monitors depth)
                     (multiple-value-bind (count call-count out-count out-call-count)
                                          (report-monitor (sort monitors #'string-lessp
                                                                :key #'monitor-namestring)
                                                          (make-pathname :name (pathname-name pathname)
                                                                         :type "html"
                                                                         :directory (append (or (pathname-directory to)
                                                                                                '(:absolute))
                                                                                            (rest (pathname-directory pathname)))
                                                                         :defaults to)
                                                          as
                                                          :name (namestring (if (probe-file pathname)
                                                                              (truename pathname)
                                                                              pathname)))
                       ;;(print (list (list pathname (length monitors)) count call-count out-count out-call-count))
                       (incf total-count count)
                       (incf total-call-count call-count)
                       (incf total-out-count out-count)
                       (incf total-out-call-count out-call-count)
                       (with-element ("tr")
                         (with-element ("td" ("style" "border-right: solid black 1px"))
                           ; include a link to the individual results
                           (dotimes (i depth)
                             (write-string "&nbsp;" stream))
                           (with-element ("a" ("href" (format nil "~{~a/~}~a.html"
                                                              (rest (pathname-directory pathname))
                                                              (pathname-name pathname)))
                                          ("target" (pathname-name pathname)))
                             (write-string (file-namestring pathname) stream)))
                         (with-element "td"        ; include a link to the source file
                           (let ((truename (ignore-errors (truename pathname))))
                             (if truename
                               (let ((pathname (if root-pathname
                                                 (pathname (enough-namestring truename root-pathname))
                                                 truename)))
                                 (with-element ("a" ("href" (format nil "/Volumes/~{~a/~}~a"    ; fix this - works for a mounted os x volume
                                                                    (rest (pathname-directory pathname))
                                                                    (file-namestring pathname)))
                                                ("type" "text")         ; suppress safari app change
                                                ("target" (file-namestring pathname)))
                                   (format stream "~a" count))))))
                         (with-element "td" (format stream "~$"
                                                    (if (zerop count) 0 (* 100.0 (/ call-count count)))))
                         (with-element "td" (format stream "~a" out-count))
                         (with-element "td" (format stream "~$"
                                                    (if (zerop out-count) 0 (* 100.0 (/ out-call-count out-count))))))))
                   (do-node (key entries &optional (depth 0))
                     (with-element "tr"
                       (with-element ("td" ("style" "border-right: solid black 1px"))
                         (dotimes (i depth) (write-string "&nbsp;" stream))
                         (typecase key
                           (string (write-string key stream))
                           (cons (format stream "~a:~{~a/~}" (first key) (cddr key))))))
                     (dolist (entry (sort (copy-list entries) #'string-lessp :key #'write-to-string))
                       (etypecase entry
                         (pathname (do-leaf entry (gethash entry by-pathname) (1+ depth)))
                         (cons
                          (do-node entry (rest (gethash entry source-tree)) (1+ depth)))))))
            
            
            ;; generate the index
            (with-element ("html")
              (with-element ("head")
                (with-element ("title")
                  (format stream "Monitor Report : ~a" name)))
              (with-element ("body")
                (with-element ("table")
                  (with-element ("tr" ("style" "background-color: lightgray"))
                    (with-element ("td" ("colspan" "5") ("style" "background-color: lightgray")) "by source file"))
                  (with-element "tr"
                    (with-element "th" (write-string "source file" stream))
                    (with-element "th" (write-string "entry count" stream))
                    (with-element "th" (write-string "called %" stream))
                    (with-element "th" (write-string "calles out" stream))
                    (with-element "th" (write-string "called out %" stream)))
                  (dolist (host.entries (sort (loop for host being the hash-key of source-tree
                                                  using (hash-value entry)
                                                  when (stringp host)
                                                  collect entry)
                                            #'string-lessp
                                            :key #'first))
                    (do-node (first host.entries) (rest host.entries)))
                  (let ((unknowns (gethash "unknown" by-pathname)))
                    (when unknowns (do-leaf "unknown" unknowns 0)))
                  (with-element "tr"
                    (with-element ("td" ("colspan" "5") ("style" "background-color: lightgray")) "totals"))
                  (with-element ("tr" ("style" "background-color: lightgray"))
                    (with-element "td")
                    (with-element "td" (format stream "~a" total-count))
                    (with-element "td" (format stream "~a" (if (zerop total-count) 0 (float (/ total-call-count total-count)))))
                    (with-element "td" (format stream "~a" total-out-count))
                    (with-element "td" (format stream "~a" 
                                               (if (zerop total-out-count) 0 (float (/ total-out-call-count total-out-count)))))))))
            (values to total-count total-call-count total-out-count total-out-call-count)))))
                        

    (:method ((monitors hash-table) (stream stream) (as mime:*/html)
              &key (test #'identity) (name "?") &allow-other-keys)
      (let ((monitors (loop for monitor being the hash-value of monitors
                            when (funcall test monitor)
                            collect monitor)))
        (with-element ("html")
          (with-element ("head")
            (with-element ("title")
              (format stream "Monitor Report : ~a" name)))
          (with-element ("body")
            (with-element ("table")
              (with-element ("tr")
                (with-element ("td" ("style" "border: solid black 1px; width: 4in;")) "entry")
                (with-element ("td" ("style" "border: solid black 1px; width: 2in;")) "calls out")
                (with-element ("td" ("style" "border: solid black 1px; width: 4in;")) "tests"))
              (dolist (monitor (sort monitors #'string-lessp
                                     :key #'monitor-namestring))
                (report-monitor monitor stream as)))))))
    
    (:method ((monitors list) (stream stream) (as mime:*/html)
              &key (name (gensym "monitors")) &allow-other-keys)
      (let ((count 0)
            (call-count 0)
            (out-count 0)
            (out-call-count 0))
        (with-element ("html")
          (with-element ("head")
            (with-element ("title")
              (format stream "Monitor Report : ~a" name)))
          (with-element ("body")
            (with-element ("table")
              (with-element ("tr")
                (with-element ("td" ("style" "border: solid black 1px; width: 4in;")) "entry")
                (with-element ("td" ("style" "border: solid black 1px; width: 2in;")) "calls out")
                (with-element ("td" ("style" "border: solid black 1px; width: 4in;")) "tests"))
              (dolist (monitor monitors)
                (multiple-value-bind (called o-count o-call-count)
                                     (report-monitor monitor stream as)
                  (incf count)
                  (when called (incf call-count))
                  (incf out-count o-count)
                  (incf out-call-count o-call-count))))))
        (values count call-count out-count out-call-count)))
  
    (:method ((monitor monitor) (stream stream) (as mime:*/html) &key &allow-other-keys)
      (let ((called-p (monitor-called-p monitor))
            (out-count 0)
            (out-call-count 0))
        (with-element ("tr")
          (with-element ("td" ("style" (format nil "~a; border-right: solid black 1px; border-left: solid black 1px"
                                               (if called-p *style.called* *style.no-call*))))
            (format stream "~a" (monitor-namestring monitor)))
          (with-element ("td" ("style" "border-right: solid black 1px"))
            (loop for calls-monitor across (monitor-calls-monitors monitor)
                  for called-p across (monitor-calls-called-p monitor)
                  do (with-element ("span" ("style" (if called-p *style.called* *style.no-call*)))
                       (when called-p (incf out-call-count))
                       (incf out-count)
                       (format stream " ~a" (monitor-namestring calls-monitor)))))
          (with-element ("td" ("style" "border-right: solid black 1px"))
            (loop for test-unit in (monitor-tests monitor)
                  do (with-element ("div" ("style" (case (test:test-unit-status test-unit)
                                                     (:passed *style.passed*)
                                                     (:failed *style.failed*)
                                                     (t *style.unknown*))))
                       (format stream " ~{~a~^.~}" (test:test-unit-path test-unit))))))
        (values called-p out-count out-call-count))))

  )
