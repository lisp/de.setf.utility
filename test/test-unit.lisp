;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;  This file is part of the 'de.setf.utility' library component.
;;;  It implementes a simple test framework

;;;  Copyright 2002,2009,2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with 'de.setf.utility'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.

;;;  a definition and execution mechanism for simple tests.
;;;  each test step corresponds to a named leaf in a test tree and comprises its path name,
;;;  a function to perform and a predicate to apply to that functions result. a test definition
;;;  supplies the function as an s-expression form and the predicate either as an s-expression form
;;;  to be compared to the result, or a function name or a lambda expression to be applied to the result.</p>
;;;
;;;  test:test (name { documentation } form &rest arguments)
;;;    defines the named test operator to execute the form in a protected context and interpret its
;;;    success with respect to the arguments
;;;    - :prerequisites names tests which must succeed before this test should run
;;;    - :situation specifies one of `:define` `:execute` to indicate immediate or defered execution
;;;    - `:value` and `:values` specify constraints on the returned value(s).
;;;    - no form is equivalent to specifying `:value t`
;;;    - a single form is equivalent to specifying `:value` _`form`_
;;;    - multiple forms is equivalent to specifying `:values (list ` _`forms`_ `)`
;;;  *test-unit-mode* 
;;;   specifies, test reporting circumstances
;;;   - :silent : reports failures only
;;;   - :verbose : reports every execution with results
;;;   - :report : reporsts succes and failure
;;;  *test-unit-situation*
;;;   indicates the execution situation
;;;   - :define  : correspsonds to test definition
;;;   - :execute corresponds to EXECUTE-TESTS

;;;
;;;  20020601 ja : new
;;;  20020601 ja : path/hashtable-based cache rather than tree-based
;;;  20090331 janderson : to facliltate individual test definition, the improved
;;;    the deftest operator with mode, situation.
;;;  20100110 ja  changed nil mode to :terse to make it clearer when setting



(defparameter *class.test-unit* 'test-unit)

(defparameter *test-unit* nil
  "Bound to the current test unit during execute-test.")

(defparameter *test-unit-mode* :terse
  "one of :silent :verbose :report :terse")

(defparameter *test-unit-situation* nil
  "member (:define :execute nil).
 in the :define situation, unless otherwise specified, a test is interned.
 in the :execute situation, unless otherwise specified, a test is executed.")

(defvar *tests* (make-hash-table :test 'equal)
  "test registry")

(defparameter *test-output* nil)

(defparameter *test-=* #'=
  "binds the two-argument numeric equality predicate. by default, =, in order
 that float precision does not distinguish otherwise equal numbers.")

(defun test-output (&key (window-title "Test Log" window-title-p))
  #-digitool (declare (ignore window-title window-title-p))
  (flet ((new-test-output ()
           #-digitool *trace-output*
           #+digitool (make-instance 'fred-window :scratch-p t :window-title window-title)))
    (cond #+digitool
          ((typep *test-output* 'fred-window)
           ;; iff the window is intact, re-use it;
           ;; change the title only when the argumetn was passed
           (cond ((wptr *test-output*)
                  (when window-title-p (set-window-title *test-output* window-title))
                  *test-output*)
                 (t
                  (setf *test-output* (new-test-output)))))
          ((and (typep *test-output* 'stream) (open-stream-p *test-output*))
           *test-output*)
          (t
           (setq *test-output* (new-test-output))))))
 
   
(deftype test-unit-mode ()
  "Indicates how to protocol a test execution:
 :verbose    report the name and form at start, and the intended and actual
               result, and status upon completion
 :report     the name and status
 :silent     report nothing
 :terse         report name and status for failure only."
 '(member :verbose :report :silent :terse))

(deftype test-unit-situation ()
  '(member :define :execute nil))

(defClass test-unit ()
  ((path :initarg :path :reader test-unit-path)
   (form :initarg :form :reader test-unit-form)
   (function :initarg :function :reader test-unit-function)
   (documentation :initarg :documentation :initform nil :reader test-unit-documentation)
   (predicate-function :initarg :predicate-function :reader test-unit-predicate-function)
   (predicate-form :initarg :predicate-form :reader test-unit-predicate-form)
   (predicate :initarg :predicate  :initform nil :reader test-unit-predicate)
   (prerequisites :initarg :prerequisites :initform nil :reader test-unit-prerequisites)
   (status :initform nil :accessor test-unit-status :initarg :status)
   (mode :initarg :mode :reader test-unit-mode :type test-unit-mode)
   (situation
    :initarg :situation :reader test-unit-situation
    :type test-unit-situation
    :documentation "specifies the test's individual processing situation.
 when this matches the test processing situation, the action is performed on the test.")))

(defmethod initialize-instance ((instance test-unit) &rest args
                                &key (mode *test-unit-mode*) (situation *test-unit-situation*))
  (apply #'call-next-method instance
         :situation situation
         :mode (or mode (case situation (:define :silent) (t nil)))
         args))

(defmethod print-object ((instance test-unit) stream)
  (print-unreadable-object (instance stream :type t)
    (format stream "~{~a~^.~} ~a ~@[(~s)~]"
            (test-unit-path instance)
            (test-unit-status instance)
            (test-unit-situation instance))))

(defgeneric test-unit-situation-p (test situation)
  (:method ((test test-unit) (situation t))
           (test-unit-situation-p (test-unit-situation test) situation))
  (:method ((test-unit-situation (eql t)) (situation t))
           t)
  (:method ((test-unit-situation symbol) (situation symbol))
           (eq test-unit-situation situation))
  (:method ((test-unit-situation symbol) (situation cons))
           (member test-unit-situation situation))
  (:method ((test-unit-situation cons) (situation symbol))
           (member situation test-unit-situation))
  (:method ((test-unit-situation cons) (situation cons))
           (intersection test-unit-situation situation)))

(defun test-unit-verbose-p (&optional test-unit)
  (if test-unit
    (eq (test-unit-mode test-unit) :verbose)
    (eq *test-unit-mode* :verbose)))

(defgeneric test-equal (v1 v2)
  (:documentation "establish recursive equality based on EQUAL, with the
 exception, that
 - right T matches all non-null, and non-condition left values
 - right condition class designator matches conditions by type")
  (:method ((left null) (right null)) t)
  (:method ((left null) (right (eql t))) nil)
  (:method ((left condition) (right (eql t))) nil)
  (:method ((left t) (right (eql t))) t)
  (:method ((left cons) (right cons))
    (labels ((plist-equal (left right)
               ;; relax plist order
               (loop for (key left-value) on left by #'cddr
                     do (let ((right-value (getf right key left)))
                          (unless (test-equal left-value right-value)
                            (break "test-equal plist failed.")
                            (return nil)))
                     finally (return t))))
      (if (ignore-errors (and (keywordp (first left)) (keywordp (first right))
                              (evenp (length left))
                              (= (length left) (length right))))
        (plist-equal left right)
        (and (test-equal (first left) (first right))
             (test-equal (rest left) (rest right))))))
  (:method ((left vector) (right vector))
    (and (= (length left) (length right))
         (every #'test-equal left right)))
  (:method ((left condition) (right symbol))
    (and (subtypep right 'condition) (typep left right)))
  (:method ((left t) (right symbol))
    (and (find-class right nil) (typep left right)))
  (:method ((left number) (right number))
    (funcall *test-=* left right))
  (:method ((left t) (right t))
    ;; equalp to allow structures
    (equalp left right))
  (:method ((left symbol) (right symbol))
    (or (eq left right)
        ;; treat uninterned symbols as wild-cards
        (and (null (symbol-package left)) (null (symbol-package right))))))

;;; (defmethod test-equal :around ((x t) (y t)) (or (call-next-method) (break "test-equal failed.")))

(defgeneric test-unit-path (path)
  (:documentation
   "Given a TEST-UNIT, return it's path. Otherwise parse and/or canonicalize a
 path designator.

 PATH : (or (designator STRING) TEST-UNIT LIST)
 VALUE : LIST : the path")

 (:method ((path string))
    (test-unit-path (split-string path " ./")))

 (:method ((path null))
   nil)

 (:method ((path symbol))
   (test-unit-path (symbol-name path)))

 (:method ((path cons))
   "handle wild component and otherwise map string designators to downcased strings."
   (mapcar #'(lambda (step)
               (cond ((string-equal step "*") :wild)
                     ((string-equal step "**") :wild-inferiors)
                     ((member step '(:wild :wild-inferiors)) step)
                     ((symbolp step) (string step))
                     ((stringp step) (if (find-if #'upper-case-p step) (string-downcase step) step))
                     (t (error "illegal path step: ~s." step))))
           path)))

(defgeneric test-unit-name (unit)
  (:method ((unit test-unit))
    (test-unit-name (test-unit-path unit)))
  (:method ((path null))
    nil)
  (:method ((path cons))
    (intern (format nil "~{~:@(~a~)~^.~}" path) :keyword)))

(defun walk-prerequisites (unit function &key (mode :walk-call) (self-p nil) (exclude nil))
  (labels ((prerequisites (unit)
             (mapcar #'(lambda (path) (or (find-tests path)
                                          (error "test not found: ~s." path)))
                     (test-unit-prerequisites unit)))
           (walk (unit)
             (etypecase unit
               (list (map nil #'walk unit))
               (test-unit
                (unless (find unit exclude)
                  (push unit exclude)
                  (ecase mode
                    (:walk-call (map nil #'walk (prerequisites unit)) (funcall function unit))
                    (:call-walk (funcall function unit) (map nil #'walk (prerequisites unit)))))))))
    (when self-p (walk unit))
    (map nil #'walk (prerequisites unit))))

(defun is-prerequisite (unit1 unit2)
  (unless (eq unit1 unit2)
    (walk-prerequisites unit2 #'(lambda (prerequisite) (when (eq unit1 prerequisite) (return-from is-prerequisite t)))
                        :mode :call-walk)))

(defgeneric unit-path-lessp (path1 path2)
  (:method ((path1 null) (path2 t)) t)
  (:method ((path1 null) (path2 null)) nil)
  (:method ((path1 t) (path2 null)) nil)
  (:method ((path1 string) (path2 string)) (string-lessp path1 path2))
  (:method ((path1 cons) (path2 cons))
           (or (unit-path-lessp (first path1) (first path2))
               (unit-path-lessp (rest path1) (rest path2))))
  (:method ((path1 cons) (path2 string))
           (unit-path-lessp (first path1) path2))
  (:method ((path1 string) (path2 cons))
           (unit-path-lessp path1 (first path2))))

(defun wild-unit-path-p (path)
  (and (consp path) (or (find :wild path) (find :wild-inferiors path))))

(defgeneric unit-path-match-p (path path-pattern)
  (:method ((path t) (path-pattern t)) nil)
  (:method ((path null) (path-pattern null)) t)
  (:method ((path null) (path-pattern (eql :wild))) nil)
  (:method ((path string) (path-pattern string)) (string-equal path path-pattern))
  (:method ((path symbol) (path-pattern symbol)) (string-equal path path-pattern))
  (:method ((path string) (path-pattern (eql :wild))) t)
  (:method ((path symbol) (path-pattern (eql :wild))) t)
  (:method ((path null) (path-pattern cons)) (equal path-pattern '(:wild-inferiors)))
  (:method ((path cons) (path-pattern cons))
           (or (and (unit-path-match-p (first path) (first path-pattern))
                    (unit-path-match-p (rest path) (rest path-pattern)))
               (when (eq (first path-pattern) :wild-inferiors)
                 (or (unit-path-match-p path (rest path-pattern))
                     (unit-path-match-p (rest path) path-pattern))))))
          

(defgeneric find-tests (path-to-match)
  (:method ((path-to-match cons))
    (when (find-if #'(lambda (step)
                       (if (stringp step)
                         (find-if #'upper-case-p step)
                         (not (member step '(:wild :wild-inferiors)))))
                   path-to-match)
      (setf path-to-match (test-unit-path path-to-match)))
    (flet ((execute-before (u1 u2)
             (or (is-prerequisite u1 u2)
                 (unit-path-lessp (test-unit-path u1) (test-unit-path u2)))))
      (or (if (wild-unit-path-p path-to-match)
            (let ((units nil))
              (maphash #'(lambda (path unit)
                           (when (unit-path-match-p path path-to-match) (push unit units)))
                       *tests*)
              (if (rest units)
                (sort units #'execute-before)
                units))
            (let ((unit (gethash path-to-match *tests*)))
              (when unit (list unit))))
          (error "test not found: ~s." path-to-match))))
  (:method ((path-to-match string))
    (find-tests (test-unit-path path-to-match)))
  (:method ((path-to-match symbol))
    (when path-to-match
      (find-tests (string path-to-match)))))

(defgeneric find-test (name)
  (:method ((name null))
    nil)
  (:method ((name symbol))
    (find-test (string name)))
  (:method ((name string))
    (find-test (test-unit-path name)))
  (:method ((path cons))
    (gethash path *tests*)))

(defgeneric (setf find-test) (test name)
  (:method ((test t) (name null))
    nil)
  (:method ((test t) (name symbol))
    (setf (find-test (string name)) test))
  (:method ((test t) (name string))
    (setf (find-test (test-unit-path name)) test))
  (:method ((test test-unit) (path cons))
    (setf (gethash path *tests*) test))
  (:method ((test null) (path cons))
    (set-tests path nil)))
  


(defgeneric set-tests (path-to-set unit)
  (:method ((path string) (unit t))
           (set-tests (test-unit-path path) unit))
  (:method ((path null) (unit t))
           unit)
  (:method ((path-to-set cons) (unit null))
           (if (wild-unit-path-p path-to-set)
             (maphash #'(lambda (path old-unit)
                          (declare (ignore old-unit))
                          (when (unit-path-match-p path path-to-set)
                            (remhash path *tests*)))
                      *tests*)
             (remhash (test-unit-path path-to-set) *tests*))
           nil)
  (:method ((path-to-set cons) (unit test-unit))
           (if (wild-unit-path-p path-to-set)
             (error "wild path provided for test instance: ~s: ~s." path-to-set unit)
             (setf (gethash (test-unit-path path-to-set) *tests*) unit))))

(defgeneric define-test (test)
  (:method ((test test-unit))
    (set-tests (test-unit-path test) test)
    (let ((name (test-unit-name test)))
      (when name
        (setf (fdefinition name) #'(lambda (&rest args) (apply #'execute-test test args)))
        #+digitool
        (ccl:record-source-file name 'function))
      (if (test-unit-situation-p test :define)
        (if name
          (values name (%execute-test test))
          (%execute-test test))
        (if name
          name
          test))))
  (:method ((test null)) nil))

(defun execute-tests (&rest args)
  (mapcar #'execute-test args))

(defgeneric execute-test (unit &key &allow-other-keys)
 (:method ((path cons) &rest args &key (prerequisites t) force-p
           ((:break-on-signals *break-on-signals*) *break-on-signals*)
           &aux (units (find-tests path)) (specified-units (copy-list units)))
    "execute the tests designated by PATH. Augment the set with all prerequisite
 tests and order it by precedence. If the unit is one of those explicitly
 specified, then force its execution. Otherwise skip passed tests."
    (let ((passed 0) (failed 0) (skipped 0) (known-failed 0))
      (when prerequisites
        (map nil #'(lambda (unit) (walk-prerequisites unit #'(lambda (unit) (push unit units)) :exclude units)) units))
      (dolist (unit (sort units #'is-prerequisite))
        (ecase (apply #'execute-test unit
                      :force-p (or force-p
                                   (eq prerequisites :force)
                                   (and (find unit specified-units) (not (eq :known-failed (test-unit-status unit)))))
                      args)
          (:skipped (incf skipped))
          (:passed (incf passed))
          (:failed (incf failed))
          (:known-failed (incf known-failed))))
      (values (if (plusp failed) :failed (if (plusp passed) :passed :skipped))
              passed failed skipped known-failed)))
  (:method ((path string) &rest args)
           (apply #'execute-test (test-unit-path path) args))
  (:method ((path symbol) &rest args)
           (apply #'execute-test (string path) args))
  (:method ((unit null) &key &allow-other-keys)
           nil)

  (:method ((unit test-unit) &key (mode *test-unit-mode*) (force-p nil) (stream *trace-output*) debug)
    (cond ((and (eq (test-unit-status unit) :passed) (not force-p))
           (when (eq mode :verbose)
             (format stream "~%~{~s~^.~}: skipped (passed)." (test-unit-path unit)))
           :skipped)
          ((and (eq (test-unit-status unit) :known-failed) (not force-p))
           (when (eq mode :verbose)
             (format stream "~%~{~s~^.~}: skipped (passed)." (test-unit-path unit)))
           :known-failed)
          ((and (not (test-unit-situation-p unit '(nil :execute))) (not force-p))
           (when (eq mode :verbose)
             (format stream "~%~{~s~^.~}: skipped (situation)." (test-unit-path unit)))
           :skipped)
          (t
           (%execute-test unit :mode mode :stream stream :debug debug)))))

(defun unintern-test (test) (set-tests (test-unit-path test) nil))

(defun make-test-unit (&rest args)
  (when *class.test-unit*
    (let ((unit (apply #'make-instance *class.test-unit* args)))
      ;; one funcallable classes are supported, set the function here ...
      unit)))


(defun clear-tests () (clrhash *tests*))

(defmacro deftests (&rest test-trees)
  (let ((tests nil) (*path* nil) (initargs nil))
    (declare (special *path*))
    (loop (unless (keywordp (first test-trees)) (return))
          (setf initargs (nconc (list (pop test-trees) (pop test-trees)) initargs)))
    (flet ((test-error (tree)
             (error "malformed test tree: ~s." tree)))
      (labels ((walk-test-tree (tree)
                 (destructuring-bind (step &rest rest &aux (*path* *path*)  unit-path)
                                     tree
                   (declare (special *path*))
                   (cond ((null rest)
                          (test-error tree))
                         ((consp (first rest))
                          (push (string step) *path*)
                          (map nil #'walk-test-tree rest))
                         ((keywordp (first rest))
                          ;; a test leaf
                          (destructuring-bind (&key form documentation value predicate type
                                                    prerequisite (prerequisites (when prerequisite (list prerequisite)))
                                                    (verbose  nil)
                                                    (mode (if verbose :verbose *test-unit-mode*))
                                                    &aux function predicate-function)
                                              rest
                            (unless (>= 1 (count-if #'identity (list value predicate type)))
                              (test-error tree))
                            (typecase form
                              (cons
                               (setf function `(function (lambda () ,form))))
                              ((and symbol (not null))
                               (setf function `(function form)))
                              (t
                               (test-error tree)))
                            (typecase predicate
                              (null (setf predicate-function (if value
                                                               `(function (lambda (result) (test-equal result ,value)))
                                                               (if type
                                                                 `(function (lambda (result) (typep result ',type)))
                                                                 '(function identity)))))
                              (symbol
                               (setf predicate-function `(function ,predicate)))
                              (cons
                               (unless (eq (first predicate) 'lambda)
                                 (test-error tree))
                               (setf predicate-function `(function ,predicate)))
                              (t
                               (test-error tree)))
                            (etypecase step
                              (list (setf *path* (append step *path*)))
                              ((or string symbol)
                               (push (string step) *path*)
                               (setf unit-path (reverse *path*))))
                            (push (list :form `(quote ,form)
                                        :path (when unit-path `(quote ,unit-path))
                                        :mode mode
                                        :function function
                                        :documentation documentation
                                        :predicate-function predicate-function
                                        :predicate-form (cond (predicate `'(function ,predicate))
                                                              (value `'(equalp ,value))
                                                              (type `'(typep ,type))
                                                              (t t))
                                        :prerequisites `(quote ,(mapcar #'test-unit-path prerequisites)))
                                  tests)))
                         (t
                          (test-error tree))))))
        (map nil #'walk-test-tree test-trees)))
    `(mapcar #'define-test
             (list ,@(mapcar #'(lambda (form) (cons 'make-test-unit (append form initargs))) tests)))))
     

(defmacro deftest (path form &rest options)
  (etypecase path
    (cons )
    (string (setf path (test-unit-path path)))
    (symbol (setf path (test-unit-path (string path)))))
  `(deftests (,path :form ,form ,@options)))

#+digitool
(pushnew '(deftest . 1) ccl:*fred-special-indent-alist* :key #'first)

(defmacro test (name form &rest args)
  (let ((documentation (when (stringp form) (shiftf form (pop args)))))
    (flet ((generate (&key (value t) (values `(list ,value)) (situation *test-unit-situation*)
                           verbose (mode (when verbose :verbose))
                           (documentation documentation) (predicate nil) (status nil)
                           (prerequisites nil)
                           (test 'test-equal))
             `(define-test
                (make-test-unit :path ',(test-unit-path name)
                                :form ',form
                                :function (function (lambda () ,form))
                                ,@(if predicate
                                    `(:predicate-function (function ,predicate)
                                                          :predicate-form ',predicate)
                                    `(:predicate-function (function (lambda (&rest results)
                                                                      (loop for result in results
                                                                            for value in ,values
                                                                            unless (,test result value)
                                                                            do (return nil)
                                                                            finally (return t))))
                                                          :predicate-form '(equalp ,values)))
                                ,@(when documentation `(:documentation ,documentation))
                                ,@(when situation `(:situation ,situation))
                                ,@(when status `(:status ,status))
                                ,@(when prerequisites `(:prerequisites ',prerequisites))
                                ,@(when mode `(:mode ,mode))))))
      (if args
        (if (keywordp (first args))
          (apply #'generate args)
          (if (rest args)
            (generate :values `(list ,@args))
            (generate :value (first args))))
        (generate :value t)))))
#+digitool
(setf (ccl:assq 'test ccl:*fred-special-indent-alist*) 1)

(defmacro with-test-situation ((situation &key mode) &body body)
  `(let ((*test-unit-situation* ,situation)
         ,@(when mode `((*test-unit-mode* ,mode))))
     (assert (typep *test-unit-situation* 'test-unit-situation) ()
             "Invalid test unit situation: '~s'." *test-unit-situation*)
     (assert (typep *test-unit-mode* 'test-unit-mode) ()
             "Invalid test unit mode: '~s'." *test-unit-mode*)
     ,@body))

(defgeneric %execute-test (unit &key &allow-other-keys)
  (:method ((unit test-unit) &key ((:mode *test-unit-mode*) (or (test-unit-mode unit) *test-unit-mode*))
            (stream *trace-output*) (debug nil)
            &aux results (*test-unit* unit))
     (handler-bind ((error (lambda (condition)
                             (format stream "~&signaled condition: ~a: ~a~% with~%~s" (type-of condition)  condition results)
                             (return-from %execute-test (values :failed condition)))))
             
       (when (eq *test-unit-mode* :verbose)
         (format stream "~%~s:~%~:W ... "
                 (test-unit-path unit) (test-unit-form unit)))
       (case (test-unit-status unit)
         (:known-failed )
         (t (setf (test-unit-status unit) nil)))
       (setf results
             (block :run-test
               (multiple-value-list (handler-bind
                                      ((error (lambda (condition) 
                                                (when debug (break "~%test ~s signaled:~%~a"
                                                                   (test-unit-path unit) condition))
                                                (return-from :run-test (list condition)))))
                                      (funcall (test-unit-function unit))))))
       (cond ((member (first results) '(:skipped :nyi))
              :skipped)
             ((apply (test-unit-predicate-function unit) results)
              (setf (test-unit-status unit) :passed)
              (case *test-unit-mode*
                (:silent )
                (:verbose
                 (format stream "~%~{~a~^.~} passed with results:~% ~s"
                         (or (test-unit-path unit) (test-unit-form unit))
                         results)
                 (finish-output stream))
                (:report
                 (format stream " +(~s)" (test-unit-name unit))
                 (finish-output stream))
                (:terse
                 ;; nothing (write-char #\+ stream)
                 (finish-output stream)))
              :passed)
             (t
              (case (test-unit-status unit)
                (:known-failed )
                (t (setf (test-unit-status unit) :failed)))
              (case *test-unit-mode*
                (:silent )
                (:verbose
                 (format stream "~&test failed: ~{~a~^.~}" (test-unit-path unit))
                 (format stream "~@[~%----------------------------------------~%~a~%----------------------------------------~]"
                         (test-unit-documentation unit))
                 (format stream
                         "~%~%form: ~s~%produced:~%~:W~%~@[expected: ~:W~]~@[predicate: ~s~]"
                         (test-unit-form unit)
                         results
                         (test-unit-predicate-form unit)
                         (test-unit-predicate unit))
                 (when (typep (first results) 'error)
                   (format stream "~%~a" (first results)))
                 (terpri stream)
                 (finish-output stream))
                ((:terse :report)
                 (format stream " -(~s)" (test-unit-name unit))
                 (finish-output stream))
                (t
                 (warn "invalid test unit mode: ~s." *test-unit-mode*)))
              (test-unit-status unit))))))

(defmacro dsu:time-and-memory (form)
  "this is a placeholder for an operator which captures time and memory usage for a form."
  ;; see profiler.lisp
  `(values (multiple-value-list ,form) 0 0))

(defmacro test-and (&rest forms)
  "perform a conditional conjunction, but signal an error if a clause fails."
  (if forms
    (destructuring-bind (first . rest) forms
      `(if ,first
         (test-and ,@rest)
         (values nil ',first)))
    t))

(defmacro test:ignored-error (&rest forms)
  `(nth-value 1 (ignore-errors ,@forms)))

#|
(deftests
  (context1
   (test1 :form (list 1 2) :value '(1 2))
   (test2 :form (symbol-name '|asdf|) :value "asdf"))
  (fails
   (test2 :form (symbol-name 'asdf) :value "asdfq" :prerequisites ("context1:test2"))))
;(trace unit-path-match-p find-tests wild-unit-path-p test-unit-path execute-test) (untrace)
(find-tests '(context1 :wild-inferiors))
(execute-tests '(context1 :wild-inferiors))
(execute-tests "FAILS:TEST2")
(execute-test "fails:test2" :prerequisites nil)
(execute-test "fails:test2" :prerequisites :force)
(clear-tests )
|#


:de.setf.utility
