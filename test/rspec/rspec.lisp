;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;  This file is part of the 'de.setf.utility' library component.
;;;  It implementes a simple test framework

;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;;  a stream-based proxied interface to rspec.
;;;  - adds a ruby code-generation operator which emits stubs for each test to
;;;    effect the test from a ruby process
;;;  - defines are bert-encoded call - response/error protocol to invoke the tests
;;;    and return the results through a stream interface


(defparameter +rspec-undesignated+ 0)
(defparameter +rspec-no-such-module+ 1)
(defparameter +rspec-no-such-function+ 2)
(defparameter +rspec-unable-to-read-header+ 1)
(defparameter +rspec-unable-to-read-data+ 2)

(defun rspec-run-test (path &key debug)
  (let ((test-unit (find-test path)))
    (cond (test-unit
           (handler-case
             (let ((values (multiple-value-list (funcall (test-unit-function test-unit)))))
               (vector :|reply| (vector (if (apply (test-unit-predicate-function test-unit) values)
                                          etf:true etf:false)
                                        values)))
             (error (condition)
                    (let ((message (format nil "test ~s signaled:~%~a"
                                           (test-unit-name test-unit) condition)))
                      (when debug
                        (with-open-file (console "/dev/console" :direction :output :if-exists :append)
                          (multiple-value-bind (sec min hr day mon year) (get-decoded-time)
                            (format console "~%~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d rspec : ~s"
                                    year mon day hr min sec message))))
                      (vector :|error| (vector :|user| 100
                                               (symbol-name (type-of condition))
                                               message
                                               nil))))))
          (t
           (vector :|error| (vector :|server| +rspec-no-such-function+
                                    "UNDEFINED-FUNCTION"
                                    (format nil "Test not found: ~a" path)
                                    nil))))))

(defun rspec-apply-function (package-name symbol-name arguments &key debug)
  (let ((package nil)
        (symbol nil)
        (function nil))
    (cond ((and (setf package (find-package (cons-symbol :keyword package-name)))
                (setf symbol (cons-symbol package symbol-name))
                (setf function (when (fboundp symbol) (fdefinition symbol))))
           (handler-case
             (vector :|reply| (multiple-value-list (apply function arguments)))
             (error (condition)
                    (let ((message (format nil "function ~s signaled:~%~a"
                                           symbol condition)))
                      (when debug
                        (with-open-file (console "/dev/console" :direction :output :if-exists :append)
                          (multiple-value-bind (sec min hr day mon year) (get-decoded-time)
                            (format console "~%~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d rspec : ~s"
                                    year mon day hr min sec message))))
                      (vector :|error| (vector :|user| 100
                                               (symbol-name (type-of condition))
                                               message
                                               nil))))))
          (t
           (vector :|error| (vector :|server| +rspec-no-such-function+
                                    "UNDEFINED-FUNCTION"
                                    (format nil "Function not found: ~a:~a" package-name symbol-name)
                                    nil))))))


;;; (test:test rspec.1 (+ 1 2) 3)
;;; (rspec-run-test "rspec.1")

(defun rspec-run (input output)
  (handler-case
    (loop
      (unless (listen input) (return))
      (let ((request (etf:decode-term input)) (response nil))
        (with-open-file (console "/dev/console" :direction :output :if-exists :append)
          (multiple-value-bind (sec min hr day mon year) (get-decoded-time)
            (format console "~%~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d rspec -> ~s"
                    year mon day hr min sec request)))
        (cond ((and (vectorp request)
                    (symbolp (aref request 0))
                    (string-equal (aref request 0) "call")
                    (stringp (aref request 1)))
               (setf response (if (string-equal (aref request 1) "keyword")
                                (rspec-run-test (elt request 2))
                                (rspec-apply-function (aref request 1) (aref request 2)
                                                      (aref request 3)))))
              (t (setf response (vector :|error| (vector :|protocol| +rspec-unable-to-read-header+
                                                       "PROTOCOL-ERROR"
                                                       (format nil "Invalid request: ~s." request)
                                                       nil)))))
        (with-open-file (console "/dev/console" :direction :output :if-exists :append)
          (multiple-value-bind (sec min hr day mon year) (get-decoded-time)
            (format console "~%~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d rspec <- ~s"
                    year mon day hr min sec response)))
        (etf:encode-term response output)))
    (error (c)
           (with-open-file (console "/dev/console" :direction :output :if-exists :append)
             (multiple-value-bind (sec min hr day mon year) (get-decoded-time)
               (format console "~%~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d rspec : ~a"
                       year mon day hr min sec c))))))

#+sbcl
(defun cl-user::rspec-repl ()
  (let ((input (sb-sys::make-fd-stream 0 :element-type '(unsigned-byte 8) :input t))
        (output (sb-sys::make-fd-stream 1 :element-type '(unsigned-byte 8) :output t)))
    (de.setf.utility.implementation::rspec-run input output)
    (finish-output output)))


#+(or)
(test:test rspec.succeed
  (+ 1 2)
  3)

#+(or)
(test:test rspec.fail
  (+ 1 2)
  0)

#+(or)
(test:test rspec.error
  (/ 1 a)
 t)