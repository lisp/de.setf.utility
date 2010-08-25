;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' library component.
;;;  (c) 2002, 2009 james anderson
;;;
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

;;; tests for test coverage monitoring

(in-package :de.setf.utility.implementation)

(defpackage :monitor-test-package
  (:nicknames :mtp)
  (:export :function-to-monitor
           :function-to-call
           :generic-function-to-monitor))

(defun mtp:function-to-monitor (arg)
  (cons arg arg))

(defun mtp::function-to-call (arg1 arg2)
  (cons arg1 arg2))

(defgeneric mtp:generic-function-to-monitor (arg)
  (:method ((arg t)) (cons t arg))
  (:method ((arg string)) (cons (mtp::function-to-call 'string arg) (call-next-method)))
  (:method ((arg number)) (cons (mtp::function-to-call 'number arg) (call-next-method)))
  (:method :around ((arg integer))
           (cons (cons (cons :around 'integer) arg) (call-next-method)))
  (:method :before ((arg cons))
           (if (eq (first arg) :before)
             (mtp::function-to-call (first arg) (rest arg))
             (setf (first arg) :before))))

(dsu:test monitor.1
  "test that monitoring succeeds"
  (progn (clrhash *monitor-registry*)
         (monitor :mtp)
         (dsu:test-and
          (eql (hash-table-count *monitor-registry*) 8)
          (find-monitor 'mtp:function-to-monitor)
          (find-monitor 'mtp:generic-function-to-monitor)
          (find-monitor '(:method mtp:generic-function-to-monitor (t)))
          (find-monitor '(:method mtp:generic-function-to-monitor (string)))
          (find-monitor '(:method mtp:generic-function-to-monitor (number)))
          (find-monitor '(:method mtp:generic-function-to-monitor :around (integer)))
          (find-monitor '(:method mtp:generic-function-to-monitor :before (cons))))))

(dsu:test monitor.2
  "test that monitoring succeeds"
  (progn (initialize-monitor :mtp)
         (dsu:test-and
          (equal (mtp:function-to-monitor 1) '(1 . 1))
          (equal (mtp:generic-function-to-monitor 1) '(((:AROUND . INTEGER) . 1) (NUMBER . 1) T . 1))
          (equal (mtp:generic-function-to-monitor "one") '((STRING . "one") T . "one"))
          (equal (mtp:generic-function-to-monitor t) '(t . t))
          (equal (mtp:generic-function-to-monitor 1.0) '((NUMBER . 1.0) T . 1.0))
          (equal (mtp:generic-function-to-monitor '(nil . 1)) '(T :BEFORE . 1))

          (monitor-called-p 'mtp:function-to-monitor)
          (monitor-called-p 'mtp:generic-function-to-monitor)
          ;; test the calls-called-p for the method with a conditional call
          (equalp (monitor-calls-called-p (find-monitor '(:method mtp:generic-function-to-monitor :before (cons))))
                 #(NIL))
          (equal (mtp:generic-function-to-monitor '(:before . 1)) '(T :BEFORE . 1))
          (equalp (monitor-calls-called-p (find-monitor '(:method mtp:generic-function-to-monitor :before (cons))))
                 #(t))))


#|
(with-open-file (stream "LIBRARY:test.html" :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
  (report-monitor *monitor-registry* stream mime:text/html))


#|
