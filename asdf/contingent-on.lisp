;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;;  This file is is a constituent of the 'de.setf.utility' library component.
;;;  It adds the 'contingent-on' relation to ASDF
;;;  (c) 2009 james anderson
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

;;;
;;; 2009-02-20  janderson  additions to asdf to support
;;; + component contingency distinct from dependency
;;; 2009-06-13  janderson  reimplemented to not use specialized classes, but
;;; instead to use component properties and augment, and/or replace methods
;;; 2010-01-10  janderson  separate extensions topically and add to asdf.asd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(asdf::component-contingent-on)
          :asdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; content :
;;;
;;; additional methods to implement contingency
;;;  component-continget-on
;;;  traverse :around
;;;
;;; additional properties
;;;  component-description
;;;  component-long-description
;;;
;;; additions to instantiation steps to support the above
;;;  shared-initialize :before (system t)
;;;  shared-initialize :after (system t)

(defparameter  asdf::*traverse-verbose* nil)    ; help debugging contingency



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; additional methods to implement contingency

(defgeneric asdf::component-contingent-on (component)
  (:documentation
   "use when collecting the traversal results. iff the component's contingencies
 are met - that is, the systems are known and the features are present, then the
 component is included in traversal results. otherwise it is invisible.")
  (:method ((component asdf::component))
    (asdf::component-property component 'asdf::contingent-on)))

(defgeneric (setf asdf::component-contingent-on) (description component)
  (:method (contingencies (component asdf::component))
    (setf (asdf::component-property component 'asdf::contingent-on) contingencies)))


(defmethod asdf::traverse :around ((operation asdf:operation) (module asdf::component))
  "specialize the behavior to suppress traversal unless declared contingencies
 are satisfied. if they are not, return (), which effectively prunes the
 component with respect to the operation."

  (dolist (contingency (asdf::component-contingent-on module))
    (destructuring-bind (contingent-op . requirements) contingency
      (when (typep operation contingent-op)
        (dolist (requirement requirements)
          (unless (etypecase requirement
                    ((or string symbol)
                     (asdf:find-system requirement nil))
                    (cons
                     (destructuring-bind (predicate value) requirement
                       (cond ((string-equal predicate "feature")
                              (find value *features* :test #'string-equal))
                             (t
                              (error "invalid contingency predicate: ~s" predicate))))))
            (when asdf::*traverse-verbose*
              (format *trace-output* "~&contingency not satisfied: ~s.~s, ~s"
                      (asdf:component-parent module) module (asdf::component-contingent-on module)))
            (return-from asdf::traverse nil))))))
  ;; if all contingencies apply
  (when asdf::*traverse-verbose*
    (format *trace-output* "~&contingency satisfied: ~s.~s, ~s"
            (asdf:component-parent module) module (asdf::component-contingent-on module))
    (when (equalp (asdf:component-name module) "fftw3-digitool") (break)))
  (call-next-method))






#|
(asdf:defsystem :de.setf.test.test
  :class asdf:system
  :components ((:contingent-module :m1
                ; :contingent-on ((explain-op :de.setf.test.none))
                :contingent-on ((explain-op (asdf:feature :test)))
                :components ((:file "file1") (:file "file2")))
               (:module :m2
                :components ((:file "file3") (:file "file4")))))
(defclass explain-op (asdf:operation) ())
(defmethod asdf:perform ((op explain-op) (component asdf:component))
  (asdf:explain op component))

(asdf:operate 'explain-op :de.setf.test.test)
(let ((*features* (cons :test *features*))) (asdf:operate 'explain-op :de.setf.test.test))


|#

