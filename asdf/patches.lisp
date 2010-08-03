;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: asdf; -*-

;;;  This file is is a constituent of the 'de.setf.utility' library component.
;;;  It contains patches for ASDF.
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
;;;  along with 'de.setf.utility'.  If not, see the GNU [site](http://www.gnu.org/licenses/).

;;;
;;; 2009-00-00  janderson  patches as they appeared

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; content
;;;
;;; patches to asdf operators
;;;

;;; relative pathname computation
;;; at least in clozure, it requires the null directory to avoid
;;; being created with (:absolute) and thereby  intefereing with any merge.

(defun merge-component-relative-pathname (pathname name type)
  (multiple-value-bind (relative path filename)
      (split-path-string name)
  (merge-pathnames
   (or pathname (make-pathname :directory `(,relative ,@path)))
   (if type
       (make-pathname :directory nil :name filename :type type)
       filename))))

;;;
;;; correct setf to return the passed value

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (component-properties c) :test #'equal)))
    (cond (a
           (setf (cdr a) new-value))
          (t
           (setf (component-properties c)
                 (acons property new-value (slot-value c 'properties)))
           new-value))))
