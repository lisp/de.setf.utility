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
;;;  along with 'de.setf.utility'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.

;;;
;;; 2010-02-03  janderson  independent file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :asdf)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; content
;;;
;;; additional to asdf operators
;;;  (setf find-system)
;;;  load-op (system &rest args)
;;;  edit (system)
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(edit-op) :asdf))



(defun (setf find-system) (system name)
  (if system
    (setf (gethash (coerce-name name) *defined-systems*) system)
    (remhash (coerce-name name) *defined-systems*))
  system)

(unless (fboundp 'load-op)
  (defun load-op (system &rest args)
    (apply #'operate 'load-op system args)))


(defgeneric edit-op (component)
  (:method ((component t))
    (asdf:edit-op (find-system component)))

  (:method ((system asdf:system))
    (ed (or (system-source-file system)
            (let ((system-name (component-name system)))
              (make-pathname :name (subseq system-name (1+ (or (position #\. system-name :from-end t) -1)))
                             :type "asd"
                             :defaults (component-relative-pathname system))))))
    
  (:method ((file source-file))
    (ed (component-pathname file)))

  (:method ((pathname pathname))
    (ed pathname)))


;;; (edit-op :de.setf.amqp)
