;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;;  This file is is a constituent of the 'de.setf.utility' library component.
;;;  It adds support for hierarchical systems names to ASDF.
;;;  (c) 2010 james anderson
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
;;; + locating components in hierarchic libraries 
;;; + library system definitions with nicknames
;;; 2009-06-13  janderson  reimplemented to not use specialized classes, but
;;; instead to use component properties and augment, and/or replace methods
;;; 2009-10-01  janderson  compute nicknames as defaults; canonicalize system
;;; pathnames relative to known logical hosts
;;; 2010-01-10  janderson  separate extensions topically and add to asdf.asd
;;; 2010-02-02  janderson  updated to use (possible null) system-source-file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(asdf::component-description
            asdf::component-long-description
            asdf::sysdef-hierarchical-search-function
            asdf::system-nicknames
            asdf::system-qualified-component-name)
          :asdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; content
;;;
;;; operators to support hierarchical system names and logical pathnames
;;;  sysdef-hierarchical-search-function
;;;  system-nicknames
;;;  system-qualified-component-name
;;;
;;; additional properties
;;;  component-description
;;;  component-long-description
;;;
;;; additions to instantiation steps to support the above
;;;  shared-initialize :before (system t)
;;;  shared-initialize :after (system t)

(defparameter *sysdef-hierarchical-search-function.verbose* nil ; t 
  "when true, print the results for the hierarchical search to *verbose-out*")

(pushnew 'asdf::sysdef-hierarchical-search-function asdf:*system-definition-search-functions*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; operators to support hierarchical system names and logical pathnames

(defun system-namestring-to-list (name)
  (setf name (string name))
  (do* ((start-dot 0                         (1+ end-dot))
        (end-dot (position #\. name)         (position #\. name :start start-dot))
        (list (list (subseq name 0 end-dot)) (cons (subseq name start-dot end-dot) list)))
       ((null end-dot) (nreverse list))))
;(mapcar #'system-namestring-to-list '("" "a" "a.s" "a.d.f"))

(defun system-list-to-namestring (path)
  ;; nb. at least in ccl, the null destination is faster than a fill buffer
  (format nil "~{~a~^.~}" path))

(defun map-registry-roots (function)
  (dolist (root asdf:*central-registry*)
    (funcall function (eval root))))

(defgeneric asdf::sysdef-hierarchical-search-function (name &key verbose-p wild-p)
  (:method ((datum t)  &key verbose-p wild-p)
    (declare (ignore verbose-p wild-p))
    nil)

  (:method ((name string) &key (verbose-p *sysdef-hierarchical-search-function.verbose*) (wild-p nil))
    "given a possibly hierarchical NAME,
 - decimate it to create a path relative to the central registry roots.
 - look first for an .asd in an eponymic leaf directory given that path
   relative to some root.
 - if that fails, look for any file ;<path butlast>;<name>*;**;<name>.asd"
    (let* ((tokens (system-namestring-to-list name))
           (name (first (last tokens))))
      (flet ((make-wild-directory-component (name)
               #+ccl (concatenate 'string name "*")
               #+sbcl (sb-impl::make-pattern `(,name :MULTI-CHAR-WILD))
               #-(or ccl sbcl)  name))
        (map-registry-roots #'(lambda (root-path)
                                (when (typep root-path 'logical-pathname)
                                  (setf root-path (translate-logical-pathname root-path)))
                                (flet ((explicit-file-name ()
                                         (merge-pathnames
                                          (make-pathname  :directory `(:relative ,@tokens)
                                                          :name name
                                                          :type "asd")
                                          root-path))
                                       (versioned-file-pattern ()
                                         ;; add a wildcard to the end the final path element to permit
                                         ;; versioned directories release tar files.
                                         (merge-pathnames
                                          (make-pathname  :directory `(:relative ,@(butlast tokens)
                                                                                 ,(make-wild-directory-component
                                                                                   (first (last tokens)))
                                                                                 ,@(when wild-p '(:wild-inferiors)))
                                                          :name name
                                                          :type "asd")
                                          root-path))
                                       (wild-file-names (wild-file-pattern)
                                         #+ccl (directory wild-file-pattern :directories nil :files t)
                                         #+lispworks (directory wild-file-pattern :directories nil)
                                         #+sbcl  (directory wild-file-pattern)))
                                  (let* ((explicit-file-name (explicit-file-name))
                                         (versioned-file-names nil)
                                         (result (or (probe-file explicit-file-name)
                                                     (first (setf versioned-file-names
                                                                  (sort (wild-file-names (versioned-file-pattern))
                                                                        #'> :key #'file-write-date))))))
                                    (when (and verbose-p asdf::*verbose-out*)
                                      (format asdf::*verbose-out* "~&asdf search: [ ~s ] [ ~s, ~s ] -> ~s"
                                              root-path
                                              explicit-file-name versioned-file-names
                                              result))
                                    (when result
                                      (return-from asdf::sysdef-hierarchical-search-function result))))))))))


(defgeneric asdf::system-nicknames (component)
  (:documentation
   "add nicknames to the base system behaviour.
 the description slots should merge with the base slots, adding only the defaults.")
  (:method ((component asdf::system))
    (asdf::component-property component 'asdf::system-nicknames)))

(defgeneric (setf asdf::system-nicknames) (nicknames component)
  (:method (nicknames (component asdf::system))
    (setf (asdf::component-property component 'asdf::system-nicknames) nicknames)))


(defgeneric asdf::system-qualified-component-name (system)
  (:documentation
   "return the fully qualified identifier for an instantiated system.
 SYSTEM : system-designator
 VALUE  : string : the qualified name
 Resolve the designator to a system, retrieve the relative pathname, which
 (for a system) should be absolute, extract the shortest path relative to some
 registry root, replace the possibly versioned leaf directory and catenate
 path components.")
  
  (:method ((designator string))
    (asdf::system-qualified-component-name (asdf:find-system designator)))
  (:method ((designator symbol))
    (asdf::system-qualified-component-name (asdf:find-system designator)))
  
  (:method ((pathname pathname))
    "given a pathname, return the minimal path as an intermediate result.
      determine which root governs the pathname, compute the relative path
      and consolodate that into a namestring."
    (when (typep pathname 'logical-pathname)
      (setf pathname (translate-logical-pathname pathname)))
    (let ((pathname-directory (pathname-directory pathname))
          (candidate nil))
      (map-registry-roots
       #'(lambda (root-path)
           (when (typep root-path 'logical-pathname)
             (setf root-path (translate-logical-pathname root-path)))
           (let ((root-directory (pathname-directory root-path)))
             (when (and (> (length root-directory) 1)
                        (< (length root-directory) (length pathname-directory))
                        (equalp (subseq pathname-directory 0 (length root-directory))
                                root-directory))
               (let ((root-relative (last pathname-directory (- (length pathname-directory) (length root-directory)))))
                 (when (or (null candidate) (< (length root-relative) (length candidate)))
                   (setf candidate root-relative)))))))
      candidate))
  
  (:method ((system asdf:system))
    (let* ((system-name (asdf::component-name system))
           (pathname (or (asdf::system-source-file system)
                         (make-pathname :name (subseq system-name (1+ (or (position #\. system-name :from-end t) -1)))
                                        :type "asd"
                                        :defaults (asdf::component-relative-pathname system))))
           (translated-pathname (when pathname (translate-logical-pathname pathname)))
           (pathname-name-list (when translated-pathname (asdf::system-qualified-component-name translated-pathname)))
           (system-name-list (system-namestring-to-list system-name)))
      ;; (print (list :pathname-name-list pathname-name-list :system-name-list system-name-list))
      (format nil "~(~{~a.~}~{~a~^.~}~)"
              (butlast pathname-name-list (length system-name-list))
              system-name-list))))

#+(or)
(maphash #'(lambda (n s) n
            (destructuring-bind (time . system) s
              (asdf::%set-system-source-file nil system)
              (print (list time
                           (asdf::component-relative-pathname system)
                           (asdf::component-name system)
                           (asdf::system-qualified-component-name system)))))
         asdf::*defined-systems*)
#+(or)
(asdf::system-qualified-component-name :de.setf.amqp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; additional properties

(defgeneric asdf::component-description (component)
  (:method ((component asdf::component))
    (asdf::component-property component 'asdf::description)))

(defgeneric (setf asdf::component-description) (description component)
  (:method (description (component asdf::component))
    (setf (asdf::component-property component 'asdf::description) description)))

(defgeneric asdf::component-long-description (component)
  (:method ((component asdf::component))
    (asdf::component-property component 'asdf::long-description)))

(defgeneric (setf asdf::component-long-description) (description component)
  (:method (description (component asdf::component))
    (setf (asdf::component-property component 'asdf::long-description) description)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; additions to instantiation steps to support the above

(defmethod shared-initialize :before ((instance asdf::system) (slots t) &key)
  (when (slot-boundp instance 'asdf::name)
    (setf (asdf::find-system (asdf:component-name instance)) nil))
  (when (slot-boundp instance 'asdf::properties)
    (dolist (nick (asdf::system-nicknames instance))
      (setf (asdf::find-system nick) nil))))

(defmethod shared-initialize :after ((instance asdf:component) (slots t) &key
                                     (description nil description-p)
                                     (long-description nil long-description-p)
                                     (contingent-on nil contingent-on-p))
  (when description-p (setf (asdf::component-description instance) description))
  (when long-description-p (setf (asdf::component-long-description instance) long-description))
  (when contingent-on-p (setf (asdf::component-contingent-on instance) contingent-on)))

(defmethod shared-initialize :after ((instance asdf:system) (slots t) &key
                                     (nicknames nil nicknames-p))
  ;; if a relative pathname was supplied, canonicalize it
  (when (slot-boundp instance 'asdf::relative-pathname)
    (etypecase (slot-value instance 'asdf::relative-pathname)
      (null nil)
      (logical-pathname nil)
      (pathname (let ((logical (de.setf.utility.implementation::translate-physical-pathname
                                (slot-value instance 'asdf::relative-pathname))))
                  (when logical
                    (setf (slot-value instance 'asdf::relative-pathname)
                          logical))))))
  ;; default nicknames to the qualified component name
  (cond (nicknames-p
         (setf (asdf::system-nicknames instance) nicknames))
        ((and (null nicknames-p)
              ; must test the binding as instantiation is split into two phases !?
              ; first make-, then reinitialize-
              (slot-boundp instance 'asdf::relative-pathname)
              (asdf::component-relative-pathname instance))
         (setf (asdf::system-nicknames instance)
               (list (asdf::system-qualified-component-name instance)))))
  
  ;; use register-system for its logging
  (when (slot-boundp instance 'asdf::name)
    (asdf::register-system (asdf:component-name instance) instance))
  (dolist (nick (asdf::system-nicknames instance))
    (asdf::register-system nick instance)))
