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
;;;  along with 'de.setf.utility'.  If not, see the GNU [site](http://www.gnu.org/licenses/).

;;;
;;; 2009-02-20  janderson  additions to asdf to support
;;;  + locating components in hierarchic libraries 
;;;  + library system definitions with nicknames
;;; 2009-06-13  janderson  reimplemented to not use specialized classes, but
;;;  instead to use component properties and augment, and/or replace methods
;;; 2009-10-01  janderson  compute nicknames as defaults; canonicalize system
;;;  pathnames relative to known logical hosts
;;; 2010-01-10  janderson  separate extensions topically and add to asdf.asd
;;; 2010-02-02  janderson  updated to use (possible null) system-source-file
;;; 2010-03-18  janderson  adjusted sysdef-hierarchical-search-function to look for -test systems in the
;;;  respective base system's directory
;;; 2040-04-05 janderson : cmucl acts similar to sbcl
;;;  (see http://common-lisp.net/project/cmucl/doc/cmu-user/extensions.html#toc50)

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (intersection '(:allegro :ccl :clisp :cmu :sbcl :ecl :lispworks) *features*)
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(asdf::component-description
            asdf::component-long-description
            asdf::sysdef-hierarchical-search-function
            asdf::system-nicknames
            asdf::system-qualified-component-name)
          :asdf))


;;; ensure support for de.setf.utility.implementation::translate-physical-pathname

(eval-when (:execute :load-toplevel :compile-toplevel)
  (unless (and (find-package :de.setf.utility.implementation)
               (find-symbol (string :translate-physical-pathname)
                            :de.setf.utility.implementation))
    (load (merge-pathnames (make-pathname :directory '(:relative :up) :name "pathnames")
                           *load-pathname*))))


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

(defvar *sysdef-hierarchical-search-function.verbose* nil ; t 
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
  (flet ((directory-pathname-p (path)
           (let ((name (pathname-name path))(type (pathname-type path)))
             (and  (or (null name) (eq name :unspecific) (zerop (length name)))
                   (or (null type) (eq type :unspecific))))))
    (dolist (root asdf:*central-registry*)
      (when (and (pathnamep (setf root (eval root)))
                 (directory-pathname-p root))
        (funcall function (eval root))))))

(defgeneric asdf::sysdef-hierarchical-search-function (name &key verbose-p wild-p)
  (:method ((datum t)  &key verbose-p wild-p)
    (declare (ignore verbose-p wild-p))
    nil)

  (:method ((name symbol) &rest args)
    (when name (apply #'asdf::sysdef-hierarchical-search-function (string-downcase name) args)))

  (:method ((name string) &key (verbose-p *sysdef-hierarchical-search-function.verbose*) (wild-p nil))
    "given a possibly hierarchical NAME,
 - decimate it to create a path relative to the central registry roots.
 - look first for an .asd in an eponymic leaf directory given that path
   relative to some root.
 - if that fails, look for any file ;<path butlast>;<name>*;**;<name>.asd"
    (let* ((tokens (system-namestring-to-list name))
           (name (first (last tokens)))
           (directory-name (if (eql (search "-test" name) (- (length name) 5))
                             (subseq name 0 (- (length name) 5))
                             name)))
      (flet ((make-wild-directory-component (name)
               :wild                    ; the default for allegro, cmucl, ecl
               #+(or ccl clisp ecl lispworks) (concatenate 'string name "*")
               #+cmu (lisp::make-pattern `(,name :MULTI-CHAR-WILD))
               #+sbcl (sb-impl::make-pattern `(,name :MULTI-CHAR-WILD)))
             (ensure-pathname-directory (pathname)
               (or (pathname-directory pathname) '(:absolute))))
        (map-registry-roots #'(lambda (root-path)
                                (when (typep root-path 'logical-pathname)
                                  (setf root-path (translate-logical-pathname root-path)))
                                (when (and verbose-p asdf::*verbose-out*)
                                  (format asdf::*verbose-out* "~&asdf >search: [ ~s ] ... " root-path))
                                (labels ((direct-file-name ()
                                         ;; look for it immediately in the registry
                                         (make-pathname :name name :type "asd" :defaults root-path))
                                       (indirect-file-name ()
                                         ;; look for it at a path implicit in the name
                                         (let ((wild-indirect-file-name
                                                (make-pathname  :directory (append (ensure-pathname-directory root-path)
                                                                                   `(,@(butlast tokens) ,directory-name)
                                                                                   '(:wild-inferiors))
                                                                :name name :type "asd"
                                                                :defaults root-path)))
                                           (when (and verbose-p asdf::*verbose-out*)
                                             (format asdf::*verbose-out* "~&... wild-indirect-file-name: ~s" wild-indirect-file-name))
                                           (first (sort (wild-file-names wild-indirect-file-name) #'> :key #'file-write-date))))
                                       (versioned-file-pattern ()
                                         ;; add a wildcard to the end the final path element to permit
                                         ;; versioned directories release tar files.
                                         (make-pathname  :directory (append (ensure-pathname-directory root-path)
                                                                            `(,@(butlast tokens)
                                                                              ,(make-wild-directory-component directory-name)
                                                                              ,@(when wild-p '(:wild-inferiors))))
                                                         :name name :type "asd"
                                                         :defaults root-path))
                                       (wild-file-names (wild-file-pattern)
                                         #+ccl (directory wild-file-pattern :directories nil :files t)
                                         #+lispworks (directory wild-file-pattern :directories nil)
                                         #+(or sbcl allegro) (directory wild-file-pattern)))
                                  (let* ((direct-file-name (direct-file-name))
                                         (indirect-file-name (indirect-file-name))
                                         (versioned-file-names nil)
                                         (result (or (probe-file direct-file-name)
                                                     indirect-file-name
                                                     ;; if there are multiple versions, take the one with the
                                                     ;; most recent write time rather than just the higher version
                                                     (first (setf versioned-file-names
                                                                  (sort (wild-file-names (versioned-file-pattern))
                                                                        #'> :key #'file-write-date))))))
                                    (when (and verbose-p asdf::*verbose-out*)
                                      (format asdf::*verbose-out* "~&... direct: ~s -> ~s"
                                              direct-file-name (and direct-file-name (probe-file direct-file-name)))
                                      (format asdf::*verbose-out* "~&... indirect: ~s -> ~s"
                                              indirect-file-name (and indirect-file-name (probe-file indirect-file-name)))
                                      (format asdf::*verbose-out* "~&... versioned: ~s -> ~s"
                                              (versioned-file-pattern) versioned-file-names )
                                      (format asdf::*verbose-out* "~&... -> ~s"
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


(defgeneric pathname-qualified-path (pathname root-path)
  (:method ((pathname pathname) (root-path pathname))
    "Iff the pathname is dominated by the root path, return the relative directory path."
    
    (when (typep pathname 'logical-pathname)
      (setf pathname (translate-logical-pathname pathname)))
    (when (typep root-path 'logical-pathname)
      (setf root-path (translate-logical-pathname root-path)))
    
    (let ((pathname-directory (pathname-directory pathname))
          (root-directory (pathname-directory root-path)))
      (when (and (> (length root-directory) 1)
                 (< (length root-directory) (length pathname-directory))
                 (equalp (subseq pathname-directory 0 (length root-directory))
                         root-directory))
        (last pathname-directory (- (length pathname-directory) (length root-directory)))))))


(assert (and (equal '("d1" "d2") (pathname-qualified-path (make-pathname :directory '(:absolute "base" "d1" "d2"))
                                                          (make-pathname :directory '(:absolute "base"))))
             (null (pathname-qualified-path (make-pathname :directory '(:absolute "base" "d1" "d2"))
                                                          (make-pathname :directory '(:absolute "x"))))
             (null (pathname-qualified-path (make-pathname :directory '(:absolute "base" "d1" "d2"))
                                                          (make-pathname :directory '(:absolute))))))


(defun registry-qualified-path (pathname)
  (let ((result nil))
    (map-registry-roots #'(lambda (root-path)
                            ;; determine which root governs the pathname, compute the relative path
                            ;; and consolodate that into a namestring.
                            (let ((candidate (pathname-qualified-path pathname root-path)))
                              (when (> (length candidate) (length result))
                                (setf result candidate)))))
    result))

(assert (let ((asdf::*central-registry* (list (make-pathname :directory '(:absolute "base")))))
          (equal '("d1" "d2") (registry-qualified-path (make-pathname :directory '(:absolute "base" "d1" "d2"))))))


(defgeneric pathname-qualified-component-name (system-name pathname)
  (:documentation "Given a system name or a pathname and a root pathname, return the minimal path as an relative
 to the root.")
  
  (:method ((designator symbol) (pathname t))
    (pathname-qualified-component-name (string designator) pathname))

  (:method ((system-name string) (pathname pathname))
    (when (typep pathname 'logical-pathname)
      (setf pathname (translate-logical-pathname pathname)))
    (let* ((system-name-list (system-namestring-to-list system-name))
           (pathname-name-list (registry-qualified-path pathname))
           (system-name-position (search system-name-list  pathname-name-list :test
                                         #'(lambda (sn pn)
                                             (and (<= (length sn) (length pn))
                                                  (string-equal sn pn :end2 (length sn)))))))
      #+(or )
      (print (list :system-name system-name
                   :pathname-name-list pathname-name-list :system-name-list system-name-list
                   :system-name-position system-name-position))
      (format nil "~(~{~a.~}~{~a~^.~}~)"
              ;; (butlast pathname-name-list (length system-name-list))
              (if system-name-position
                (butlast pathname-name-list (- (length pathname-name-list) system-name-position))
                pathname-name-list)
              system-name-list))))


(defgeneric asdf::system-qualified-component-name (system)
  (:documentation
   "return the fully qualified identifier for an instantiated system.
 SYSTEM : system-designator
 VALUE  : string : the qualified name
 Resolve the designator to a system, retrieve the relative pathname, which
 (for a system) should be absolute, extract the shortest path relative to some
 registry root, replace the possibly versioned leaf directory and catenate
 path components.")
  
  (:method ((system asdf:system))
    "Given a system, compute the qualified name based on the system's source file, or the presumed location
 with respect to its relative pathname."
    (let ((system-name (asdf::component-name system)))
      (pathname-qualified-component-name system-name
                                         (or (asdf::system-source-file system)
                                             (make-pathname :name (subseq system-name
                                                                          (1+ (or (position #\. system-name :from-end t) -1)))
                                                            :type "asd"
                                                            :defaults (asdf::component-relative-pathname system))))))

  (:method ((designator symbol))
    (asdf::system-qualified-component-name (asdf:find-system designator)))

  (:method ((designator string))
    (asdf::system-qualified-component-name (asdf:find-system designator))))


#+(or)
(maphash #'(lambda (n s)
            (destructuring-bind (time . system) s
              (asdf::%set-system-source-file nil system)
              (print (list (cons time n)
                           (asdf::component-relative-pathname system)
                           (asdf::component-name system)
                           (asdf::system-qualified-component-name system)))))
         asdf::*defined-systems*)
#+(or)
(asdf::system-qualified-component-name :de.setf.amqp)


(assert (let ((asdf::*central-registry* (list (make-pathname :directory '(:absolute "base")))))
          (and (equal "d1.d2" (pathname-qualified-component-name "d2" (make-pathname :directory '(:absolute "base" "d1" "d2"))))
               (equal "d1.d2" (pathname-qualified-component-name "d2" (make-pathname :directory '(:absolute "base" "d1" "d2-v-0") :name "d2" :type "asd")))
               (equal "d1.d2" (pathname-qualified-component-name "d1.d2" (make-pathname :directory '(:absolute "base" "d1" "d2-v-0") :name "d2" :type "asd")))
               (equal "d1.d2" (pathname-qualified-component-name "d2" (make-pathname :directory '(:absolute "base" "d1" "d2-v-0" "src"))))
               (equal "d1.d3-v-0.d2" (pathname-qualified-component-name "d2" (make-pathname :directory '(:absolute "base" "d1" "d3-v-0")))))))



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

;;; use primtive interface to allow that the operator extensions are not loaded
(defmethod shared-initialize :before ((instance asdf::system) (slots t) &key)
  (when (slot-boundp instance 'asdf::name)
    (remhash (asdf::coerce-name (asdf:component-name instance)) asdf::*defined-systems*))
  (when (slot-boundp instance 'asdf::properties)
    (let ((time (get-universal-time)))
    (dolist (nick (asdf::system-nicknames instance))
      (setf (gethash (asdf::coerce-name nick) asdf::*defined-systems*)
            (cons time instance))))))

(defmethod shared-initialize :after ((instance asdf:component) (slots t) &key
                                     (description nil description-p)
                                     (long-description nil long-description-p)
                                     (contingent-on nil contingent-on-p))
  (when description-p (setf (asdf::component-description instance) description))
  (when long-description-p (setf (asdf::component-long-description instance) long-description))
  (when contingent-on-p  (setf (asdf::component-property instance 'asdf::contingent-on) contingent-on)))

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


(pushnew :asdf.hierarchical-names *features*)
