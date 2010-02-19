;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It defines package operators with provisions for side-effects on exxisting packages.

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


;;;
;;; content : several package operators to extend and/or standardize defpackage
;;;
;;;  modpackage (package-designator &rest options)
;;;    A macro to define or modify the designated package. The options include operations with
;;;    side-effects on existing packages.
;;;
;;;  ensure-package (designator &key if-exists if-does-not-exist)
;;;    modify a ackage's existence to create, load, delete based its curent existence and the
;;;    existence arguments
;;;
;;;  modify-package (package &rest options)
;;;    function implementation for the modpackage macro.
;;;
;;; In particular, 
;;; with defined behavior for repeated operations
;;;
;;; as a special-case, this file defines the implementation package and the utility package.
;;; it should be loaded as the first of the utility source files, as they require the implementation package
;;; to exist and use its package definition operator to extend the interface package.

;;;
;;; 20030328 neither openmcl nor allegro5.* support package documentation
;;; 20030412 corrected :export-only, :export-from order to preceed other :export-* options
;;; 20030416 correct load-package to look first for a binary file (still without regard to write date)
;;; 20030416  and added optional separate package file
;;; 20030419 observe exisiting accessibles for :import-from and :export-to, but not :export-from and :export-through
;;; 20030419  signal error/cerror for :shadowing-inport-from shadowing-inport-to
;;; 20030419  stable-sort for operation ordering, with changes to precedence predicate to return nil for self reference
;;; 20030419  explicit :use nil argument to make-package in ensure-package
;;; 20030901 :ensure method to modify-package;
;;; 20030901 replaced package-search-path* with a single *package-host-name* to avoid problems with host-free logical pathnames
;;; 20030901 optional package file is <designator>-package rather than <designator>/package to allow arbitrary content
;;; 20030901 in subdirectories
;;; 20031212.jaa changed package op order to :export-from :export-only since the import must happen first
;;; 20040106.jaa exported *package-host-name*; added package as load-package designator
;;; 20050525.janderson single-case symbol names
;;; 20090303.janderson unified utility packages; import modpackage into cl-user
;;; 20100203.janderson  unified with asdf for loading
;;; 20100210.janderson  initial package definitions (this file and pathnames) in one place


(in-package :de.setf.utility.implementation)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (import '(modpackage) :common-lisp-user)
  (defparameter *package-operations*
    '(:purge  :clear :ensure
      :like :shadow :shadowing-import-from :use-only :use
      :import-only :import-from :intern-only :intern
      :export-from :export-only :unexport :export :alias :nicknames
      :shadowing-import-to :use-by :export-through :version)))

(defmacro defvarconstant (name value &optional documentation)
  (let ((name-var (gensym)))
    `(defconstant ,name
       (let ((,name-var ,value))
         (if (boundp ',name)
           (progn (assert (equalp (symbol-value ',name) ,name-var) ()
                          "Previous value for constant ~a not equal to new binding: ~s."
                          ',name ,name-var)
                  (symbol-value ',name))
           ,name-var))
       ,@(when documentation (list documentation)))))


(defparameter *package-host-name* "library"
  "Used by functions ensure-package and load-package.
   The *package-host-name* variable should be bound to a logical host name which
   designates translations which comprise the possible locations of loadable package implementations.")

(defparameter *binary-type* (pathname-type (compile-file-pathname "file.LISP")))

(define-condition package-not-found (#+(and mcl digitool) ccl::no-such-package warning)
  ((package :initform nil)
   (name :initform nil :initarg :name)
   (pathname :initform nil :initarg :pathname))
  (:report (lambda (condition stream)
             (with-slots (name pathname) condition
               (format stream "package not found: ~a~@[ in pathname: ~s~]." name pathname)))))  

(defun intern-symbol-name (datum in-package)
  (etypecase datum
    (string )
    ((and symbol (not null)) (setf datum (string datum))))
  (intern datum in-package))

(defun find-symbol-name (datum in-package)
  (etypecase datum
    (string )
    ((and symbol (not null)) (setf datum (string datum))))
  (or (find-symbol datum in-package) (error "symbol not found: ~s ~s" datum in-package)))

(defun call-with-symbol-names (function designators)
  (if (listp designators)
    (let* ((length (length designators))
           (names (make-list length)))
      (declare (fixnum length) (dynamic-extent names))
      (flet ((string-first (symbols designators)
               (setf (first symbols) (string (first designators)))))
        (declare (dynamic-extent #'string-first))
        (mapl #'string-first names designators)
        (funcall function names)))
    (funcall function (string designators)))
  (values))

(defun call-with-interned-symbols (function designators in-package)
  (if (listp designators)
    (let* ((length (length designators))
           (symbols (make-list length)))
      (declare (fixnum length) (dynamic-extent symbols))
      (flet ((intern-first (symbols designators)
               (setf (first symbols) (intern-symbol-name (first designators) in-package))))
        (declare (dynamic-extent #'intern-first))
        (mapl #'intern-first symbols designators)
        (funcall function symbols)))
    (funcall function (intern-symbol-name designators in-package)))
  in-package)

(defun call-with-found-symbols (function designators in-package)
  (if (listp designators)
    (let* ((length (length designators))
           (symbols (make-list length)))
      (declare (fixnum length) (dynamic-extent symbols))
      (flet ((find-first (symbols designators)
               (setf (first symbols) (find-symbol-name (first designators) in-package))))
        (declare (dynamic-extent #'find-first))
        (mapl #'find-first symbols designators)
        (funcall function symbols)))
    (funcall function (find-symbol-name designators in-package))))

(defun string-match-p (string pattern)
  #+ccl(ccl::%component-match-p string pattern)
  #-ccl(error "no definition available for string-match-p: ~s: ~s" string pattern))

(defun find-packages (wildname &key (if-does-not-exist nil))
  (or (remove-if (complement #'(lambda (package)
                                 (flet ((test-name (name) (string-match-p name wildname)))
                                   (or (test-name (package-name package))
                                       (dolist (nickname (package-nicknames package))
                                         (when (test-name nickname) (return t)))))))
                 (list-all-packages) :key #'package-name)
      (ecase if-does-not-exist
        ((nil) nil)
        (:error (error 'package-not-found :name wildname :pathname nil)))))

(defgeneric modify-package-operation (package operation arguments)
  (:method ((package t) (operation t) (arguments t))
           (modify-package-operation (ensure-package package :if-does-not-exist :error) operation arguments))

  (:method ((package package) (operation t) (symbol t))
           (modify-package-operation package operation (list symbol)))

  (:method ((package package) (operation t) (symbols list))
           (no-applicable-method #'modify-package-operation package operation symbols))

  (:method ((package package) (operation (eql :clear)) (arguments t))
           (unuse-package (package-use-list package) package)
           (dolist (using-package (package-used-by-list package)) (unuse-package package using-package))
           (do-all-symbols (symbol package) (unintern symbol package)))

  (:method ((package package) (operation (eql :ensure)) (arguments cons))
           (dolist (to-ensure arguments)
             (ensure-package to-ensure  :if-does-not-exist :load)))

  (:method ((package package) (operation (eql :like)) (arguments cons))
           (modify-package-operation package :like (first arguments))
           (when (rest arguments)
             (apply #'modify-package package (rest arguments))))

  (:method ((package package) (operation (eql :like)) (prototype t))
           (modify-package-operation package :like (ensure-package prototype :if-does-not-exist :error)))

  (:method ((package package) (operation (eql :like)) (prototype package))
           (modify-package-operation package :clear nil)
           (when (package-shadowing-symbols prototype)
             (shadowing-import (package-shadowing-symbols prototype) package))
           (dolist (using-package (package-used-by-list prototype)) (use-package package using-package))
           (dolist (used-package (package-use-list prototype)) (use-package used-package package))
           (with-package-iterator (next-symbol prototype :internal :external)
             (loop (multiple-value-bind (more-p symbol accessibility) (next-symbol)
                     (unless more-p (return))
                     (ecase accessibility
                       (:external (import symbol package) (export symbol package))
                       (:internal (import symbol package)))))))

  (:method ((package package) (operation (eql :alias)) (arguments list))
           (flet ((do-alias (names)
                    (unless (listp names) (setf names (list names)))
                    (rename-package package (package-name package) (union names (package-nicknames package) :test #'string=))))
             (declare (dynamic-extent #'do-alias))
             (call-with-symbol-names #'do-alias arguments)))

  (:method ((package package) (operation (eql :nicknames)) (nicknames list))
           (rename-package package (package-name package) (mapcar #'string nicknames)))

  (:method ((package package) (operation (eql :export-only)) (symbols list))
           (flet ((do-export (symbols) (export symbols package)))
             (declare (dynamic-extent #'do-export))
             (do-external-symbols (symbol package)
               (unless (find symbol symbols :test #'string=) (unexport symbol package)))
             (when symbols
               (call-with-interned-symbols #'do-export symbols package))))

  (:method ((package package) (operation (eql :export)) (symbols list))
           (flet ((do-export (symbols) (export symbols package)))
             (declare (dynamic-extent #'do-export))
             (call-with-interned-symbols #'do-export symbols package)))

  (:method ((package package) (operation (eql :unexport)) (symbols list))
           (flet ((do-unexport (symbols) (unexport symbols package)))
             (declare (dynamic-extent #'do-unexport))
             (call-with-interned-symbols #'do-unexport symbols package)))

  (:method ((package package) (operation (eql :intern)) (symbols list))
           (call-with-interned-symbols #'identity symbols package))

  (:method ((package package) (operation (eql :intern-only)) (symbols list))
           (with-package-iterator (next-symbol package :internal)
             (loop (multiple-value-bind (more-p symbol) (next-symbol)
                     (unless more-p (return))
                     (unless (or (find symbol symbols :test #'string=)
                                 (find symbol (package-shadowing-symbols package)))
                       (unintern symbol package))))
             (when symbols
               (call-with-interned-symbols #'identity symbols package))))
  
  (:method ((package package) (operation (eql :import-from)) (source-and-symbols list))
           (flet ((do-import (symbol) (import symbol package)))
             (declare (dynamic-extent #'do-import))
             (let ((source-package (first source-and-symbols))
                   (symbols (rest source-and-symbols)))
               (if symbols
                 (call-with-found-symbols #'do-import symbols source-package)
                 (do-external-symbols (symbol source-package)
                   (unless (find symbol (package-shadowing-symbols package) :test #'string=)
                     (do-import symbol)))))))

  (:method ((package package) (operation (eql :import-only)) (source-and-symbols list))
           (with-package-iterator (next-symbol package :internal)
             (loop (multiple-value-bind (more-p symbol) (next-symbol)
                     (unless more-p (return))
                     (unless (eq (symbol-package symbol) package)
                       (unintern symbol package))))
             (when source-and-symbols
               (modify-package-operation package :import-from source-and-symbols))))

  (:method ((package package) (operation (eql :shadow)) (symbols list))
           (flet ((do-shadow (symbols) (shadow symbols package)))
             (declare (dynamic-extent #'do-shadow))
             (when symbols
               (call-with-interned-symbols #'do-shadow symbols package))))

  (:method ((package package) (operation (eql :shadowing-import-from)) (source-and-symbols list))
           (labels ((do-shadowing-import (symbol &aux accessible)
                      (if (setf accessible (find-symbol (string symbol) package))
                        (if (eq accessible symbol)
                          (unless (find symbol (package-shadowing-symbols package))
                            (shadowing-import symbol package))
                          (if (eq (symbol-package accessible) package)
                            (error "symbol already present in package: ~s: ~s: ~s" symbol accessible package)
                            (progn (cerror "allow new symbol to shadow accessible symbol: ~s: ~s."
                                           (make-condition 'package-error :package package)
                                           symbol accessible)
                                   (shadowing-import symbol package))))
                        (shadowing-import symbol package)))
                    (do-shadowing-import-list (symbols) (dolist (symbol symbols) (do-shadowing-import symbol))))
             (declare (dynamic-extent #'do-shadowing-import-list #'do-shadowing-import))
             (let ((source-package (first source-and-symbols))
                   (symbols (rest source-and-symbols)))
               (if symbols
                 (call-with-interned-symbols #'do-shadowing-import-list symbols source-package)
                 (do-external-symbols (symbol source-package)
                   (do-shadowing-import symbol))))))

  (:method ((package package) (operation (eql :shadowing-import-to)) (destination-and-symbols list))
           (let ((destination-package (first destination-and-symbols))
                 (symbols (rest destination-and-symbols)))
             (labels ((do-shadowing-import (symbol &aux accessible)
                        (if (setf accessible (find-symbol (string symbol) destination-package))
                          (if (eq accessible symbol)
                            (unless (find symbol (package-shadowing-symbols destination-package))
                              (shadowing-import symbol destination-package))
                            (if (eq (symbol-package accessible) destination-package)
                              (error "symbol already present in package: ~s: ~s: ~s" symbol accessible destination-package)
                              (progn (cerror "allow new symbol to shadow accessible symbol: ~s: ~s."
                                             (make-condition 'package-error :package destination-package)
                                             symbol accessible)
                                     (shadowing-import symbol destination-package))))
                          (shadowing-import symbol destination-package)))
                      (do-shadowing-import-list (symbols) (dolist (symbol symbols) (do-shadowing-import symbol))))
               (declare (dynamic-extent #'do-shadowing-import-list #'do-shadowing-import))
               (if symbols
                 (call-with-interned-symbols #'do-shadowing-import-list symbols package)
                 (do-external-symbols (symbol package)
                   (do-shadowing-import symbol))))))

  (:method ((package package) (operation (eql :export-from)) (source-and-symbols list))
           (flet ((do-import-export (symbol) (import symbol package) (export symbol package)))
             (declare (dynamic-extent #'do-import-export))
             (let ((source-package (first source-and-symbols))
                   (symbols (rest source-and-symbols)))
               (if symbols
                 (call-with-interned-symbols #'do-import-export symbols source-package)
                 ;; do all and don't allow for shadowing
                 (do-external-symbols (symbol source-package) (do-import-export symbol))))))

  (:method ((package package) (operation (eql :export-through)) (destination-and-symbols list))
           (let ((destination-package (ensure-package (first destination-and-symbols) :if-does-not-exist :error))
                 (symbols (rest destination-and-symbols)))
             ;; the symbol may be present in the other package or may be new
             (if symbols
               (flet ((do-export-export (symbol) (export symbol package)
                                                 (import symbol destination-package)
                                                 (export symbol destination-package)))
                 (declare (dynamic-extent #'do-export-export))
                 (dolist (symbol symbols)
                   (when (setf symbol (find-symbol (string symbol) destination-package))
                     (import symbol package)))
                 (call-with-interned-symbols #'do-export-export symbols package))
               ;; do all and don't allow for shadowing
               (do-external-symbols (symbol package)
                 (import symbol destination-package) (export symbol destination-package)))))

  (:method ((package package) (operation (eql :use-only)) (packages list))
           (let ((packages-used (package-use-list package)))
             (when packages-used (unuse-package packages-used package)))
           (use-package packages package))

  (:method ((package package) (operation (eql :use)) (packages list))
           (when packages (use-package packages package)))

  (:method ((package package) (operation (eql :use-by)) (packages list))
           (dolist (other-package packages) (use-package package other-package)))

  (:method ((package package) (operation (eql :version)) (version cons))
           (modify-package-operation package operation (first version)))

  (:method ((package package) (operation (eql :version)) (version t))
           (setf (package-version package) version))

  (:method ((package package) (operation (eql :documentation)) (documentation cons))
           #-(or (and allegro allegro-version>= (not (version>= 6 0))) clisp)
           (setf (documentation package t) (first documentation))
           documentation)

  (:method ((package package) (operation (eql :documentation)) (documentation t))
           #-(or (and allegro allegro-version>= (not (version>= 6 0))) clisp)
           (setf (documentation package t) documentation)
           documentation)

  (:method ((package package) (operation (eql :purge)) (argument t))
           (dolist (using-package (package-used-by-list package))
             (unuse-package package using-package))
           (dolist (importing-package (list-all-packages))
             (with-package-iterator (next-symbol importing-package :internal :external)
               (loop (multiple-value-bind (more-p symbol access) (next-symbol)
                       (unless more-p (return))
                       (when (eq (symbol-package symbol) package)
                         (multiple-value-bind (again-symbol access) (find-symbol (symbol-name symbol) package)
                           (when (eq access :external) (unexport again-symbol package)))
                         (unintern symbol package)
                         (when (eq access :external)
                           (unexport symbol importing-package))
                         (unintern symbol importing-package)
                         (import symbol importing-package)
                         (when (eq access :external)
                           (export symbol importing-package)))))))
           (when argument (delete-package package))))



(defgeneric modify-package-operation-preceeds-p (operation1 operation2)
  (:method :before ((op1 (eql :purge)) (op2 t)) (error "package operation permitted alone only: ~s." op1))
  (:method :before ((op2 t) (op1 (eql :purge))) (error "package operation permitted alone only: ~s." op1))
  (:method :before ((op1 (eql :clear)) (op2 t)) (error "package operation permitted alone only: ~s." op1))
  (:method :before ((op2 t) (op1 (eql :clear))) (error "package operation permitted alone only: ~s." op1))
  (:method ((op1 t) (op2 t)) (member op2 (rest (member op1 *package-operations*)))))


#|
(sort '(:documentation  :version :export-only :export-from :export-from :export-from :export-from :export )
      #'modify-package-operation-preceeds-p)
|#

(defgeneric modify-package (package &rest options)
  (:method ((package t) &rest args)
           (apply #'modify-package (ensure-package package :if-does-not-exist :error) args))
  (:method ((package string) &rest args)
           (if (wild-pathname-p package)
             (apply #'modify-package (find-packages package :if-does-not-exist :error) args)
             (call-next-method)))
  (:method ((packages list) &rest args)
           (dolist (package packages)
             (apply #'modify-package package args)))
  (:method ((package package) &rest options)
           (unless (evenp (length options))
             (error "illegal option list: ~s." options))
           (do ((key (pop options) (pop options))
                (arguments (pop options) (pop options)))
               ((null key))
             (modify-package-operation package key arguments))
           package))


(defmacro modPackage (name-and-args &rest options)
  "modify a package as specified. the options include those of defpackage. additional specifications support inter-package operations. if the package exists, it is destructively modified."
  (let ((package-form `(ensure-package ,(string (if (consp name-and-args) (first name-and-args) name-and-args))
                                       ,@(when (consp name-and-args) (rest name-and-args)))))
    (labels ((check-modpackage-option (option)
               (destructuring-bind (operation . arguments) option
                 (unless (compute-applicable-methods #'modify-package-operation (list *package* operation arguments))
                   (error "illegal option: ~s not supported by ~s." (first option) #'modify-package-operation))
                 (case operation
                   (:like 
                    `(:like '(,(string (first arguments))
                              ,@(apply #'append
                                       (stable-sort (mapcar #'check-modpackage-option (rest arguments))
                                                    #'modify-package-operation-preceeds-p
                                                    :key #'first)))))
                   (t
                    (list operation
                          (when arguments (list 'quote (mapcar #'string arguments)))))))))
      `(eval-when (:execute :load-toplevel :compile-toplevel)
         (modify-package ,package-form ,@(apply #'append
                                                (stable-sort (mapcar #'check-modpackage-option options)
                                                             #'modify-package-operation-preceeds-p
                                                             :key #'first)))))))

#+(and mcl digitool)
(pushnew '(modPackage . 1) *fred-special-indent-alist* :key #'first)

#+source-analysis
(de.setf.utility.documentation:defSourceForm (modPackage :class de.setf.utility.documentation:package-source :category package))


(defgeneric package-pathname (designator)
  (:documentation
   "used to transform a designator for a package location into a logical pathname - minus the file type.")
  (:method ((designator string)) (logical-pathname (concatenate 'string *package-host-name* ":" (substitute #\; #\. designator))))
  (:method ((pathname pathname)) pathname)
  (:method ((package package)) (package-pathname (package-name package)))
  (:method ((designator symbol)) (package-pathname (string-downcase (symbol-name designator)))))

(defgeneric pathname-package-name (pathname)
  (:method ((pathname pathname))
           (format nil "~{~a.~}~@[~a~]"
                   (rest (pathname-directory pathname)) (pathname-name pathname))))

(defgeneric load-package (designator)
  (:method ((designator package))
    (load-package (package-name designator)))

  #+asdf
  (:method ((designator symbol))
    (asdf:operate 'asdf:load-op designator))

  #+asdf
  (:method ((designator string))
    (asdf:operate 'asdf:load-op designator))

  #-asdf
  (:method ((designator t))
    (load-package (package-pathname designator)))
  #-asdf
  (:method ((designator pathname))
    "iterate over registerd hosts; merge the imputed pathname with the host and, where a binary or source file is found, load that file and return the pathname. where a file is found, alos look for an optional, separate, package definition file."
    (let ((package-pathname (make-pathname :name (concatenate 'string (pathname-name designator) "-PACKAGE")
                                           :defaults designator))
          (truename nil)
          (package-truename nil))
      ;; there should be a base file. if there is an additional package file, load it first
      (unless (setf truename (or (probe-file (make-pathname :type *binary-type* :defaults designator))
                                 ;; heuristic to deal with case-specific logical pathnames in linux
                                 (probe-file (string-downcase (namestring (make-pathname :type *binary-type* :defaults designator))))
                                 (probe-file (make-pathname :type "LISP" :defaults designator))
                                 (probe-file (string-downcase (namestring (make-pathname :type "LISP" :defaults designator))))))
        (error 'package-not-found
               :name (pathname-package-name designator) :pathname designator))
      (when (setf package-truename (or (probe-file (make-pathname :type *binary-type* :defaults package-pathname))
                                       (probe-file (string-downcase (namestring (make-pathname :type *binary-type* :defaults package-pathname))))
                                       (probe-file (string-downcase (namestring (make-pathname :type "LISP" :defaults package-pathname))))
                                       (probe-file (make-pathname :type "LISP" :defaults package-pathname))))
        
        (load package-truename))
      (load truename)
      ;; compare explicitly to avoid case problems
      (or (let ((test-name (pathname-package-name designator)))
            (dolist (package (list-all-packages))
              (when (or (string-equal test-name (package-name package))
                        (dolist (nickname (package-nicknames package))
                          (when (string-equal test-name nickname) (return package))))
                (return package))))
          (error 'package-not-found
                 :name (pathname-package-name designator) :pathname truename)))))

(defgeneric edit-package (designator)
  (:method ((designator t))
    (edit-package (find-package designator)))
  (:method ((package package))
    ;; perform indirectly in case the operators are not loaded
    (asdf:operate 'asdf::edit-op (package-name package))))
  

(defgeneric ensure-package (designator &key if-does-not-exist)
  (:method ((package package) &key &allow-other-keys) package)
  (:method ((name symbol) &rest args)
    (declare (dynamic-extent args))
    (apply #'ensure-package (string name) args))
  (:method ((name string) &key (if-does-not-exist :create) (if-exists t))
    (let ((package (find-package name)))
      (if package
        (ecase if-exists
          ((t) package)
          (:supersede
           (ecase if-does-not-exist
             (:create (delete-package package)
                      (make-package name :use nil))
             (:load (load-package name))))
          (:error (error "package exists: ~s" package)))
        (ecase if-does-not-exist
          (:load (load-package name))
          (:error (error 'package-not-found :name name :pathname nil))
          (:create (make-package name :use nil)))))))


;;
;;
;;

(defun purge-package (package &optional (delete-p t))
  "dissolve all dependancies upon the package and delete it. where it is used by another package, unuse it.
   where a symbol has been imported, if it is still present in the argument package. move it to the importing package."
  (modify-package-operation package :purge delete-p))
  
(defun clean-package (package &aux (count 0))
  "remove any uninterned symbols from the package"
  (setf package (find-package package))
  (with-package-iterator (next-symbol package :external :internal)
    (loop (multiple-value-bind (next-p symbol accessibility) (next-symbol)
            (unless next-p (return))
            (unless (symbol-package symbol)
              (when (eq :external accessibility)
                (unexport symbol package))
              (unintern symbol package)
              (incf count))))
    (values package count)))

;;
;;
;;


(defmacro check-feature (feature)
  "ensure that a compiled file is loaded in the same environemnt in which it was compiled."
  (if (find feature *features*)
    `(unless (find ,feature *features* :test #'string-equal)
       (warn "compile-time feature missing upon load: ~s: ~s." ',feature *load-pathname*))
    `(when (find ,feature *features* :test #'string-equal)
       (warn "compile-time non-feature present upon load: ~s: ~s." ',feature *load-pathname*))))

(defparameter *package-version-indicator-name* (string '#:*PACKAGE-VERSION*))

(defun package-version (&optional (designator *package*) &aux version-indicator package)
  "if provided a package designator, returns the values of the version identifier in that package. if provided the value t, returns a plist of package names and version values."
  (if (eq designator t)
    (remove nil (mapcar #'(lambda (package)
                            (multiple-value-bind (version package)
                                                 (package-version package)
                              (when version
                                (cons (package-name package) version))))
                        (list-all-packages)))
    (if (setf package (find-package designator))
      (values (when (and (setf version-indicator (find-symbol *package-version-indicator-name* package))
                         (boundp version-indicator))
                (symbol-value version-indicator))
              package)
      nil)))

(defun (setf package-version) (version designator &aux version-indicator package)
  (setf package (find-package designator))
  (assert (packagep package))
  (setf version-indicator (intern *package-version-indicator-name* package))
  (setf (symbol-value version-indicator) version))


(provide :de.setf.utility.package)

;(trace (setf package-version))

:de.setf.utility


