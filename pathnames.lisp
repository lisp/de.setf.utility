;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It defines helpful pathname operators and defaults and loads the skeleton package definition.
;;;
;;;  (c) 2008, 2009, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; 2008-11-17 [jaa](mailto:james.anderson@setf.de) : factored out of various system definitions
;;; 2010-02-03 jaa : added P-LIBARY host for production v/s dev source


;;;
;;; content :
;;;
;;;  runtime-directory-name ()
;;;    return a directory name unique to the lisp implementation runtime and version
;;;
;;;  set-relative-logical-pathname-translations (host)
;;;    given a logical host name, set its translations relative to a given root.
;;;    by default, it uses the currently processed source file.
;;;    see the use cases at the end of this file.
;;;
;;; translate-physical-pathname (pathname)
;;;    return the most immediate governing logical pathname given a physical pathname


(in-package :common-lisp-user)

;;; ensure an implementation package definition

(eval-when (:execute :load-toplevel :compile-toplevel)
  (unless (find-package :de.setf.utility.implementation)
    (load (make-pathname :name "package" :defaults *load-pathname*))))

(in-package :de.setf.utility.implementation)


(defParameter *logical-source-type*
  #+(or (and allegro unix) sbcl) "lisp"
  #-(or (and allegro unix) sbcl) "LISP")

 
(defParameter *logical-binary-type*
   #+(or (and allegro unix) sbcl) "bin"
   #-(or (and allegro unix) sbcl) "BIN")

(defParameter *physical-source-type* "lisp")
(defParameter *physical-binary-type*
  (pathname-type (compile-file-pathname (make-pathname :name "source" :type *physical-source-type*))))

#+(or )
(setq sb-fasl:*fasl-file-type* "sbcfsl")



;;; set up logical pathname translations relative to a given root

(defun runtime-directory-name ()
  ;; returns the first one which features satisfy
  (or
   #+(and allegro allegro-version>= (version>= 8 0) linux amd64)                     "acl8linux64"
   #+(and allegro allegro-version>= (version>= 7 0) linux)                           "acl7linux"
   #+(and allegro allegro-version>= (version>= 8 0) osx)                             "acl8osx"
   #+(and allegro allegro-version>= (version>= 7 0) osx)                             "acl7osx"
   #+(and allegro allegro-version>= (version>= 7 0) mswindows)                       "acl7win"
   #+(and allegro allegro-version>= (version>= 6 0) osx)                             "acl6osx"
   #+(and allegro allegro-version>= (version>= 6 0) mswindows)                       "acl6win"
   #+(and allegro allegro-version>= (version>= 6 0))                                 "acl6unix"
   #+(and allegro allegro-version>= )                                                "acl5"

   #+clisp                                                                           "clispfasl"
   
   #+cmu                                                                             "cmuclfasl"

   #+cormanlisp                                                                      "corfasl"

   #+(and digitool ccl-5.3)                                                          "digi-5-3"
   #+(and digitool ccl-5.2)                                                          "digi-5-2"
   #+(and digitool ccl-5.1)                                                          "digi-5-1"
   #+(and digitool ccl-5.0)                                                          "digi-5-0"
   #+(and mcl m68k)                                                                  "digim68k"

   #+(and lispworks lispworks5.1 powerpc)                                            "lw-5-1-ppc"
   #+(and lispworks powerpc)                                                         "lw-ppc"
   #+(and lispworks)                                                                 "lw"

   #+(and clozure-common-lisp ccl-1.4 ppc-target)                                    "ccl-1-4-ppc"
   #+(and clozure-common-lisp ccl-1.3 ppc-target)                                    "ccl-1-3-ppc"
   #+(and clozure-common-lisp ccl-1.2 ppc-target)                                    "ccl-1-2-ppc"
   #+openmcl                                                                         "omcl"

   #+(and sbcl linux)                                                                "sbcl-linux"
   #+(and sbcl (or osx darwin))                                                      "sbcl-osx"

   (subseq (lisp-implementation-type) 0
           (position #\space (lisp-implementation-type)))))


#-ccl
(defun directory-pathname-p (path)
  (let ((name (pathname-name path))(type (pathname-type path)))
    (and  (or (null name) (eq name :unspecific) (zerop (length name)))
          (or (null type) (eq type :unspecific)))))

(defun make-hosted-pathname (host namestring)
  (format nil "~a:~a" host namestring))

(defun make-binary-translation-target (host)
  (make-pathname :host host :directory `(:absolute "bin" ,(runtime-directory-name) :wild-inferiors)
                 ;; :version :newest
                 :name :wild :type (pathname-type (compile-file-pathname "NAME.LISP"))))


(defun set-relative-logical-pathname-translations
       (host &key (base  (or *compile-file-truename* *load-truename*))
             ((:relative-pathname location) nil)
             ((:absolute-pathname root-directory)
              (if location (merge-pathnames location base) base))
             (root-target (make-pathname :name :wild :type :wild
                                         :directory (append (pathname-directory root-directory)
                                                            '(:wild-inferiors))
                                         :defaults root-directory))
             (translations nil))
  (let ((bin nil)
        #+clisp (CUSTOM:*PARSE-NAMESTRING-ANSI* t))
    (setf host (string host))
    ;; first bootstrap bin, use it to get the actual location
    ;; then install the extended translation - w/ bin mapped to a physical location
    ;; nb. clisp fails with the error "*** - MAKE-PATHNAME: Illegal :DIRECTORY argument (:DIRECTORY)"
    ;; if the mapping include intermediate targets, eg. ("ROOT;**;*.*.*" ,(make-pathname :version :wild :defaults root-target))
    ;; thus the enumerated targets
    (setf (logical-pathname-translations host)
          `(("**;*.*.*" ,(make-pathname :version :wild :defaults root-target))
            ("**;*.*" ,(make-pathname :version nil :defaults root-target))))
    (setf bin (translate-logical-pathname (make-binary-translation-target host)))
    (when *load-verbose*
      (format *trace-output* "~&Host translations: ~a~%        base: ~s.~%    location: ~s.~% root-target: ~s.~%      binary: ~s."
              host base location root-target bin))
    (setf (logical-pathname-translations host)
          ;; some implementation require the distinction in version designator
          (remove-duplicates `(,@translations
                               ("**;*.bin" ,(make-pathname :version nil :defaults bin))
                               ("**;*.BIN" ,(make-pathname :version nil :defaults bin))
                               ("**;*.bin.*" ,bin)
                               ("**;*.BIN.*" ,bin)
                               (,(format nil "**;*.~a" *physical-binary-type*) ,(make-pathname :version nil :defaults bin))
                               (,(format nil "**;*.~a.*" *physical-binary-type*) ,bin)
                               ("code;**;*.*" ,(make-pathname :directory `(,@(butlast (pathname-directory root-target))
                                                                           "code"
                                                                           ,(first (last (pathname-directory root-target))))
                                                              :name :wild :type :wild :version nil
                                                              :defaults root-target))
                               ("code;**;*.*.*" ,(make-pathname :directory `(,@(butlast (pathname-directory root-target))
                                                                             "code"
                                                                             ,(first (last (pathname-directory root-target))))
                                                                :name :wild :type :wild
                                                                :defaults root-target))
                               ("**;*.*" ,(make-pathname :directory (pathname-directory root-target)
                                                         :name :wild :type :wild :version nil
                                                         :defaults root-target))
                               ("**;*.*.*" ,(make-pathname :directory (pathname-directory root-target)
                                                           :name :wild :type :wild
                                                           :defaults root-target))
                               ;;("**;*.*.*" ,(make-hosted-pathname host  "ROOT;**;*.*.*"))
                               ;;("**;*.*" ,(make-hosted-pathname host  "ROOT;**;*.*"))
                               )
                             :from-end t :key #'first :test #'equalp))))

#+(or )
(let ((bin-type "fas"))
  (setf (logical-pathname-translations "ASDFTEST")
        `(;(,(format nil "**;*.~a" bin-type)
          ; ,(make-pathname :directory '(:absolute "development" "bin" :wild-inferiors) :name :wild :type bin-type :version nil))
          ;(,(format nil "**;*.~a.*" bin-type)
          ; ,(make-pathname :directory '(:absolute "development" "bin" :wild-inferiors) :name :wild :type bin-type))
          ("**;*.*"
           ,(make-pathname :directory '(:absolute "development" "source" :wild-inferiors) :name :wild :type :wild :version nil))
          ("**;*.*.*"
           ,(make-pathname :directory '(:absolute "development" "source" :wild-inferiors) :name :wild :type :wild)))))

#+allegro
(defun logical-hosts ()
  (loop for host being each hash-key of excl::*logical-pathname-translations*
        collect host))

;;; both clozure and digitool bind host definitions as an alist
#+ccl
(defun logical-hosts-translations ()
  ccl::%logical-host-translations%)
#+ccl
(defun logical-hosts ()
  (mapcar #'first (logical-hosts-translations)))

#+(or clisp lispworks)
(defun logical-hosts ()
  (loop for host being each hash-key of system::*logical-pathname-translations*
        collect host))

#+cmu
(defun logical-hosts ()
  (loop for host being each hash-key of lisp::*logical-hosts*
        collect host))

#+ecl
;; as per jj.garcia-ripoll
(progn
  (eval-when (:execute)
    (setf (symbol-function 'logical-hosts)
          (compile nil '(lambda ()
                          (mapcar #'first (ffi:c-inline () () :object "cl_core.pathname_translations" :one-liner t))))))

  (eval-when (:compile-toplevel :load-toplevel)
    (defun logical-hosts ()
      (mapcar #'first (ffi:c-inline () () :object "cl_core.pathname_translations" :one-liner t)))))

#+sbcl
(defun logical-hosts ()
  (loop for host being each hash-key of SB-IMPL::*LOGICAL-HOSTS*
        collect host))

#-(or allegro ccl clisp lispworks sbcl ecl cmu)
(defun logical-hosts ()
  (cerror "Assume no logical hosts." "This runtime has no definition for ~s." 'logical-hosts)
  nil)

(defgeneric translate-physical-pathname (pathname &rest args)
  (:documentation "translate a given PATHNAME back to the most specific logical pathname.
 PATHNAME : (designator PATHNAME)
 VALUE    : (or LOGICAL-PATHNAME NULL) : the most specific logical pathname or NIL if
  no host dominates the given pathname")

  (:method ((designator string) &rest args)
    (declare (dynamic-extent args))
    (apply #'translate-physical-pathname (pathname designator) args))

  (:method ((designator stream) &rest args)
    (declare (dynamic-extent args))
    (apply #'translate-physical-pathname (pathname designator) args))

  (:method ((pathname logical-pathname) &key if-does-not-exist)
    (declare (ignore if-does-not-exist))
    pathname)

  (:method ((pathname pathname) &key (if-does-not-exist :create))
    (let ((specific-enough nil)
          (namestring (namestring pathname))
          (translated nil))
      (flet ((record-candidate (host enough)
               (let* ((candidate (make-pathname :host host :directory (cons :absolute (rest (pathname-directory enough)))
                                                :name (pathname-name pathname) :type (pathname-type pathname)
                                                :version (pathname-version pathname)
                                                :defaults enough))
                      (retranslated-candidate (ignore-errors (translate-logical-pathname candidate))))
                 (when (and (equalp pathname retranslated-candidate)
                            (ecase if-does-not-exist
                              ((nil) (probe-file retranslated-candidate))
                              (:create t)))
                   (cond ((null translated)
                          (setf specific-enough enough)
                          (setf translated candidate))
                         ((and (> (length specific-enough) (length enough))
                               (string-equal specific-enough enough :start1 (- (length specific-enough) (length enough))))
                          (setf specific-enough enough)
                          (setf translated candidate))
                         ((string-equal specific-enough enough)
                          ;; there are two hosts which map to the same file, with the same host-relative path
                          (cerror "ignore ambiguity and continue."
                                  "translate-physical-pathname: ambiguous host-relative pathname: ~s , ~s)."
                                  translated candidate)
                          (setf specific-enough enough)
                          (setf translated candidate)))))))
        (dolist (host (logical-hosts))
          (let ((proto-translation (ignore-errors (translate-logical-pathname
                                                   (make-pathname :host host :directory '(:absolute) :name "TEST" :type "LISP")))))
            (when proto-translation
              (let* ((host-translation (make-pathname :name nil :type nil :defaults proto-translation))
                     (enough (enough-namestring pathname host-translation)))
                ;; if the pathname is "hosted", check if it is commensurable with already found
                ;; if yes, retain the more specific host. if no, signal an error
                ;; if non yet, cache this one 
                (unless (equal enough namestring)
                  (record-candidate host enough)))))))
      translated)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; set standard host complement
;;; - LIBRARY    presumes that this is in in `#p"LIBRARY:de;setf;utility;"`
;;; - P-LIBRARY  iff it is found at `#p"LIBRARY:..;..;production;Library;"`
;;;

;;; (setf (logical-pathname-translations "LIBRARY") nil)
;;; if there is no LIBRARY host, make one
;;; the macrolet captures *compile-file-pathname*, which should work in all runtimes.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-library-host (source-pathname)
    ;; must be physical in order to serve at the target for logical mappings
    (setf source-pathname (translate-logical-pathname source-pathname))
    (set-relative-logical-pathname-translations "LIBRARY"
                                                :absolute-pathname
                                                (make-pathname :directory (butlast (pathname-directory source-pathname) 3)
                                                               :name nil :type nil
                                                               :defaults source-pathname)))
  
  (or (ignore-errors (logical-pathname-translations "LIBRARY"))
      (macrolet ((source-pathname () (truename (or *compile-file-pathname* *load-pathname*))))
        #+cmu (handler-bind ((error (lambda (c) (warn "redefining LIBRARY: ~a." c) (continue))))
                (define-library-host (source-pathname)))
        #-cmu (define-library-host (source-pathname)))))

;; nb. clisp neither merges :up relative pathnames, nor (by default) correctly parses logical namestrings
(let* ((library (truename (make-pathname :host "LIBRARY" :directory '(:absolute))))
       (production (make-pathname :directory (append (butlast (pathname-directory library) 2)
                                                     '("production" "Library"))
                                  :name nil :type nil :defaults library)))
  (when (and (#-clisp probe-file #+clisp ext:probe-directory production)
             (not (equalp production library)))
    (set-relative-logical-pathname-translations "P-LIBRARY" :absolute-pathname production)))
           

(let* ((logical (make-pathname :host "LIBRARY" :directory '(:absolute "de" "setf" "utility")
                               :name "pathnames" :type "lisp"))
       (physical (translate-logical-pathname logical))
       (back (translate-physical-pathname physical)))
  ;; test equality for the primary attributes
  ;; and control for uniqueness
  (let ((result (when back
                  (list (equalp (pathname-host back) (pathname-host logical))
                        (equal (pathname-directory back) (pathname-directory logical))
                        (equalp (pathname-name back) (pathname-name logical))
                        (equalp (pathname-type back) (pathname-type logical))
                        (null (translate-physical-pathname (make-pathname :type "xxx" :defaults physical)
                                                           :if-does-not-exist nil))))))
    (when (or (null result) (member nil result))
      (warn "translate-physical-pathname failed: ~s" result))))
;(trace translate-physical-pathname)
;(translate-physical-pathname (translate-logical-pathname #P"LIBRARY:de;setf;utility;pathnames.lisp"))
:de.setf.utility
