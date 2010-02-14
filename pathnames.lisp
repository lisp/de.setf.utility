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

   #+(and clozure-common-lisp ccl-1.3 ppc-target)                                    "ccl-1-3-ppc"
   #+(and clozure-common-lisp ccl-1.2 ppc-target)                                    "ccl-1-2-ppc"
   #+openmcl                                                                         "omcl"

   #+(and sbcl linux)                                                                "sbcl-linux"
   #+(and sbcl (or osx darwin))                                                      "sbcl-osx"
   
   (error "no runtime directory defined for ~s / ~s."
          (lisp-implementation-type) (lisp-implementation-version))))


(defun make-hosted-pathname (host namestring)
  (format nil "~a:~a" host namestring))

(defun make-binary-translation-target (host)
  (make-pathname :name :wild
                 :type (pathname-type (compile-file-pathname ";NAME.LISP"))
                 :version :newest
                 :defaults
                 (make-hosted-pathname host (format nil "root;bin;~a;**;*.*.*" (runtime-directory-name)))))

(defun set-relative-logical-pathname-translations
       (host &key (base  (or *compile-file-truename* *load-truename*))
             ((:relative-pathname location) nil)
             ((:absolute-pathname root-directory)
              (if location (merge-pathnames location base) base))
             (root-target (make-pathname :name :wild :type :wild :version :newest
                                         :directory (append (pathname-directory root-directory)
                                                            '(:wild-inferiors))
                                         :defaults root-directory))
             (translations nil))
  (let ((bin nil))
    (setf host (string host))
    ;; first bootstrap bin, use it to get the actual location
    ;; then install the extended translation - w/ bin mapped to a physical location
    (setf (logical-pathname-translations host)
          `(("root;**;*.*.*" ,root-target)))
    (setf bin (translate-logical-pathname (make-binary-translation-target host)))
    (when *load-verbose*
      (format *trace-output* "~&Host translations: ~a~%        base: ~s.~%    location: ~s.~% root-target: ~s.~%      binary: ~s."
              host base location root-target bin))
    (setf (logical-pathname-translations host)
          ;; some implementation require the distinction in version designator
          `(("**;*.bin" ,(make-pathname :version nil :defaults bin))
            ("**;*.BIN" ,(make-pathname :version nil :defaults bin))
            ("**;*.bin.*" ,bin)
            ("**;*.BIN.*" ,bin)
            (,(format nil "**;*.~a" *physical-binary-type*) ,(make-pathname :version nil :defaults bin))
            (,(format nil "**;*.~a.*" *physical-binary-type*) ,(make-pathname :version :wild :defaults bin))
            ("code;**;*.*.*" ,(make-hosted-pathname host "root;code;**;*.*.*"))
            ("code;**;*.*" ,(make-hosted-pathname host  "root;code;**;*.*"))
            ("root;**;*.*" ,(make-pathname :version nil :defaults root-target))
            ("root;**;*.*.*" ,root-target)
            ,@translations
            ("**;*.*" ,(make-hosted-pathname host  "root;**;*.*"))
            ))))

;;; both clozure and digitool bind host definitions as an alist
#+ccl
(defun logical-hosts-translations ()
  ccl::%logical-host-translations%)
#+ccl
(defun logical-hosts ()
  (mapcar #'first (logical-hosts-translations)))
#+lispworks
(defun logical-hosts ()
  (loop for host being each hash-key of system::*logical-pathname-translations*
        collect host))
#+sbcl
(defun logical-hosts ()
  (loop for host being each hash-key of SB-IMPL::*LOGICAL-HOSTS*
        collect host))

(defgeneric translate-physical-pathname (pathname &key &allow-other-keys)
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

  (:method ((pathname logical-pathname) &key &allow-other-keys)
    pathname)

  (:method ((pathname pathname) &key &allow-other-keys)
    (let ((specific-host nil)
          (specific-enough nil)
          (namestring (namestring pathname)))
      (flet ((record-candidate (host enough)
               (setf specific-host host)
               (setf specific-enough enough)))
        (dolist (host (logical-hosts))
          (let ((proto-translation (ignore-errors (translate-logical-pathname (concatenate 'string host ":TEST.LISP")))))
            (when proto-translation
              (let* ((host-translation (make-pathname :name nil :type nil :defaults proto-translation))
                     (enough (enough-namestring pathname host-translation)))
                ;; if the pathname is "hosted", check if it is commensurable with already found
                ;; if yes, retain the more specific host. if no, signal an error
                ;; if non yet, cache this one
                (unless (equal enough namestring)
                  (cond ((null specific-host)
                         ;; save first candidate
                         (record-candidate host enough))
                        ((and (> (length specific-enough) (length enough))
                              (string-equal specific-enough enough :start1 (- (length specific-enough) (length enough))))
                         ;; replace candidate
                         (record-candidate host enough))
                        ((and (> (length enough) (length specific-enough))
                              (string-equal specific-enough enough :start2 (- (length enough) (length specific-enough))))
                         ;; skip additional candidate
                         )
                        (t
                         ;; neither fit in the other
                         (cerror "ignore ambiguity and continue."
                                 "translate-physical-pathname: ambiguous host-relative pathname: (~s . ~s) (~s . ~s)"
                                 specific-host specific-enough host enough)))))))))
      (when specific-host
        (make-pathname :host specific-host
                       :directory (cons :absolute (rest (pathname-directory specific-enough)))
                       :name (pathname-name specific-enough)
                       :type (pathname-type specific-enough)
                       :defaults specific-enough)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; set standard host complement
;;; - LIBRARY    presumes that this is in in `#p"LIBRARY:de;setf;utility;"`
;;; - P-LIBRARY  iff it is found at `#p"LIBRARY:..;..;production;Library;"`

;;; (setf (logical-pathname-translations "LIBRARY") nil)
;;; if there is no LIBRARY host, make one
(or (ignore-errors (logical-pathname-translations "LIBRARY"))
    (apply #'set-relative-logical-pathname-translations
           "LIBRARY"
           #+digitool (list :relative-pathname
                            (make-pathname :directory '(:relative :up :up :up)))
           
           #+clozure (list :absolute-pathname
                            (let ((pathname *LOADING-FILE-SOURCE-FILE*))
                              (make-pathname :directory (butlast (pathname-directory pathname) 3)
                                             :name nil :type nil
                                             :defaults pathname)))
           #-(or digitool clozure) (list :absolute-pathname
                                         (let ((pathname *LOAD-pathname*))
                                           (make-pathname :directory (butlast (pathname-directory pathname) 3)
                                                          :name nil :type nil
                                                          :defaults pathname)))))
(let ((production (merge-pathnames (make-pathname :directory '(:relative :up :up "production" "Library"))
                                   (truename #p"LIBRARY:"))))
  (when (probe-file production)
    (set-relative-logical-pathname-translations "P-LIBRARY" :absolute-pathname production)))
           

(unless (let* ((logical #P"LIBRARY:asdf;asdf.lisp")
               (physical (translate-logical-pathname logical))
               (back (translate-physical-pathname physical)))
          ;; test equality for the primary attributes
          ;; and control for uniqueness
          (and (equalp (pathname-host back) (pathname-host logical))
               (equal (pathname-directory back) (pathname-directory logical))
               (equalp (pathname-name back) (pathname-name logical))
               (equalp (pathname-type back) (pathname-type logical))
               (null (translate-physical-pathname
                      (make-pathname :directory (butlast (pathname-directory physical) 2)
                                     :defaults physical)))))
  (warn "translate-physical-pathname ?"))
;(trace translate-physical-pathname)
:de.setf.utility
