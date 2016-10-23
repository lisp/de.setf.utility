;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)


(:documentation :file
 (description "This file defines a CLOS model for MIME types.")
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description
  "See : (among others)

   - [wikipedia](http://en.wikipedia.org/wiki/MIME)
   - [rfc2046](http://tools.ietf.org/html/rfc2046) : (MIME) Part Two: Media Types
   - [rfc2049](http://tools.ietf.org/html/rfc2049) : (MIME) Part Five: Conformance Criteria and Examples
   - [IANA](http://www.iana.org/assignments/media-types/) : media type list
   - [rdfa](http://www.w3.org/TR/rdfa-core/#xmlrdfaconformance) : rdfa media types

   Each type is defined as a singleton in a major/minor type lattice and bound to a
 global variable with the same name. The `text/*` types include a slot for a content encoding
 name."))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (defParameter *mime-type-package* (find-package :de.setf.utility.mime.type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-mime-type-key (name &key (if-does-not-exist :error))
    (or (find-symbol (setf name (string-upcase name)) *mime-type-package*)
        (case if-does-not-exist
          (:error (error "undefined mime type keyword: ~s." name))
          ((:create mime-type)
           (setf name (intern name *mime-type-package*))
           (export name *mime-type-package*)
           name)
          (t ; if nil or some other class, return nil to indicate it does not exist
           nil)))))

(defMacro def-mime-type-key (symbol &rest args &key (if-does-not-exist :create))
  (setf symbol (apply #'intern-mime-type-key symbol :if-does-not-exist if-does-not-exist args))
  `(defvar ,symbol ',symbol))
(defmacro defmimetypekey (symbol)
  `(def-mime-type-key ,symbol))

(def-mime-type-key "APPLICATION")
(def-mime-type-key "CSV")
(def-mime-type-key "FORM-DATA")
(def-mime-type-key "HTML")
(def-mime-type-key "HTML+RDFA")
(def-mime-type-key "IMAGE")
(def-mime-type-key "JSON")
(def-mime-type-key "JPEG")
(def-mime-type-key "MARKDOWN")
(def-mime-type-key "MULTIPART")
(def-mime-type-key "N3")
(def-mime-type-key "PLAIN")
(def-mime-type-key "PDF")
(def-mime-type-key "RDF")
(def-mime-type-key "RDF+XML")
(def-mime-type-key "SVG")
(def-mime-type-key "SVG+XML")
(def-mime-type-key "TEXT")
(def-mime-type-key "TAB-SEPARATED-VALUES")
(def-mime-type-key "TURTLE")
(def-mime-type-key "VND.GRAPHVIZ")
(def-mime-type-key "X-GRAPHVIZ")
(def-mime-type-key "X-WWW-FORM-URLENCODED")
(def-mime-type-key "XML")
(def-mime-type-key "XHTML")
(def-mime-type-key "XHTML+RDFA")
(def-mime-type-key "XHTML+XML")
(def-mime-type-key "*")

(defmacro def-mime-type ((major minor) &optional supers slots &rest options)
  "define three mime type classes: one each for the major ane minor type , and one for
   the specific type which specializes the two general types.
   define and bind static instances for each class."
  (setf major (intern-mime-type-key major)
        minor (intern-mime-type-key minor))
  (flet ((defvar-form (class-name)
           `(if (boundp ',class-name)
              (unless (typep ,class-name ',class-name)
                (error "invalid mime type constant for type '~a': ~s."
                       ',class-name ,class-name))
              (defvar ,class-name (make-instance ',class-name)))))
    (let ((class-name (intern-mime-type-key (format nil "~a/~a" major minor)
                                            :if-does-not-exist :create)))
      ;; for the compiler
      (proclaim `(special ,class-name))
      ;; if the monor type is wild, then this is a major-type definition
      ;; otherwise, it's a concrete mime type.
      ;; define the class - either just the major, or the concrete and
      ;; the minor; export the names and bind them to constant instances.
      (if (eq minor 'mime:*)
        `(progn                         ; a prog1 form cause class-not-found error in mcl
           (defclass ,class-name (major-mime-type)
             ((major-type :initform ',major :allocation :class) ,@slots)
             ,@options)
           (eval-when (:execute :compile-toplevel :load-toplevel)
             (declaim (special ,class-name))
             (export ',class-name *mime-type-package*))
           ,(defvar-form class-name)
           (find-class ',class-name))
        (let ((major-class-name (intern-mime-type-key (format nil "~a/~a" major "*")
                                                      :if-does-not-exist :create))
              (minor-class-name (intern-mime-type-key (format nil "~a/~a" "*" minor)
                                                      :if-does-not-exist :create)))
          (proclaim `(special ,minor-class-name))
          `(progn 
             (eval-when (:execute :load-toplevel)
               (export '(,class-name ,minor-class-name ,major-class-name)
                       *mime-type-package*))
             (progn ;; always do it unless (find-class ',minor-class-name nil)
               (defclass ,minor-class-name (minor-mime-type)
                 ((minor-type :initform ',minor :allocation :class)))
               (eval-when (:execute :compile-toplevel :load-toplevel)
                 (declaim (special ,minor-class-name)))
               ,(defvar-form minor-class-name))
             (prog1                     ; see above re major type definition
                                        ; nb. progn . find-class failed in ccl-1.4
               (defclass ,class-name ( ,@supers ,major-class-name ,minor-class-name mime:*/*)
                 ((expression :allocation :class :initform '(,(intern (string major) :keyword)
                                                             ,(intern (string minor) :keyword)))
                  ,@slots)
                 ,@options)
               ,(defvar-form class-name))))))))

(defmacro defmimetype (&rest args)
  `(def-mime-type ,@args))

(defclass mime-type ()
   ((expression :allocation :class :reader mime-type-expression :initform nil)
    (file-type :reader get-mime-type-file-type :initform nil)
    (quality :initarg :q :initarg :quality :initform 1
             :reader mime-type-quality)
    ))

(defclass mime-type-profile (mime-type)
  ((profile :initarg :profile :initform nil
            :accessor mime-type-profile)
   (base-type :initarg :base-type :initform nil
              :reader mime-type-base-type)))

(defgeneric mime-type-profile-p (mime-type profile)
  (:method ((mime-type t) (profile t))
    nil)
  (:method ((mime-type mime-type-profile) (profile string))
    (mime-type-profile-p (mime:mime-type-profile mime-type) profile))
  (:method ((type-profile string) (profile string))
    (string= profile type-profile))
  (:method ((type-profile list) (profile string))
    (find profile type-profile :test #'equal)))


(defclass unsupported-mime-type (mime-type)
  ((expression :allocation :instance :initarg :expression
               :initform "expression is required for unsupported media types.")))
(defmethod initialize-instance :before ((instance unsupported-mime-type) &key &allow-other-keys)
  ;; ignore everything
  )

(defclass mime:binary (mime:*/*)
  ()
  (:documentation "The abstract binary mime type is specialized, eg as APPLICATION/OCTET-STREAM, to mark a stream
 for binary operations rather than text decoding."))

(defclass delegate-mime-type ()
  ((canonical-mime-type
    :initform nil
    :reader mime-type-canonical-mime-type)))

(defclass experimental-mime-type (delegate-mime-type)
  ())

(defclass superseded-mime-type (delegate-mime-type)
  ())

(defgeneric canonical-mime-type (mime-type)
  (:method ((type delegate-mime-type))
    (or (mime-type-canonical-mime-type type)
        type))

  (:method ((type mime-type))
    type))

#+(or)                                  ; no longer suffices once w/ charset
(defmethod make-load-form ((instance mime-type) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(type-of instance)))

(defmethod make-load-form ((instance mime-type) &optional environment)
  "Compute a load form which includes any slots, eg. charset."
  (make-load-form-saving-slots instance :environment environment))

(defgeneric mime-type-p (datum)
  (:method ((datum mime-type)) t)
  (:method ((datum t)) nil))

(defgeneric binary-mime-type-p (datum)
  (:method ((datum mime:binary)) t)
  (:method ((datum t)) nil))

(defclass major-mime-type (mime-type)
  ((major-type :initform nil :initarg :major-type
               :reader mime-type-major-type)))


(defclass minor-mime-type (mime-type)
  ((minor-type :initform nil :initarg :minor-type
               :reader mime-type-minor-type)))

(defclass mime:*/* (major-mime-type minor-mime-type)
  ((charset
    :initarg :charset :initform nil
    :reader mime-type-charset
    :type symbol
    :documentation "See http://www.iana.org/assignments/character-sets")))
(defvar mime:*/* (make-instance 'mime:*/*))

;;; must be ordered such that abstract mime types appear first,
;;; as each definition form instantiates a singleton

(def-mime-type ("APPLICATION" "*"))
(def-mime-type ("IMAGE" "*"))
(def-mime-type ("MULTIPART" "*"))
(def-mime-type ("RDF" "*"))
(def-mime-type ("TEXT" "*") ()
  ((charset
    :initform :iso-8859-1)))

(defclass mime:graphviz (mime:*/*)
  ((file-type :initform "dot" :allocation :class))
  (:documentation "The abstract graphviz mime type is specialized as
 TEXT/X-GRAPHVIZ as per [graphviz-interest](https://mailman.research.att.com/pipermail/graphviz-interest/2009q1/005997.html),
 and as TEXT/VND.GRAPHVIZ as per [IANA](http://www.iana.org/assignments/media-types/text/)."))

(def-mime-type ("TEXT" "PLAIN") ()
  ((file-type :initform "txt" :allocation :class)))

(defclass mime:rdf (mime:*/*)
  ()
  (:documentation
    "A protocol class to indicate that the concreate mime type is an RDF media type.
 see: http://www.w3.org/2008/01/rdf-media-types, which is obsolete, but a start.
 It is specified neither as text nor binary as there are alternatives."))

(defclass mime:n3 (mime:text/plain mime:rdf)
  ()
  (:documentation "The N3 mime class is the abstract base class for application/n3 and text/n3."))

(defclass mime:turtle (mime:text/plain mime:rdf)
  ((charset
    :initform nil))
  (:documentation "The TURTLE mime class is the abstract base class for text/turtle."))

(def-mime-type ("APPLICATION" "JSON"))
(def-mime-type ("APPLICATION" "N3") (mime:n3))
(def-mime-type ("APPLICATION" "OCTET-STREAM") (mime:binary))
(def-mime-type ("APPLICATION" "PDF"))
(def-mime-type ("APPLICATION" "XHTML+XML") ()
  ((file-type :initform "html" :allocation :class))
  (:documentation "as per [w3c](http://www.w3.org/TR/xhtml-media-types/)."))
(def-mime-type ("APPLICATION" "XHTML+RDFA") (mime:application/xhtml+xml))
(def-mime-type ("APPLICATION" "XML"))
(def-mime-type ("APPLICATION" "RDF+XML") (mime:rdf mime:application/xml)
  ((file-type :initform "rdf" :allocation :class))
  (:documentation "This includes OWL as well as per [w3c](http://www.w3.org/TR/owl-ref/#MIMEType)."))
(def-mime-type ("APPLICATION" "X-WWW-FORM-URLENCODED") (experimental-mime-type)
  ((charset :initform :utf-8)
   (file-type :initform nil)))
(def-mime-type ("MULTIPART" "FORM-DATA"))
(def-mime-type ("TEXT" "TAB-SEPARATED-VALUES") ()
  ((charset :initform nil)
   (file-type :initform "tsv" :allocation :class))
  (:documentation "SPARQL results encoded as a stream of binary symbolic expressions"))
(def-mime-type ("IMAGE" "JPEG") (mime:binary)
  ((file-type :initform "jpg" :allocation :class)))
(def-mime-type ("IMAGE" "SVG") ()
  ((file-type :initform "svg" :allocation :class)))
(def-mime-type ("IMAGE" "SVG+XML") (mime::image/svg))
(def-mime-type ("TEXT" "CSV") ()
  ((file-type :initform "csv" :allocation :class)))
(def-mime-type ("TEXT" "N3") (mime:n3)
  ((file-type :initform "nt" :allocation :class))
  (:documentation "The [w3c](http://www.w3.org/TR/rdf-testcases/#ntriples) specifies text/plain."))
(def-mime-type ("TEXT" "XHTML"))
(def-mime-type ("TEXT" "HTML") ()
  ((file-type :initform "html" :allocation :class)))
(def-mime-type ("TEXT" "HTML+RDFA") (mime:text/html))
(def-mime-type ("TEXT" "MARKDOWN") ()
  ((file-type :initform "md" :allocation :class)))
(def-mime-type ("TEXT" "TURTLE") (mime:turtle))
(def-mime-type ("TEXT" "VND.GRAPHVIZ") (mime:graphviz)
  ((file-type :initform "gv" :allocation :class)))
(def-mime-type ("TEXT" "X-GRAPHVIZ") (mime:graphviz))
(def-mime-type ("TEXT" "XML") ()
  ((file-type :initform "xml" :allocation :class)))


(defgeneric mime-type-file-type (mime-type)
  (:method ((type minor-mime-type))
    (or (get-mime-type-file-type type)
        (setf (slot-value type 'file-type)
              (string-downcase (mime-type-minor-type type))))))

(defgeneric mime-type-profile (mime-type)
  (:documentation "return the given media type's profile.
   this signifies for profile-mime-type instance only. other return nil.")
  (:method ((type mime-type))
    nil))

(defgeneric mime-type-base-type (mime-type)
  (:documentation "return a base type instance for a given media type.
   This is an initialization value for profile types, but the identity for others.")
  (:method ((type mime-type))
    type))

(defgeneric mime-type-namestring (mime-type)
  (:documentation "generate the namestring for a media type given its properties")
  (:method ((media-type mime-type))
    (format nil "~(~a~@[; q=~$~]~@[; charset=~a~]~@[; profile=\"~a\"~]~)"
            (type-of (or (mime-type-base-type media-type) media-type))
            (let ((q (mime-type-quality media-type))) (unless (= q 1) q))
            (mime-type-charset media-type)
            (mime-type-profile media-type)))
  (:method ((media-type unsupported-mime-type))
    (mime-type-expression media-type))
  (:method ((type string)) ; assume it is correct
    type)
  (:method ((type null))
    nil))


(defgeneric mime-type (designator &rest args)
  (:documentation
   "coerce a designator to a mime type. fail if non is defined.")

  (:method ((mime-type mime-type) &rest args)
    (if args
      (apply #'make-instance (class-of mime-type) args)
      mime-type))

  (:method ((designator cons) &key &allow-other-keys)
    "Given a cons, either the first two elements arethe type and the remainder
     the initargs, in which case they should both be keywords, or the
     first is a mime-type type and all thr rest are the initargs "
    (destructuring-bind (major minor . args) designator
      (declare (dynamic-extent args))
      (typecase major
        (keyword 
         (let* ((expression (format nil "~a/~a" major minor))
                (type  (intern-mime-type-key expression :if-does-not-exist nil)))
           (if type
               (apply #'mime-type type args)
               (make-instance 'unsupported-mime-type :expression expression))))
        (symbol
         (apply #'mime-type major minor args)))))

  (:method ((designator string) &rest args &key (if-does-not-exist 'unsupported-mime-type idne-s) &allow-other-keys)
    "Given a string, parse it - isolating any arguments, coerce the type to the
     class designator and continue with the argument list."
    (declare (dynamic-extent args))
    (setf designator (string-trim #(#\space #\tab) designator))
    (flet ((quote-p (char)
             (case char ((#\" #\') t))))
      (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*package* (find-package :keyword)))
            (destructuring-bind (type-name . parameters) (split-string designator ";")
              (setf parameters (loop for parameter in parameters
                                 append (destructuring-bind (attribute value) (split-string parameter "=")
                                          (setf attribute (cons-symbol :keyword attribute))
                                          (list attribute
                                                (canonicalize-media-type-property attribute value)))))
              (destructuring-bind (&key profile &allow-other-keys) parameters
                (when idne-s
                  (setf args (plist-difference args '(:if-does-not-exist))))
                (let ((mime-type-symbol (intern-mime-type-key type-name :if-does-not-exist if-does-not-exist)))
                  ;; the symbol is either a known media type, or some other type, which is to be constrained, or null
                  (cond (mime-type-symbol
                         (when profile
                           ;; look for a possible subtype
                           (let ((profile-type (profile-media-type-type mime-type-symbol profile)))
                             (when profile-type (setf mime-type-symbol profile-type))))
                         (apply #'mime-type mime-type-symbol (append parameters args)))
                        (if-does-not-exist
                         (assert (subtypep if-does-not-exist 'mime-type) ()
                                 "Specified media type must specialize mime:mime-type: ~s" if-does-not-exist)
                         (apply #'make-instance if-does-not-exist :expression type-name parameters))
                        (t
                         nil)))))))))

  (:method ((designator symbol) &rest args)
    "Given a type designator, w/ args make a new one, w/o args return the singleton.
 Test validity by checking for a binding and constraining its type."
    (declare (dynamic-extent args))
    (let ((type (and (boundp designator) (symbol-value designator))))
      (assert (typep type 'mime-type) ()
              "Invalid mime type designator: ~s." designator)
      ;;(when (getf args :charset) (break "mime-type: ~s" args))
      (if args ; 
        (apply #'make-instance designator args)
        type)))

  (:method ((location pathname) &key &allow-other-keys)
    (let ((file-type (pathname-type location)))
      (with-package-iterator  (next :mime :external)
        (loop (multiple-value-bind (next-p designator) (next)
                (unless next-p (return))
                (let ((mime-type (when (boundp designator) (symbol-value designator))))
                  (when (and (typep mime-type 'minor-mime-type)
                             (typep mime-type 'major-mime-type)
                             (string-equal (mime-type-file-type mime-type) file-type))
                    (return mime-type)))))))))

(defgeneric canonicalize-media-type-property (property-name value)
  (:method ((name (eql :profile)) (value string))
    (if (plusp (length value))
        (if (eql (char value 0) #\")
            (when (eql (char value (1- (length value))) #\")
              (setf value (split-string (subseq value 1 (1- (length value))) " ")))
            (unless (eql (char value (1- (length value))) #\")
              (setf value (split-string value " "))))
        (setf value ()))
    (assert (listp value) ()
            "invalid media type profile: ~s" value)
    value)
  (:method ((name (eql :q)) (value string))
    (canonicalize-media-type-property :quality value))
  (:method ((name (eql :quality)) (value string))
    (labels ((qvalue-char-p (c) (or (digit-char-p c) (eql c #\.)))
             (parse-qvalue (qvalue)
               (assert (every #'qvalue-char-p qvalue) ()
                       "Invalid qvalue: '~a'" qvalue)
               (read-from-string qvalue)))
      (parse-qvalue value)))
  (:method ((name t) (value string))
    (assert (plusp (length value)) () "invalid media type property: ~s" value)
    (read-from-string value)))

(defgeneric profile-media-type-type (root-type profile)
  (:documentation "iterate over known media type classes to locate one
   which is both a subtype of the given type and indicates the given profile.
   the iteration is in order not to use the mop operators.
   the profile->class mapping must be unique.")
  (:method ((root-type symbol) profile)
    (with-package-iterator  (next :mime :external)
      (loop (multiple-value-bind (next-p designator) (next)
              (unless next-p (return))
              (let ((mime-type (when (boundp designator) (symbol-value designator))))
                (when (and (typep mime-type root-type)
                           (equalp profile (mime-type-profile mime-type)))
                  (return designator)))))))
  (:method ((mime-type mime-type) profile)
    (profile-media-type-type (type-of mime-type) profile)))

(defun list-mime-types ()
  (let ((types ()))
    (with-package-iterator  (next :mime :external)
      (loop (multiple-value-bind (next-p sym) (next)
              (unless next-p (return))
              (when (find-class sym nil)
                (pushnew sym types))))
    types)))
;;; (list-mime-types)


(defun clear-mime-types ()
  (dolist (type (list-mime-types))
    (setf (find-class type nil) nil)
    (unintern type :mime)))


;;;
;;; simple cloning

(unless (fboundp 'de.setf.utility::clone-instance)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(de.setf.utility::clone-instance
              de.setf.utility::initialize-clone
              de.setf.utility::clone-instance-as)
            :de.setf.utility))

  (defgeneric de.setf.utility::initialize-clone (old new &rest args)
    (:documentation
      "invoke shared-initialize on the collected initargs to initialize slots
       prior to copying from the instance to the clonein order to override the existing
       slot values and preclude unwanted deep cloning.")

    (:method ((old standard-object) (new standard-object) &rest args)
      (apply #'shared-initialize new t args)
      new)

    (:method de.setf.utility::initialize-clone ((old mime:text/*) (new mime:text/*) &rest args
                                                &key (charset (slot-value old 'charset)))
             (apply #'call-next-method old new
                    :charset charset
                    args)))
  (defgeneric de.setf.utility::clone-instance-as (instance class &rest initargs)
  (:method ((instance standard-object) (class symbol) &rest initargs)
           (apply #'de.setf.utility::clone-instance-as instance (find-class class) initargs))
  (:method ((instance standard-object) (class class)
                                       &rest initargs &aux new)
           "observing that both mcl and allegro support allocate-instance
            on all of {built-in,funcallable-standard,standard,structure}class
            the specialization for this function can be relaxed accordingly."
           (declare (dynamic-extent initargs))
           (setf new (allocate-instance class))
           ;; pass any initargs through and augment them on the
           ;; way to the ultimate shared-initialize call
           (apply #'de.setf.utility::initialize-clone instance new initargs)
           new))

  (defgeneric de.setf.utility::clone-instance (instance &rest args)
    (:documentation 
      "reproduce a given instance.")

    (:method ((instance standard-object) &rest args)
      (apply #'de.setf.utility::clone-instance-as instance (class-of instance)
             args)))
  )


(defmethod content-encoding ((mime-type mime:text/*) &rest args)
  (declare (dynamic-extent args) (ignore args))
  (content-encoding (mime-type-charset mime-type)))


:mime

