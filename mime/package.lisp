;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

;;;
;;; A CLOS model for MIME types.
;;;
;;; See : (among others)
;;;  http://en.wikipedia.org/wiki/MIME
;;;  http://tools.ietf.org/html/rfc2046 : (MIME) Part Two: Media Types
;;;  http://tools.ietf.org/html/rfc2049 : (MIME) Part Five: Conformance Criteria and Examples
;;;
;;; 20100106 james.anderson : added octet-stream

(in-package :de.setf.utility.implementation)

(modPackage :de.setf.utility
  (:use-only )
  (:use-by :de.setf.utility.implementation)
  (:export
   :*class.content-encoding*
   :*mime-type-package*
   :clone-instance
   :compute-charset-codecs
   :content-encoding
   :content-encoding-byte-decoder
   :content-encoding-byte-encoder
   :content-encoding-encoded-code-point-size
   :content-encoding-name
   :def-mime-type
   :def-mime-type-key
   :defmimetype
   :defmimetypekey
   :intern-mime-type-key
   :major-mime-type
   :mime-type
   :mime-type-charset
   :mime-type-expression
   :mime-type-major-type
   :mime-type-minortype
   :mime-type-p
   :minor-mime-type
   ))

(defPackage :mime
  (:nicknames :de.setf.utility.mime.type)
  (:use )

  (:documentation
   "The MIME package comprises symbols which name mime types.
 A name is present for each concrete, major, and minor type, whereby major
 and minor types are present both as stems and as .../*, */... generalizations.
 Each concrete type is reified as an instance of the respective concrete class,
 which is the global value of the respective class name. The class precedence
 is arranged such that the generalizations are present as super-classes. Of
 which the major-type generaalization preceeds the minor-type.")

  (:import-from :de.setf.utility :mime-type)
  (:export
   :*
   :*/*
   :*/plain
   :*/xhtml
   :*/xml
   :*/text
   :application
   :application/*
   :application/json
   :application/octet-stream
   :application/xml
   :html
   :image
   :json
   :mime-type
   :octet-stream
   :plain
   :svg
   :svg+xml
   :text
   :text/*
   :text/plain
   :text/xhtml
   :text/html
   :text/xml
   :xhtml
   :xhtml+xml
   :xml
   ))

