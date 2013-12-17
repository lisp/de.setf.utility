;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;  This file extends the package definition for 'de.setf.utility' Common Lisp library
;;;  to incorporate names for mime-related operators and classes. It also defined a `MIME` package
;;;  to comprehend names for the mime types themselves.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; Copyright 2009 [james anderson](mailto:james.anderson@setf.de)
;;; 20100106 james.anderson : added octet-stream


(modpackage :de.setf.utility
  (:use-only )
  (:use-by :de.setf.utility.implementation)
  (:export
   :*class.content-encoding*
   :*mime-type-package*
   :binary-mime-type-p
   :canonical-mime-type
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
   :encode-string
   :experimental-mime-type
   :intern-mime-type-key
   :major-mime-type
   :mime-type
   :mime-type-charset
   :mime-type-canonical-mime-type
   :mime-type-expression
   :mime-type-major-type
   :mime-type-minor-type
   :mime-type-file-type
   :mime-type-p
   :minor-mime-type
   :size-string
   ))

(defpackage :mime
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

  (:import-from :de.setf.utility
                :binary-mime-type-p
                :mime-type
                :mime-type-charset
                :mime-type-expression
                :mime-type-p
                :size-string)
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
   :application/rdf+xml
   :binary
   :binary-mime-type-p
   :graphviz
   :html
   :image
   :json
   :markdown
   :mime-type
   :mime-type-charset
   :mime-type-expression
   :mime-type-p
   :n3
   :octet-stream
   :plain
   :rdf
   :rdf+xml
   :size-string
   :svg
   :svg+xml
   :text
   :text/*
   :text/markdown
   :text/plain
   :text/xhtml
   :text/html
   :text/vnd.graphviz
   :text/x-graphviz
   :text/xml
   :turtle
   :vnd.graphviz
   :x-graphviz
   :xhtml
   :xhtml+xml
   :xml
   ))

