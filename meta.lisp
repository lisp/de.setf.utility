;;;;										;
;;;; (c) 2001 by Jochen Schmidt.
;;;; (c) 2003 james anderson
;;;;
;;;; File:            meta.lisp
;;;; Revision:        1.0.1++
;;;; Description:     A simple parsing technique
;;;; Date:            30.05.2002
;;;; Authors:         Jochen Schmidt
;;;; Tel:             (+49 9 11) 47 20 603
;;;; Email:           jsc@dataheaven.de
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER 
;;;; EXPRESSED NOR IMPLIED WARRANTIES -  THIS INCLUDES, BUT 
;;;; IS NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;; AND FITNESS FOR A PARTICULAR PURPOSE.IN NO WAY ARE THE
;;;; AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
;;;; LOSS OF USE, DATA, OR PROFITS OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;;
;;;; NOTE:
;;;; This code is based on the well known paper "Pragmatic Parsing in Common Lisp"
;;;; of Henry G. Baker. You can find it at:
;;;;
;;;;    http://linux.rice.edu/~rahul/hbaker/Prag-Parse.html
;;;;
;;;; The parsing technique Baker describes in his paper goes back to:
;;;;
;;;;     Schorre, D.V.  "META II: A Syntax-Oriented Compiler Writing Language".
;;;;       Proc. 19'th Nat'l. Conf. of the ACM (Aug. 1964),D1.3-1-D1.3-11.
;;;;
;;;;
;;;; Nuernberg, 01.Jul.2001 Jochen Schmidt
;;;; 20030829 james.anderson@setf.de
;;;;  integrate standard lisp expressions and allow s-expression patterns.
;;;;  unify generator as one abstract engine which accepts functions (peek-char, read-char,
;;;;  read, position, etc) to access various concrete source (stream, strings, buffers).
;;;;  change readtable management such that it is effective during macroexpansion only.
;;;; 20031124 james.anderson@setf.de
;;;;  included + in the read-macro,
;;;;  added reinstatement of standard readtable to !( ... ) form, which permits use of
;;;;  more standard macro characters, eg #\* for zero-or-more and #\? for one-or-more
;;;;  while $ becomes object match and @ remains character match
;;;; 20031214.jaa repaired compile-it to recognize t form which is needed for ? macro
;;;;  repaired printer for #\* form
;;;
;;; simple description
;;; @   <char>              match character
;;; $   predicate <form>    satisfy predicate 
;;; *   <form>              zero-or-more
;;; +   <form>              one or more
;;; ?   <form>              one or zero
;;; {   <forms>             or
;;; [   <forms>             and
;;; !   <sexp>              eval

(defpackage :de.setf.utility.meta
  (:nicknames :meta)
  (:use #:common-lisp)
  (:export
   #:with-string-meta
   #:with-list-meta
   #:with-meta
   #:with-stream-meta
   #:end-p
   #:meta-position
   #:match
   )
  )

(in-package :de.setf.utility.meta)

(defVar *meta-binding* (gensym "DATUM-"))

(defstruct (meta
            (:print-function
             (lambda (m s d &aux (char (meta-char m)) (form (meta-form m)))
               (declare (ignore d))
               (ecase char
                 ((#\@ #\! #\$ #\? #\+ #\*) (format s "~A~s" char form))
                 (#\[ (format s "[~{~s~^ ~}]" form))
                 (#\{ (format s "{~{~s~^ ~}}" form))))))
    char
    form)

(defun compileit (x &aux form)
  "decide how to match based on the type of the pattern and compile it accordingly.
   a meta instance is an interned expression and comprises an operator, denoted by a character, and
   a form which declares the pattern to match. the operator is replaced with the equivalent symbol
   to generate an expression and that expression is returned.
   a list is is returned verbatim for subsequent macroexpansion.
   any other data is taken as a matching literal."
  (setf form
        (etypecase x
          ((member t nil) x)
          (cons x)
          (meta
           (compileit (ecase (meta-char x)
                        (#\[ (cons '?and (meta-form x)))
                        (#\@ (cons '?char-typep (meta-form x)))
                        (#\! (list '?eval (meta-form x)))
                        (#\{ (cons '?or (meta-form x)))
                        (#\$ (cons '?typep (meta-form x)))
                        (#\? (list '?? (meta-form x)))
                        (#\+ (list '?+ (meta-form x)))
                        (#\* (list '?* (meta-form x))))))
          ((or string character) `(?= ,x))))
  form)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *meta-readtable* (copy-readtable))
  (defparameter *standard-readtable* (copy-readtable *readtable*))
  
  (defun meta-reader (s c) (make-meta :char c :form (read s)))
  (defun meta-read-exclamation (s c &aux (*readtable* *standard-readtable*))
    (make-meta :char c :form (read s)))
  (defun meta-read-brace (s c)
    (make-meta :char c :form (read-delimited-list #\} s t)))
  (defun meta-read-bracket (s c)
      (make-meta :char c :form (read-delimited-list #\] s t)))
  
  (mapc #'(lambda (c) (set-macro-character c #'meta-reader nil *meta-readtable*))
        '(#\@ #\$ #\? #\+ #\*))

  (set-macro-character #\! 'meta-read-exclamation nil *meta-readtable*)
  
  (set-macro-character #\{ 'meta-read-brace nil *meta-readtable*)
  
  (set-macro-character #\[ 'meta-read-bracket nil *meta-readtable*)
  
  (mapc #'(lambda (c) (set-macro-character c (get-macro-character #\))  nil *meta-readtable*))
	'(#\] #\}))
  )


(defmacro match-char-type (predicate binding)
  (etypecase predicate
    (character `(when (eql ,predicate (meta-peek-char)) (setf ,binding (meta-read-char))))
    (string
     (case (char predicate 0)
       (#\^ `(unless (find (meta-peek-char) ,(subseq predicate 1)) (setf ,binding (meta-read-char))))
       (#\\ `(when (find (meta-peek-char) ,(subseq predicate 1)) (setf ,binding (meta-read-char))))
       (t `(when (find (meta-peek-char) ,predicate) (setf ,binding (meta-read-char))))))
    ((or cons symbol) `(when (,predicate (meta-peek-char)) (setq ,binding (meta-read-char))))))

(defmacro match-type (predicate binding)
  (etypecase predicate
    ((or cons symbol) `(when (,predicate (meta-peek)) (setq ,binding (meta-read))))))

(defMacro match-literal (literal)
  (etypecase literal
    (character
     `(when (eql ,literal (meta-peek-char)) (meta-read-char)))
    (string
     (let ((old-index-symbol (gensym "OLD-INDEX-")))
       `(let ((,old-index-symbol (meta-position)))
          (or (and ,@(map 'list #'(lambda (c) `(match-literal ,c)) literal))
              (progn (meta-position ,old-index-symbol) nil)))))))

(defun read-from-meta-string (string)
  (let ((*readtable* *meta-readtable*))
    ;; ! don't forget, the strings must have double \\ escapes
    ;; and escape quotes
   (read-from-string (concatenate 'string string " "))))


;; format wthout object matching
(defmacro with-meta-old ((&key peek-char read-char read position)
                     &body body
                     &aux form)
  (setf form
        `(let* ((,*meta-binding* nil))
           (labels ((meta-read () (if ,*meta-binding* (shiftf ,*meta-binding* nil)
                                      (funcall ,read)))
                    (meta-peek () (if ,*meta-binding* ,*meta-binding*
                                      (setf ,*meta-binding* (meta-read)))))
             (macrolet ((meta-position (&rest args) (list* 'funcall ',position args))
                        (meta-peek-char () (list 'funcall ',peek-char))
                        (meta-read-char () (list 'funcall ',read-char))
                        (end-p () '(null (meta-peek-char)))
                        (match (x)
                          (etypecase x
                            (string (compileit (read-from-meta-string x)))
                            (meta (compileit x))
                            (list (compileit x))))
                        (?and (&rest forms) `(and ,@(mapcar #'(lambda (f) (compileit f)) forms)))
                        (?char-typep (type-predicate binding) `(match-char-type ,type-predicate ,binding))
                        (?eval (form) form)
                        (?or (&rest forms) `(or ,@(mapcar #'(lambda (f) (compileit f)) forms)))
                        (?typep (type-predicate binding) `(match-type ,type-predicate ,binding))
                        (?? (form) `(?or ,form t))
                        (?* (form) `(not (do () ((not ,(compileit form))))))
                        (?+ (form) `(?and ,form (?* ,form)))
                        (?= (value) `(match-literal ,value)))
               ,@body))))
  form)

(defmacro with-meta ((&key peek-char read-char (read read-char) (peek peek-char) position)
                      &body body
                      &aux form)
  (setf form
        `(macrolet ((meta-position (&rest args) (list* 'funcall ',position args))
                      (meta-peek () (list 'funcall ',peek))
                      (meta-read () (list 'funcall ',read))
                      (meta-peek-char () (list 'funcall ',peek-char))
                      (meta-read-char () (list 'funcall ',read-char))
                      (end-p () '(null (meta-peek-char)))
                      (match (x)
                        (etypecase x
                          (string (compileit (read-from-meta-string x)))
                          (meta (compileit x))
                          (list (compileit x))))
                      (?and (&rest forms) `(and ,@(mapcar #'(lambda (f) (compileit f)) forms)))
                      (?char-typep (type-predicate binding) `(match-char-type ,type-predicate ,binding))
                      (?eval (form) form)
                      (?or (&rest forms) `(or ,@(mapcar #'(lambda (f) (compileit f)) forms)))
                      (?typep (type-predicate binding) `(match-type ,type-predicate ,binding))
                      (?? (form) `(?or ,form t))
                      (?* (form) `(not (do () ((not ,(compileit form))))))
                      (?+ (form) `(?and ,form (?* ,form)))
                      (?= (value) `(match-literal ,value)))
             ,@body))
  form)


(defGeneric meta-position (source &optional new)
  (:method ((stream file-stream) &optional new)
           (if new
             (file-position stream new)
             (file-position stream)))
  (:method ((stream string-stream) &optional new)
           #+ccl(ccl::stream-position stream new)
           #+allegro(if new
                        (setf (slot-value stream 'excl::buffpos) new)
                      (slot-value stream 'excl::buffpos))
           #-(or ccl allegro)(error "no stream-position definition"))
  (:method ((stream stream) &optional new)
           (declare (ignore new))
           (error "no stream-position definition")))





(defmacro with-stream-meta ((stream) &rest body &aux (stream-binding (gensym "STREAM-")))
 `(let ((,stream-binding ,stream))
    (flet ((p-c () (peek-char nil ,stream-binding nil))
           (r-c () (read-char ,stream-binding nil))
           (r () (read ,stream-binding nil))
           (p (&optional position) (meta-position ,stream-binding position)))
      (declare (dynamic-extent #'p-c #'r-c #'r #'p))
      (with-meta (:peek-char #'p-c :read-char #'r-c :read #'r :position #'p)
        ,@body))))



(defmacro with-string-meta ((string-buffer &key (start 0) end) &body body &aux (string-binding (gensym "STRING-")))
  `(let* ((,string-binding ,string-buffer)
          (index ,start)
          (end ,(or end `(length ,string-binding))))
     (declare (fixnum index end)
              (type simple-base-string ,string-binding))
     (flet ((p-c () (when (< index end) (char ,string-binding index)))
            (r-c () (when (< index end)
                      (prog1 (char ,string-binding index)
                        (incf index))))
            (r () (multiple-value-bind (value new-index)
                                       (read-from-string ,string-binding nil nil
                                                         :start index :end end)
                    (setf index new-index)
                    value))
            (p (&optional position) (if position (setf index position) index)))
       (with-meta (:peek-char #'p-c :read-char #'r-c :read #'r :position #'p)
         ,@body))))


#+mcl ;MCL won't compile to a file without this.
(defmethod make-load-form ((m meta) &optional env)
  (declare (ignore env))
  `(make-meta :char ,(meta-char m) :form ',(meta-form m)) )


(provide :de.setf.utility.meta)

#|



;; with explicit eval expresions
(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (string)
                    (and
                     (match
                      "[{#\\+ [#\\- !(setq s -1)] []}
                            @(\"0123456789\" d) !(setq n (digit-char-p d))
                            *[@(\"0123456789\" d) !(setq n (+ (* n 10) (digit-char-p d)))]]")
                     (* s n))))

;; with plain forms
(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (string)
                    (and
                     (match
                      "[{#\\+ [#\\- (setq s -1)] []}
                            @(\"0123456789\" d) !(setq n (digit-char-p d))
                            *[@(\"0123456789\" d) !(setq n (+ (* n 10) (digit-char-p d)))]]")
                     (* s n))))

;; with an end test
(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (string)
                    (and
                     (match
                      "[{#\\+ [#\\- (setq s -1)] []}
                            @(\"0123456789\" d) !(setq n (digit-char-p d))
                            *[@(\"0123456789\" d) !(setq n (+ (* n 10) (digit-char-p d)))]]")
                     (end-p)
                     (* s n))))

;; with +
(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (string)
                    (and
                     (match
                      "[{#\\+ [#\\- (setq s -1)] []}
                       +[@(\"0123456789\" d) !(setq n (+ (* n 10) (digit-char-p d)))]]")
                     (end-p)
                     (* s n))))

(parse-int "+123")
(parse-int "-123")
(parse-int "-123.")
(parse-int "-")


(defun parse-float (string &aux (s +1) (es +1) (i 0) (f 0) (e 0) (m #\e) (f-count 0) (i-count 0) (e-count 0) (v 0) d)
  (with-string-meta (string)
      (and
       (match
        "[{#\\+ [#\\- !(setq s -1)] []}
          *[@(\"0123456789\" d) !(setf i (+ (* i 10) (digit-char-p d)) i-count (1+  i-count))]
          {#\\. []}
          *[@(\"0123456789\" d) !(setf f (+ (* f 10) (digit-char-p d)) f-count (1+ f-count))]
          {@(\"eEsSdDfFlL\" m) []}
          {#\\+ [#\\- !(setq es -1)] []}
          *[@(\"0123456789\" d) !(setf e (+ (* e 10) (digit-char-p d)) e-count (1+ e-count))]
          ]")
       (when (> (+ f-count i-count) 0)
         (when (> f-count 0) (setf f (/ f (expt 10 f-count))))
         (setf v (+ i f))
         (when (plusp e-count) (setf v (* v (expt 10 (* es e)))))
         (when (< s 0) (setf v (- v)))
         (case m
           ((#\E #\e) (float v 0.0e0))
           ((#\S #\s) (float v 0.0s0))
           ((#\D #\d) (float v 0.0d0))
           ((#\F #\f) (float v 0.0s0))
           ((#\L #\l) (float v 0.0s0)))))))
                     
                            
(parse-float "0.0")
(parse-float "0E0")
(parse-float "-.0")
(parse-float "6.02E+23")

(mapcar #'parse-float
        '("1" "-1" "1034" "-364" "3.5333" "2.4E4" "6.8d3" "13.09s3" "35.66L5" "21.4f2"))

|#
