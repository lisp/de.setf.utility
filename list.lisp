;;; -*- Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' library component.
;;;  It defines list utility operators

;;;  Copyright 2009, 2009, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of version 3 of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with `de.setf.utility`, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).


;;; 2006-02-09  janderson  added &aux collection to parse-lambda-list
;;; 2009-03-29  janderson  incorporated partial-order-sort from old system
;;;  definition tools


(in-package :de.setf.utility.implementation)

(modpackage :de.setf.utility
  (:export
   :collate
   :destructuring-keys
   :lambda-list-arity
   :map-plist
   :do-plist
   :collect-list
   :with-gensyms
   :gensym-macrolet
   :parse-lambda-list
   :permutations
   :partial-order-sort
   ))


(defmacro with-gensyms (variables &rest forms)
  (setf forms
        (sublis (mapcar #'(lambda (variable) (cons variable (gensym (string variable)))) variables)
                forms))
  (if (rest forms) (cons 'progn forms) (first forms)))

(defmacro gensym-macrolet (variables &rest forms)
  (setf forms
        (sublis (mapcar #'(lambda (variable) (cons variable (gensym (string variable)))) variables)
                forms))
  (if (rest forms) (cons 'progn forms) (first forms)))

#+digitool
(pushnew '(gensym-macrolet . 1) *fred-special-indent-alist* :key #'first)


(defun map-plist (function plist &aux key)
  (declare (type cons plist) (optimize (speed 3) (safety 0)))
  (loop (unless (consp plist) (return))
        (setf key (first plist) plist (rest plist))
        (unless (consp plist) (return))
        (funcall function key (first plist))
        (setf plist (rest plist))))

#| the tail-recursive form takes 30% longer
(defun map-plist (function plist &aux key)
  (declare (type cons plist) (optimize (speed 3) (safety 0)))
  (when (consp plist)
    (setf key (first plist) plist (rest plist))
    (when (consp plist)
      (funcall function key (first plist))
      (map-plist function (rest plist)))))|#

(defmacro do-plist ((p-var v-var list &optional result) &rest body
                    &aux (l-var (if (symbolp list) list (gensym))))
  (let ((form `(do ((,p-var (pop ,l-var) (when (rest ,l-var) (pop ,l-var)))
                    (,v-var (pop ,l-var) (pop ,l-var)))
                   ((null ,p-var) ,@(when result `(,result)))
                 ,@body)))
    (if (eq l-var list)
      form
      `(let ((,l-var ,list)) ,form))))

(defmacro collect-list ((collector &key (predicate 'identity) (finally 'rest) key last) &rest body)
  (let ((list (gensym "LIST-"))
        (end (gensym "END-")))
    `(let* ((,list (list nil)) (,end ,list))
       (block nil
         (flet ((,collector (datum)
                  ,@(when key `((setf datum (,key datum))))
                  ,(case predicate
                     ((nil) `(setf (rest ,end) (list datum) ,end (rest ,end)))
                     (identity `(when datum (setf (rest ,end) (list datum) ,end (rest ,end))))
                     (t `(when (funcall ,predicate datum) (setf (rest ,end) (list datum) ,end (rest ,end))))))
                ,@(when last
                    `(((setf ,last) (datum)
                       (setf (rest ,end) datum)))))
           ,@body))
       (,finally ,list))))

#+digitool
(setf (ccl:assq 'collect-list *fred-special-indent-alist*) 1)


(defun collate (key list &key (test #'eq))
  (let ((result nil) (entry nil) (key-value nil))
    (dolist (element list)
      (setf key-value (funcall key element))
      (setf entry (assoc key-value result :test test))
      (if entry
        (push element (rest entry))
        (push (list key-value element) result)))
    (dolist (entry result)
      (setf (rest entry) (nreverse (rest entry))))
    result))

;(collate #'symbol-package '(+ - :test :one *))


;; nb. lispworks does not tolerate let variables with '&' initials
(defun parse-lambda-list (list)
  (let ((.optional nil)
        (.rest nil)
        (.key nil)
        (.allow-other-keys nil)
        (.aux nil)
        (positional nil)
        (state nil))
    (dolist (parameter list)
      (case parameter
        ((&optional &rest &key &aux) (setf state parameter))
        (&allow-other-keys (setf .allow-other-keys t))
        (t (case state
             (&optional (push parameter .optional))
             (&rest (setf .rest parameter))
             (&key (push parameter .key))
             (&aux (push parameter .aux))
             (t (push parameter positional))))))
    `(,(nreverse positional)
      ,@(when .optional `(:optional ,(nreverse .optional)))
      ,@(when .rest `(:rest , .rest))
      ,@(when .key `(:key ,(nreverse .key)))
      ,@(when .allow-other-keys '(:allow-other-keys t))
      ,@(when .aux `(:aux ,(nreverse .aux))))))

(defun lambda-list-arity (lambda-list)
  (length (first (parse-lambda-list lambda-list))))

(defgeneric permutations (sequence length)
  (:method ((list list) length)
           (labels ((permute (remaining-count)
                      (if (zerop remaining-count)
                        (mapcar #'list list)
                        (let ((base (permute (1- remaining-count)))
                              (new nil))
                          (dolist (permutation base new)
                            (dolist (element list)
                              (push (cons element permutation) new)))))))
             (permute (1- length)))))
;(permutations '(:a :s :d) 2)



(defun partial-order-sort
       (sequence predicate &key key
                 &aux (keys (if key (map 'vector key sequence) sequence))
                      (length (length sequence)))
  "sort a series for which the order relation is partial.
 SEQUENCE : sequence : for which to destrictively sort the elements
 PREDICATE : (FUNCTION (t t) BOOLEAN)
 :KEY : (FUNCTION (T) T)

 performs a destructive sort of the sequence's elements, where by all
 element are pairwise compared, in order to ensure the a partial
 relation is observed."
  (dotimes (i1 length) 
    (do* ((i2 (1+ i1) (1+ i2)))
         ((>= i2 length))
      (when (and (funcall predicate (elt keys i2) (elt keys i1))
                 (not (funcall predicate (elt keys i1) (elt keys i2))))
        (rotatef (elt sequence i2) (elt sequence i1)))))
  sequence)

(defmacro destructuring-keys (lambda-list value &body body-arg &environment env)
  (let* ((body (member-if #'(lambda (x) (not (and (consp x) (eq (first x) 'declare)))) body-arg))
         (declarations (ldiff body-arg body)))
    (destructuring-bind (required-arguments &key key rest) (parse-lambda-list lambda-list)
      (assert (null required-arguments))
      (unless rest
        (setf rest
              (if (and (symbolp value) (eq (macroexpand-1 value env) value)) value (gensym "rest"))))
      (setf key (mapcar #'(lambda (key)
                            (etypecase key
                              (symbol `((,(intern (symbol-name key) :keyword) ,key) nil))
                              (cons (destructuring-bind (key value) key
                                      (etypecase key
                                        (symbol `((,(intern (symbol-name key) :keyword) ,key) ,value))
                                        (cons `(,key ,value)))))))
                        key))
      `(let (,@(unless (eq rest value) `((,rest ,value)))
             ,@(loop for ((nil variable) value) in key collect `(,variable ,value)))
         ,@declarations
         (loop (case (first ,rest)
                 ,@(loop for ((key variable) nil) in key
                         collect `(,key (pop ,rest) (setf ,variable (pop ,rest))))
                 (t (return))))
         ,@body))))

(assert (equalp (macroexpand-1 '(destructuring-keys (&key key1 key2) forms (declare (optimize)) (list key1 key2)))
                '(let ((key1 nil) (key2 nil))
                   (declare (optimize))
                   (loop (case (first forms)
                           (:key1 (pop forms) (setf key1 (pop forms)))
                           (:key2 (pop forms) (setf key2 (pop forms))) (t (return))))
                   (list key1 key2))))

(assert (equalp (macroexpand-1 '(destructuring-keys (&rest rest-forms &key key1 key2) forms (list forms rest-forms key1 key2)))
                '(let ((rest-forms forms) (key1 nil) (key2 nil))
                   (loop (case (first rest-forms)
                           (:key1 (pop rest-forms) (setf key1 (pop rest-forms)))
                           (:key2 (pop rest-forms) (setf key2 (pop rest-forms)))
                           (t (return))))
                   (list forms rest-forms key1 key2))))

(assert (equalp (subst-if t #'(lambda (x) (and (symbolp x) (null (symbol-package x))))
                          (macroexpand-1 '(destructuring-keys (&key key1 key2) (some forms) (declare (optimize)) (list key1 key2))))
                '(let ((t (some forms)) (key1 nil) (key2 nil))
                   (declare (optimize)) 
                   (loop (case (first t)
                           (:key1 (pop t) (setf key1 (pop t)))
                           (:key2 (pop t) (setf key2 (pop t)))
                           (t (return))))
                   (list key1 key2))))
:de.setf.utility
