;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

;;;  This file part of the 'de.setf.utility' Common Lisp library.
;;;  It defines a method combination to combine arbitrary named methods

;;;  Copyright 2003,2004,2009,2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; 20041010 janderson changed logic of qualifier matching to allow a method to
;;;   match multiple names and play all roles simultaneously
;;; 2005-09-19  janderson  added verbose-p
;;; 2010-03-15  janderson   consolidated denominated combinations in this file

;;;
;;; contents
;;;
;;; method combinations
;;;  denominated
;;;  denominated-progn
;;;
;;;  The two method combination use the results of one qualified method group to compute the
;;; effectively applicable methods in terms of the respective qualifiers. They are differ in that
;;; - denominate-progn permits multiple qualifiers, but only the progn operator, has no unqualified methods,
;;;   and allows :before and :after methods in additon to :around.
;;; - denominated allows exactly one qualifier per method, allows progn, and, or operators,
;;;   required unqualified primary methods, and support :around, but neither :before nor :after groups.

(define-method-combination denominated (&key (order :most-specific-first)
                                             (verbose nil))
                           ((around (:around) :order :most-specific-first)
                            (qualifying (:qualifying) :required t :order :most-specific-first)
                            (primary () :required t :order :most-specific-first)
                            (qualified-methods * :order :most-specific-last))
  (:generic-function function)

  "combine all qualified methods. no unqualified method is permitted.
   the method qualifiers are arbitrary. the initial set of applicable
   methods is grouped by qualifier. the qualifier groups are then arranged
   as specified by the applicable qualifiers for the given function and arguments.
   for a given generic function definition, the qualifiers may be a literal
   list, or it may be a function designator. in the latter cases, that function
   is applied to a list* of the generic function and the specializer classes,
   as derived from the most specific method :denominative.
   methods within a group are then ordered according to the :order specified
   for the given function. when no group matches, a t group is used if applicable,
   otherwise an error is signaled."
  (assert (member order '(:most-specific-first :most-specific-last)))

  (flet ((eliminate (these from)
           "In case * matches everything, not just those unmatched by others;
            use remove to retain order."
           (dolist (this these) (setf from (remove this from)))
           from))
    (setf qualified-methods
          (eliminate primary
                     (eliminate around
                                (eliminate qualifying qualified-methods)))))
          
  (let* ((grouped-methods nil)
         (form nil)
         (effective-qualifiers-form `(call-method ,(first qualifying) ,(rest qualifying))))
    
    ;; group the methods by qualifier, result is most-specific-first within each group
    (dolist (method qualified-methods)
      (let ((method-qualifiers (method-qualifiers method)))
        (cond ((= 1 (length method-qualifiers))
               (let* ((qualifier (first method-qualifiers))
                      (group (assoc qualifier grouped-methods)))
                 (if group
                   (push method (rest group))
                   (push (list qualifier method) grouped-methods))))
              (t
               (invalid-method-error method "method must have exactly one qualifier.")))))
    
    ;; sort the groups by applicable qualifier
    (when (eq order :most-specific-last)
      (setf grouped-methods (mapcar #'(lambda (group) (cons (first group) (reverse (rest group))))
                                    grouped-methods)))
    ;; iterate over the computed list of qualifiers and apply the respective method
    ;; group. return the result of the last group run
    (setf form `(let ((last nil))
                  (dolist (qualifier ,effective-qualifiers-form last)
                    (case qualifier
                      ,@(mapcar #'(lambda (group)
                                    (destructuring-bind (qualifier . methods) group
                                      `(,qualifier
                                        (setf last (call-method ,(first methods) ,(rest methods))))))
                              grouped-methods)))))
    ;; primaries are run once, after the qualified groups
    (setf form `(prog1 ,form (call-method ,(first primary) ,(rest primary))))
    ;; around methods wrap everything and over-ride the result
    (when around
      (setf form `(call-method ,(first around)
                               (,@(rest around) (make-method ,form)))))

    (when verbose
      (format *trace-output* "~%~s: ~s:~%~:W" function
              `(:around ,around :primary ,primary :qualified ,qualified-methods) form))
    form))


(define-method-combination denominated-progn (&key (order :most-specific-first)
                                                   (verbose-p nil)
                                                   (verbose verbose-p))
                           ((around (:around) :order :most-specific-first)
                            (after (:after) :order :most-specific-last)
                            (before (:before) :order :most-specific-first)
                            (between (:between) :order :most-specific-first)
                            (qualifying (:qualifying) :required t :order :most-specific-first)
                            (qualified-methods * :required t :order :most-specific-last))
  (:generic-function function)
  "the qualified methods are combined as the base methods - no unqualified method is permitted.
   the method qualifiers are arbitrary. in addition :around :before, and :after methods are
   incorporated as for a standard method combination. in addition :between methods are interposed between
   each qualified method.
   the initial set of applicable methods  is grouped by qualifier, sorted according to the combination's
   :order specification. the qualifier groups are then arranged as specified by the applicable qualifiers
   as computed by the :qualifying methods."

  (assert (member order '(:most-specific-first :most-specific-last)))
  (flet ((eliminate (these from)
           "In case * matches everything, not just those unmatched by others;
            use remove to retain order."
           (dolist (this these) (setf from (remove this from)))
           from)
         (call-methods (methods)
           (mapcar #'(lambda (method) `(call-method ,method)) methods)))
    (let ((primary (eliminate after
                              (eliminate around
                                         (eliminate before
                                                    (eliminate qualifying
                                                               (eliminate between qualified-methods))))))
          (grouped-methods nil)
          (form nil)
          (effective-qualifiers-form `(call-method ,(first qualifying) ,(rest qualifying)))
          (between-form (when between `(call-method ,(first between) ,(rest between)))))

      (when verbose
        (format t "~%:around: ~s~%:before: ~s~%between: ~s~%qualifying: ~s~%qualified: ~s~%:after: ~s"
                around before between qualifying qualified-methods after))

      
      ;; group the methods by applicable qualifier, result is least-specific-first
      ;; within arbitrary specializer order.
      ;; allow multiple qualifiers to select for all matching names
      (dolist (method primary)
        (dolist (qualifier (method-qualifiers method))
          (let ((group (assoc qualifier grouped-methods)))
            (if group
              (push method (rest group))
              (push (list qualifier method) grouped-methods)))))

      ;; reverse groups if desired to get t groups back in most-specific-last order
      (when (eq order :most-specific-last)
        (setf grouped-methods (mapcar #'(lambda (group) (cons (first group) (reverse (rest group))))
                                      grouped-methods)))

      (when verbose
        (format *trace-output* "~%grouped: ~:w" grouped-methods))
      (setf form `(let ((first-p t) (last nil))
                    (dolist (qualifier ,effective-qualifiers-form last)
                      ,@(when between-form
                          `((if first-p
                              (setf first-p nil)
                              ,between-form)))
                      (case qualifier
                         ,@(mapcar #'(lambda (group)
                                       (destructuring-bind (qualifier . methods) group
                                         `(,qualifier
                                           (setf last (call-method ,(first methods) ,(rest methods))))))
                                   grouped-methods)))))

      (when before (setf form `(progn ,@(call-methods before) ,form)))
      (when after (setf form `(multiple-value-prog1 ,form ,@(call-methods after))))
      (when around (setf form `(call-method ,(first around) (,@(rest around) (make-method ,form)))))

      (when verbose
        (format *trace-output* "~%~s:~%~:W" function form))
      form)))



(defgeneric .test-denominated. (controller)
  (:method-combination denominated)
  (:method :around ((value t)) (cons :around (call-next-method)))
  (:method ((value t)) value)
  (:method blank ((value t)) (return-from .test-denominated. :blank))
  (:method plus ((value t))  :plus)
  (:method minus ((value t)) :minus)
  (:method zero ((value t)) :zero)

  (:method :qualifying ((value t)) '(t?))
  (:method :qualifying ((value string))
           (append (when (null (find #\space value :test-not #'eql))'(blank))
                   (call-next-method)))
  (:method :qualifying ((value number))
           (append (cond ((plusp value) '(plus)) ((minusp value) '(minus)) (t '(zero)))
                   (call-next-method))))

(handler-case
  (assert (equalp (list (.test-denominated. -1) (.test-denominated. 0) (.test-denominated. 1)
                        (.test-denominated. "a") (.test-denominated. " ")
                        (.test-denominated. 'x))
                  '((:around . :minus) (:around . :zero) 
                    (:around . :plus) (:around) (:around . :blank) (:around))))
  (error (c)
    (warn "denominated combination test signaled an error: ~a" c)))

(makunbound '.test-denominated.)
  

;;; for reference, the version which uses a method to compute the effective qualifiers at the point
;;; when the computation is computed.

#+(or)
;;; rewritten to interpret the constituency on each call from a maximal set
(define-method-combination denominated (&key (operator 'progn)
                                             (qualifiers (error "qualifiers required for denominated method combination."))
                                             (order :most-specific-first)
                                             (if-not-applicable nil)
                                             (verbose nil))
                           ((around (:around) :order :most-specific-first)
                            (denominative (:denominative) :required t :order :most-specific-first)
                            (default (t) :order :most-specific-first)
                            (qualified-methods * :required t :order :most-specific-last))
  (:generic-function function)
  "combine all qualified methods. no unqualified method is permitted.
   the method qualifiers are arbitrary. the initial set of applicable
   methods is grouped by qualifier. the qualifier groups are then arranged
   as specified by the applicable qualifiers for the given function and arguments.
   for a given generic function definition, the qualifiers may be a literal
   list, or it may be a function designator. in the latter cases, that function
   is applied to a list* of the generic function and the specializer classes,
   as derived from the most specific method :denominative.
   methods within a group are then ordered according to the :order specified
   for the given function. when no group matches, a t group is used if applicable,
   otherwise an error is signaled."
  (ecase if-not-applicable ((nil)) (:error))
  (ecase order (:most-specific-first ) (:most-specific-last ))
  (let ((grouped-methods nil)
        (group nil)
        (applicable-qualifiers nil)
        (method-qualifiers nil)
        (qualifier nil)
        (form nil))
    ;; collect the qualifier constraints for the given arguments and function
    (setf applicable-qualifiers
          (etypecase qualifiers
            (cons
             qualifiers)
            ((or (and symbol (not null)) function)
             #+(or)    ;; try w/o the denominatives
             (unless denominative
               (method-combination-error "no applicable :denominative method: ~s. ~s"
                                         function qualified-methods))
             (remove-duplicates 
              (apply qualifiers function
                     (mapc #'finalize-if-needed 
                           (method-specializers
                            #+(or) (first denominative)
                            (first (last qualified-methods)))))
              :from-end t))))
    (when verbose (format *trace-output* "~%~s: ~s -> applicable qualifiers: ~s."
                          function qualifiers applicable-qualifiers))
    ;; group the methods by applicable qualifier, result is least-specific-first within arbitrary specializer order
    (dolist (method qualified-methods)
      (setf method-qualifiers (method-qualifiers method))
      (cond ((= 1 (length method-qualifiers))
             (setf qualifier (first method-qualifiers))
             (if (or (member qualifier applicable-qualifiers) (find t applicable-qualifiers))
               (cond ((setf group (assoc qualifier grouped-methods))
                      (push method (rest group)))
                     (t
                      (push (list qualifier method) grouped-methods)))
               (when if-not-applicable
                 (invalid-method-error method "method qualifier not among those permitted: ~s." applicable-qualifiers))))
            (t
             (invalid-method-error method "method must have exactly one qualifier."))))
    
    ;; sort the groups by applicable qualifier
    (setf grouped-methods (stable-sort grouped-methods #'<
                                       :key #'(lambda (group) (or (position (first group) applicable-qualifiers)
                                                                  (position t applicable-qualifiers)
                                                                  (error "no position: ~s: ~s." qualifier applicable-qualifiers)))))
    (when verbose
      (format *trace-output* "~%grouped: ~:w" grouped-methods))

    (flet ((call-method-group (method-group)
             (destructuring-bind (nil . methods) method-group
               ;; reverse them if desired to get the most specific methods within each group last
               (when (eq order :most-specific-last) (setf methods (reverse methods)))
               `(call-method ,(first methods) ,(rest methods)))))
      (setf form
            (cond ((rest grouped-methods)
                   ;; if there is more than one group, combine them with the operator.
                   `(,operator ,@(mapcar #'call-method-group grouped-methods)))
                  (grouped-methods
                   (call-method-group (first grouped-methods)))
                  (default
                    (call-method-group (cons t default)))
                  (t
                   (method-combination-error "no method groups: ~s." function)))))
    
    (when around
      (setf form `(call-method ,(first around)
                               (,@(rest around)
                                (make-method ,form)))))
    (when verbose
      (format *trace-output* "~%~s: ~s:~%~:W" function `(:around ,around :denominative ,denominative ,qualified-methods) form))
    form))


