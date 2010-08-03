;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package de.setf.utility.implementation)


(:documentation "This file implements a generic graph walker for use with data models as part of the
 'de.setf.utility' library."
 
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

 (history
  (delta 20100310 "janderson" "reorganized to consolidate image/runtime operators and
 isolate the runtime-depencies.")
  (delta 20031020 "janderson" "rearranged the class constituency to clarify the node-link symmetry")
  (delta 20030902 "jaa@setf.de" "added missing walk-link-qualifiers")
  (delta 20021214 "janderson" "rewritten based on what was originally a DOM walker."))

 (description
  "The walker operates on both acyclic and cyclic directed graphs, and performs either preorder,
  endorder, or symmetric order traversal. The behaviour is factored into distinct classes for node and
  link traversal and manipulation each of which contributes qualifiers to an effective method. The potential
  method complement is determined by the node class, but the actions of the effective methos are further
  constrained based on the actual argument. (see the denominated method combination.)
  an application controls the behaviour by providing a walker which mixes in the desired control classes
  to offer the desired methods and, in needed, specializes the :qualifying method to indicate which aspects
  of the effective method to run.
  
  The traversal terminology follows from Knuth (section 2.3 of "fundamental algorithms"), as does the test
  example. His discussion concentrates on the graph nodes and relegates the links to representational artifacts,
  so the terms "initial" and "final" node are adopted from Truss ("discrete methematics for computer scientists").</p>
  
  A test example demonstrates how the abstract components are combined to define a walker.
  tHe literal test model does not agree with knuth's list notation (p 334 of the 1968 edition), as this latter 
  is ambiguous and incomplete, but that does not affect the results."))


(:documentation "class structure"

  "the abstract walker classes distinguish the aspects

 - walker is the abstract class for which the elementary interface functions - walk-model
   walk-node, and walk-link, are defined.
 - walk-operator distinguished classes intended to operate on nodes and links.
 - walk-navigator distinguishes classes intended to traverse nodes and links.
 - cyclic-walker binds a cache and implements operations to record properties for nodes and links.
   uses recorded node properties to limit node operations to once only.
 - acyclic-walker distinguishes classes intended to traverse nodes and links.


these abstract classes are specialized to distinguish walking patterns

 - node-operator operates on nodes
 - node-constituent-navigator traverses a node's constituents
 - node-predecessor-navigator traverses a node's predecessors
 - node-successor-navigator traverses a node's successors
 - link-navigator traverse from a links origin to its target
 - link-relation-operator operates on a named link relation


these, in turn, are combined in classes which implement concrete walking patterns

 - node-walker
 - preorder-node-walker operates on the node and constituents, then walks predecessors, then successors
 - symmetric-node-walker traverses predecessors, then the node and constituents, then traverses the successors
 - endorder-node-walker walks predecessors, then successors, then operates on the node and constituents
 - node-navigator  walks predecessors, then successors, but does not operate on a node 
 - preorder-link-walker operates on a links relation then traverses from origin to target
 - endorder-link-walker traversed from origin to target, then operates on a links relation
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coerce-to-package (datum)
  (etypecase datum
    (symbol (coerce-to-package (string datum)))
    (string (or (find-package datum)
                (error "package not found: ~s." datum)))
    (package datum)))

(defgeneric in-extent-p (object)
  (:documentation "Return true iff either *walk-extent* is null, which means then no extent constraint
    is present, or the object satisifies the constraint.")

  (:method ((object t))
    t)

  (:method ((object symbol))
    (when (or (find object *walk-extent*) (find (symbol-package object) *walk-extent*))
      t))

  (:method :around ((object t))
    (if (null *walk-extent*)
      t
      (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-abstract-class walker ()
  ((class-qualifiers
    :initform nil  ;; no initarg
    :allocation :class
    :reader walker-class-qualifiers
    :documentation
    "a list of method qualifiers which constrain the selection and order of
     methods for a given denominated method combination. by default it is
     shared by the combinations for the functions walk-node, walk-link,
     walk-navigate-nde-predecessors, and walk-node-successors. should there
     be some reason to need to treat identical qualifiers differently for the
     respective methods, the immediate source in each case is the respective
     function *-qualifiers, which can be specialized by walker. note that the
     literal source is an allocated instance, which means that the reference
     must suceed on an uninitialized instance; thus the class allocation.")
   (qualifiers
    :reader walker-qualifiers
    :documentation "This is a list of the qualifiers in effect for the particuler concrete walker class.
     The set is constructed lazily as the union of all superclasses' qualifiers."))
  (:documentation
   "the abstract class of all walker controls."))

(def-abstract-class walk-navigator (walker) ()
  (:documentation
   "the abstract class of walker mixin which controls link navigation."))

(def-abstract-class walk-operator (walker) ()
  (:documentation
   "the abstract class of walker mixin which controls node manipulation."))

(def-abstract-class cyclic-walker (walker)
  ((cache
    :initform nil :initarg :cache
    :reader walker-cache))
  (:documentation
   "The abstract cyclic-walker class uses a node cache to constrain node operations to once only
 based on walker-node-visited / walker-node-visited-p.
 The cache also supports the walker-node-cache-entry and walker-node-properties operators."))

(defclass acyclic-walker (walker) ())


(defclass node-operator (walk-operator)
  ((class-qualifiers :initform '(self) :allocation :class))
  (:documentation
   "a node-operator implements walk-node-self methods to effect operator application to the node proper."))

(defclass node-constituent-navigator (walk-navigator)
  ((class-qualifiers :initform '(constituents) :allocation :class))
  (:documentation
   "a node-constituent-navigator implements walk-node-constituent methods to effect traversal of links
 to the node constituents."))

(defclass node-predecessor-navigator (walk-navigator)
  ((class-qualifiers :initform '(predecessors) :allocation :class))
  (:documentation
   "a node-predecessor-navigator implements walk-node-predecessors methods to effect traversal of links
 which preceed the node proper."))

(defclass node-successor-navigator (walk-navigator)
  ((class-qualifiers :initform '(successors) :allocation :class))
  (:documentation
   "a node-successor-navigator implements walk-node-successors methods to effect traversal of links
 which succeed the node proper."))


(defclass link-navigator (walk-navigator)
  ((class-qualifiers :initform '(other) :allocation :class))
  (:documentation
   "a link-navigator implements walk-link-other to effect traversal to the link's target node.
 the default method ignores the initial node."))

(defclass link-relation-operator (walk-operator)
  ((class-qualifiers :initform '(relations) :allocation :class))
  (:documentation
   "a link-relation-operator implements walk-link-relation methods to effect operator application
 to the (relation X initial-node X target-node) combination."))



(defgeneric walker-node-qualifiers (walker)
  (:method ((class standard-class))
    (walker-node-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))

(defgeneric walker-self-qualifiers (walker)
  (:method ((class standard-class))
    (walker-self-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))

(defgeneric walker-constituent-qualifiers (walker)
  (:method ((class standard-class))
    (walker-constituent-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))

(defgeneric walker-predecessor-qualifiers (walker)
  (:method ((class standard-class))
    (walker-predecessor-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))

(defgeneric walker-successor-qualifiers (walker)
  (:method ((class standard-class))
    (walker-successor-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))

(defgeneric walker-link-qualifiers (walker)
  (:method ((class standard-class))
    (walker-link-qualifiers (class-prototype class)))
  (:method ((walker walker))
    (walker-qualifiers walker)))


;;;
;;; idiomatic combinations

(defclass node-walker
  (node-operator node-constituent-navigator)
  ()
  (:documentation "A node-walker operates on the node and constituents only."))

(defclass preorder-node-walker
  (node-operator node-constituent-navigator node-predecessor-navigator node-successor-navigator)
  ()
  (:documentation
   "a preorder-node-walker combines walk-node methods so as to implement preorder traversal."))

(defclass symmetric-node-walker
  (node-predecessor-navigator node-operator node-constituent-navigator node-successor-navigator)
  ()
  (:documentation
   "a symmetric-order-node-walker combines walk-node methods so as to implement symmetric (postorder) traversal."))

(defclass endorder-node-walker
  (node-predecessor-navigator node-successor-navigator node-operator node-constituent-navigator)
  ()
  (:documentation
   "a endorder-node-walker combines walk-node methods so as to implement endorder traversal."))

(defclass node-navigator (node-predecessor-navigator node-successor-navigator)
  ()
  (:documentation
   "a node-navigator combines walk-node methods so as to implement navigation without manipulation."))

(defclass preorder-link-walker (link-relation-operator link-navigator)
  ()
  (:documentation
   "a preorder-link-walker combines walk-link methods so as to implement link traversal as a relation operation followed by navigation to the target."))

(defclass endorder-link-walker (link-navigator link-relation-operator)
  ()
  (:documentation
   "a endorder-link-walker combines walk-link methods so as to implement link traversal as navigation to the target followed by a relation operation."))



(:documentation "cache management"
  "a cyclic-walker maintaines a record for nodes. the specialization
 cyclic-walker implements and :around method for walk-node which cuts off
 the walk if the respective cache entry indicates that the node has already
 been visited. it does not, however, either add the cache entry or set the
 indicator. this is left to the application in order that it can specify
 when it is through with the respective node.")


(defstruct walker-entry
  "each walker cache entry binds the node, a visited flag, and a property list"
  (visited nil :type (or null cons))
  (node nil :type t)
  (properties nil :type list))

(defgeneric make-walker-cache-entry (navigator node)
  (:method ((navigator cyclic-walker) (node t))
    (make-walker-entry :node node)))

(defgeneric walker-node-cache-entry (navigator node)
  (:method ((navigator t) (node t)) nil)
  (:method ((navigator cyclic-walker) (key cons))
    (with-slots (cache) navigator
      (gethash key cache)))
  (:method ((navigator cyclic-walker) (key string))
    (with-slots (cache) navigator
      (gethash key cache)))

  (:method ((navigator cyclic-walker) (node t))
    (with-slots (cache) navigator
      (let* ((designator (dsw:object-designator node))
             (entry (gethash designator cache)))
        (cond (entry
               (setf (walker-entry-node entry) node)
               entry)
              (t
               (setf (gethash designator cache) (make-walker-cache-entry navigator node))))))))

(defgeneric (setf walker-node-cache-entry) (entry navigator key)
  (:method (entry (navigator cyclic-walker) (key t))
    (with-slots (cache) navigator
      (setf (gethash key cache) entry))))

(defgeneric walker-node-properties (navigator node)
  (:method ((navigator t) (node t)) nil)
  (:method ((navigator cyclic-walker) (node t))
    (walker-entry-properties (walker-node-cache-entry navigator node))))

(defgeneric (setf walker-node-properties) (properties navigator node)
  (:method ((properties list) (navigator cyclic-walker) (node t))
    (setf (walker-entry-properties (walker-node-cache-entry navigator node)) properties)))

(defgeneric walker-node-property (navigator node property)
  (:method ((navigator t) (node t) (property t)) nil)
  (:method ((navigator cyclic-walker) (node t) (property t))
    (let ((entry (walker-node-cache-entry navigator node)))
      (when entry
        (getf (walker-entry-properties entry) property)))))

(defgeneric (setf walker-node-property) (value navigator node property)
  (:method (value (navigator cyclic-walker) (node t) (property t))
    (setf (getf (walker-entry-properties (walker-node-cache-entry navigator node))
                property)
          value)))

(defgeneric walker-node-visited-p (navigator node)
  (:method ((navigator t) (node t)) nil)
  (:method ((cache-entry walker-entry) (node t))
    (eq (walker-entry-visited cache-entry) *walker-cycle*))

  (:method ((navigator cyclic-walker) (node t))
    (walker-node-visited-p (walker-node-cache-entry navigator node) node)))

(defgeneric (setf walker-node-visited) (visited navigator node)
  (:method ((visited t) (navigator cyclic-walker) (node t))
    (setf (walker-entry-visited (walker-node-cache-entry navigator node))
          (if visited *walker-cycle* nil))))

(defgeneric walker-make-cache (navigator)
  (:method ((navigator cyclic-walker))
    (make-hash-table :test 'equalp)))

(defgeneric walker-clear-cache (navigator)
  (:method ((navigator cyclic-walker))
    (clrhash (walker-cache navigator))))


(defgeneric walker-class-qualifiers (prototype)
  (:method ((prototype standard-class))
    (finalize-if-needed prototype)
    (walker-class-qualifiers (class-prototype prototype)))
  (:method ((object t))
    nil))


(defgeneric walker-inherited-qualifiers (walker)
  (:documentation "Compute the qualifiers for the specific walker class. In no initialization argument is
 provided, the instrinsic set applies. Given an argument, it constrains and orders the default set.")

  (:method ((walker walker))
    (walker-inherited-qualifiers (class-of walker)))

  (:method ((class standard-class))
    (remove-duplicates
     (reduce #'append (c2mop:class-precedence-list class)
             :key  #'walker-class-qualifiers))))


(defmethod initialize-instance :after ((instance walker) &key (qualifiers nil q-s)
                                       added-qualifiers excluded-qualifiers)
  (unless q-s
    (setf qualifiers (walker-inherited-qualifiers instance)))
  (when excluded-qualifiers
    (setf qualifiers (remove-if #'(lambda (q) (member q excluded-qualifiers)) qualifiers)))
  (when added-qualifiers
    (setf qualifiers (append added-qualifiers qualifiers)))
  (setf (slot-value instance 'qualifiers) qualifiers))


(defmethod initialize-instance :after ((instance cyclic-walker) &key)
  (with-slots (cache) instance
    (unless cache (setf cache (walker-make-cache instance)))))


(:documentation "generic traversal"

  "The traversal operators implement symmetric iteration over nodes and links involving
 the distinct phases

 - operate on the node
 - navigate from the node through the links
 - operate on the link
 - navigate from the link through the nodes

 The implementation is expressed in distinct qualified methods for the functions walk-node and walk-link.
 Each method is qualifed to indicate its role in a general traversal

 - from nodes: self, predecessors, and successors
 - from links: other, and relations.

 In both cases, the order of combination is determined for the respective navigator class through the method
 combination, in that it integrates the denominated methods as specialized by the walker through the various
 forms of walker-*-qualifiers. Each of the abstract classes node-operator, node-predecessor-navigator,
 node-successor-navigator, link-relation-operator, and link-navigator contribute a qualifier and the
 abstract idiomatic classes like endorder-node-walker cause these to be ordered appropriately.
 an application is also free to define and specify entirely different walk-node and walk-link components.")


(defgeneric walk-model (walker model operator)
  (:documentation
   "iterate over a model graph using a navigator instance to control link traversal and operator application.
 the operator should be both polymorphic and multivalent to the extent required by the nodes and link triples.
 this includes children as well as class-specific dependants, such as definitional aspects of a document,
 element, or annotation. a before method is defined for caching walker to clear the cache and a general base
 method is defined which combines the navigator, model, and function in call to walk-node to start navigation.")

  (:method :around ((walker cyclic-walker) (model t) (operator t))
    (let ((*walker-cycle* (list (get-universal-time))))
      (call-next-method)))

  (:method ((*walker* t) (model t) (operator t))
    (walk-node *walker* model operator)
    *walker*)

  (:method ((*walker* t) (model cons) (operator t))
    (dolist (model model)
      (walk-node *walker* model operator))
    *walker*))



(defgeneric walk-node (navigator node operator)
  (:documentation
   "walk within the node proper. the primary navigation methods distinguish pre- and post-order
navigation. the method combination, denominated, is declared to use the function walk-node-qualifiers to
determine which qualifiers apply to a given walker instance and in which order. the initial generic
definition defines an :around methods for cyclic-walker and for a nullnode to filter traversal, and the
qualified methods self - for walk-node-self, predecessors - for walk-node-predecessors, and successors -
for walk-node-successors.")
  
  (:method-combination denominated :order :most-specific-first )

  (:method :around ((navigator cyclic-walker) (node t) (operator t))
           (cond ((walker-node-visited-p navigator node))
                 (t
                  (setf (walker-node-visited navigator node) t)
                  (call-next-method))))

  (:method :around ((navigator walker) (node null) (operator t))
           nil)

  (:method :around ((navigator walker) (node t) (operator t))
    ;; constrain the :closed boundary mode here: the node must be within the
    ;; extent to permit the operation
    (when (or (eq *walk-extent-boundary* :open) (in-extent-p node))
      (let ((*walk-depth* (1+ (or *walk-depth* 0))))
        (call-next-method))))
  (:method ((navigator walker) (node t) (operator t))
    node)
  (:method self ((navigator walker) (node t) (operator t))
           (walk-node-self navigator node operator))

  (:method predecessors ((navigator walker) (node t) (operator t))
           (walk-node-predecessors navigator node operator)
           node)
  (:method successors ((navigator walker) (node t) (operator t))
           (walk-node-successors navigator node operator)
           node)
  (:method constituents ((navigator walker) (node t) (operator t))
           (walk-node-constituents navigator node operator)
           node)

  (:method :qualifying ((navigator walker) (node t) (operator t))
           (walker-node-qualifiers navigator)))


(defgeneric walk-node-self (node navigator function)
  (:method-combination denominated :order :most-specific-first )

  (:method ((navigator node-operator) (node t) (function t))
           (funcall function node))

  (:method ((navigator t) (node t) (function t))
           node)

  (:method :qualifying ((navigator walker) (node t) (operator t))
           (walker-self-qualifiers navigator)))



(defgeneric walk-node-constituents (navigator node operator)
  (:method-combination denominated :order :most-specific-first )
  (:documentation
   "concrete specializations of node-constituent-navigator must implement named
    methods to walk constituent links for each node class by calling walk-link
    with arguments navigator, relation, outgoing node, and incident node. the
    effective method is a most-specific-last and method combination.")

  (:method ((navigator walker) (node t) (operator t))
           node)
  (:method :qualifying ((navigator walker) (node t) (operator t))
           (walker-constituent-qualifiers navigator)))


(defgeneric walk-node-predecessors (navigator node operator)
  (:method-combination denominated :order :most-specific-first )
  (:documentation
   "concrete specializations of node-successor-navigator must implement named
    methods to walk predecessor links for each node class by calling walk-link
    with arguments navigator, relation, outgoing node, and incident node. the
    effective method is a most-specific-last and method combination.")

  (:method ((navigator walker) (node t) (operator t))
           node)
  (:method :qualifying ((navigator walker) (node t) (operator t))
           (walker-predecessor-qualifiers navigator)))


(defgeneric walk-node-successors (navigator node operator)
  (:method-combination denominated :order :most-specific-first )
  (:documentation
   "concrete specializations of node-successor-navigator must implement named
    methods to walk predecessor links for each initial node class. they
    effective method is a most-specific-last and method combination.")

  (:method ((navigator walker) (node t) (operator t))
           node)
  (:method :qualifying ((navigator walker) (node t) (operator t))
           (walker-successor-qualifiers navigator)))


(defgeneric walk-link (navigator relation from to operator)
  (:documentation
   "walk a relation link between a node from and a node to
    controlled by navigator and using operator. If the boundary is open, constrain the
 extent constraint applies to the link operation itself. Otherwise the node operation
 checks.")
  (:method-combination denominated :order :most-specific-first )
  (:method :around ((navigator t) (relation t) (from t) (to t) (op t))
    ;; constrain the :open boundary mode here: the other node must be in the
    ;; extent to permit the operation
    (when (or (eq *walk-extent-boundary* :closed) (in-extent-p to))
      (call-next-method)))
  (:method ((navigator walker) (relation t) (from t) (to t) (op t))
           to)
  (:method other ((navigator walker) (relation t) (from t) (to t) (op t))
           (walk-link-other navigator from to op))
  (:method relations ((navigator walker) (relation t) (from t) (to t) (op t))
           (walk-link-relation navigator relation from to op))

  (:method :qualifying ((navigator walker) (relation t) (from t) (to t) (op t))
           (walker-link-qualifiers navigator)))


(defgeneric walk-link-other (navigator from to op)
  (:method ((navigator t) (from t) (to t) (op t))
           to)
  (:method ((navigator link-navigator) (from t) (to t) (op t))
    "Constrain the traversal extent at the decision whether to walk the other node in
 a link relation. If it is in the extent, continue, otherwise just return."
    (walk-node navigator to op)))

(defgeneric walk-link-relation (navigator relation from-node to-node operator)
  (:method ((navigator link-relation-operator) (relation t) (from t) (to t) (operator t))
           (funcall operator from to relation)
           (call-next-method))
  (:method ((navigator t) (relation t) (from t) (to t) (operator t))
           to))


(defgeneric map-walk-link (to-node-sequence navigator relation from-node operator)
  (:method ((to t) (navigator t) (relation t) (from t) (operator t))
           (walk-link navigator relation from to operator))
  (:method ((to null) (navigator t) (relation t) (from-node t) (operator t))
           nil)
  (:method ((to-nodes cons) (navigator t) (relation t) (from-node t) operator)
           (dolist (to to-nodes) (walk-link navigator relation from-node to operator)))
  (:method ((to-nodes hash-table) (navigator t) (relation t) (from-node t) operator)
           (maphash #'(lambda (key to)
                        (declare (ignore key))
                        (walk-link navigator relation from-node to operator)) to-nodes))
  (:method ((to-nodes sequence) (navigator t) (relation t) (from-node t) operator)
           (map nil #'(lambda (to) (walk-link navigator relation from-node to operator)) to-nodes)))



:de.setf.utility.walker

