;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;;  This file is is a constituent of the 'de.setf.utility' library component.
;;;  It adds graph visualization to ASDF.
;;;  (c) 2010 james anderson
;;;
;;;  'de.setf.utility' is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;
;;;  'de.setf.utility' is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with 'de.setf.utility'.  If not, see the GNU [site](http://www.gnu.org/licenses/).

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; content :
;;;
;;; Given an asdf system model, generate a graph in .dot encoding.

;;; ENCODE-SYSTEM-GRAPH

#+ignore
(progn
(defclass count-op (asdf:operation)
  ())

(defmethod asdf::perform ((op count-op) (component asdf:component))
  (declare (special *counted-components* *component-count*))
  (typecase component
    (asdf:system
     (when *component-count-dependents*
       (dolist (system (asdf:component-depends-on t component))
         (unless (find system *counted-components* :test 'equalp)
           (push system *counted-components*)
           (asdf:operate (type-of op) system :force t)))))
    (asdf:source-file
     (incf *component-count*))))
)
    


(defclass collection-op (asdf:operation)
  ((class :initform t :initarg :class :reader operation-class)))
(defclass system-collection-op (collection-op)
  ((class :initform 'asdf:system)))

(defmethod asdf::perform ((op collection-op) (component asdf:component))
  (declare (special *collected-components*))
  (when (typep component (operation-class op))
    (push component *collected-components*)
    (dolist (system (asdf:component-depends-on t component))
      (asdf:operate (type-of op) system :force t))))
    
(defun graph-system (system destination &rest args &key (name system) &allow-other-keys)
  (apply #'encode-system-graph (asdf:find-system system) destination
           :name name
           args))


(defparameter *system-graph-properties* nil)
(defparameter *system-graph-compound* t)
(defparameter *system-graph-predecessor-depth* 0)
(defparameter *system-graph-component-depth* 0)
(defparameter *system-graph-components* t)
(defparameter *system-graph-system-depth* 0)
(defparameter *system-graph-file-depth* 0)
(defparameter *system-graph-module-depth* 0)
(defparameter *system-graph-definition-depth* 0)
(defparameter *graph-constituent-color* "blue")
(defparameter *graph-predecessor-color* "red")

(defgeneric system-graph-predecessors-p (scope)
  (:documentation
    "return true if either the predecessors are not constrained, or if the depth
 is within the constraint. properties are scoped by generic component type.")
  (:method ((scope symbol))
    (<= *system-graph-predecessor-depth*
        (getf (getf *system-graph-properties* scope) :predecessors most-positive-fixnum)))
  (:method ((scope asdf:system))
    (system-graph-predecessors-p :system))
  (:method ((scope asdf:source-file))
    (system-graph-predecessors-p :file))
  (:method ((scope asdf:module))
    (system-graph-predecessors-p :module))
  (:method ((scope definition))
    (system-graph-predecessors-p :definition)))

(defgeneric graph-component-shape (component)
  (:documentation "return the scope indicator for a given component.")
  (:method ((scope asdf:system)) "hexagon")
  (:method ((scope asdf:source-file)) "rectangle")
  (:method ((scope asdf:module)) "octagon")
  (:method ((scope definition)) "circle"))

(defgeneric graph-component-scope (component)
  (:documentation "return the scope indicator for a given component.")
  (:method ((scope asdf:system)) :system)
  (:method ((scope asdf:source-file)) :file)
  (:method ((scope asdf:module)) :module)
  (:method ((scope definition)) :definition))

(defgeneric graph-component-depth (component)
  (:documentation "return the current depth for the given component class")
  (:method ((scope asdf:system)) *system-graph-system-depth*)
  (:method ((scope asdf:source-file)) *system-graph-file-depth*)
  (:method ((scope asdf:module)) *system-graph-module-depth*)
  (:method ((scope definition)) *system-graph-definition-depth*))

(defun graph-component-depth-limit (component)
  "return the depth limit for the given component class."
  (getf (getf *system-graph-properties* (graph-component-scope component)) :depth most-positive-fixnum))

(defun system-graph-depth-p (component)
  "return true if depth is not constrained, or the current depth is within the compoennt type's limit."
  (<= (graph-component-depth component) (graph-component-depth-limit component)))

(defun system-graph-expand-p (component)
  (getf (getf *system-graph-properties* (graph-component-scope component)) :expand *system-graph-components*))

(defun system-graph-subgraph-p (component)
   "return true if either the components are not constrained, or this component scope is included, and
 either depth is not constrained, or this component scope is within its depth."
   (and (system-graph-expand-p component)
        (system-graph-depth-p component)))
               

(defun system-graph-system-p (name)
  (let ((ignored (getf *system-graph-properties* :ignored-systems)))
    (not (find name ignored :test #'string-equal))))

(defun (setf system-graph-system-p) (state name)
  (let ((ignored (getf *system-graph-properties* :ignored-systems)))
    (if state
      (setf ignored (remove name ignored :test #'string-equal))
      (setf ignored (pushnew name ignored :test #'string-equal)))
    (setf (getf *system-graph-properties* :ignored-systems) ignored))
  state)

;; nb. in order that graphviz lay-out intelligently and, in any case, not abort, the following must hold:
;;  link expressions must be consistent with whether a component appears as a node or a subgraph
;;  link expressions must appear after the respective nodes
;;  link expressions must appear in the same scope as the respective nodes. an outer scope causes  bus error.

(labels ((find-id (parent name)
           (setf.dot:id (if parent
                          (asdf:find-component parent name)
                          (asdf:find-system name))))
         (depends-on-ids (component)
           (print (cons :doi component))

           (let ((ps (component-direct-predecessors component)))
             (mapcar #'(lambda (p) (setf.dot:id  p))
                     ps))
           #+ignore
           (mapcar #'(lambda (name)
                       (setf name (asdf::coerce-name name))
                       (or (find-id (asdf:component-parent component) name)
                           (format nil "LOST-~a" name)))
                   (component-predecessor-names component)))
         (sorted-components (component)
           (de.setf.utility:partial-order-sort (copy-list (asdf:module-components component))
                          #'required-p))
         (required-p (other dependent?)
           (or (and (null (component-predecessor-names other))
                    (component-predecessor-names dependent?))
               (find other (component-predecessor-names dependent?))))
         (cluster-name (component)
           (or (asdf:component-property component :cluster-name)
               (setf (asdf:component-property component :cluster-name)
                     (string (gensym "cluster")))))
         (cluster-node-id (component)
           (format nil "n_~a" (if (or (symbolp component) (stringp component))
                                component
                                (cluster-name component)))))
           
  (defun encode-system-graph (system destination &key (name (string (gensym "SYSTEM")))
                                     (pretty  *print-pretty*)
                                     ((:properties *system-graph-properties*) *system-graph-properties*)
                                     (graph-properties `(:rankdir "LR"
                                                         :compound ,(if *system-graph-compound* "true" "false"))))
    (let ((*system-graph-compound*
           (equal (getf graph-properties :compound (if *system-graph-compound* "true" "false")) "true")))
      (handler-bind
        ((asdf:missing-component (lambda (condition)
                                   ;; if a component is missing, not to ignore it an continue
                                   (let ((missing-name (asdf::missing-requires condition)))
                                     (when (system-graph-system-p missing-name)
                                       (warn "System component not found: ~a." condition)
                                       (setf (system-graph-system-p missing-name) nil))
                                     (use-value nil)))))
        (setf.dot:with-context destination
          (let ((*system-components* nil))
            (declare (special *system-components*))
            (apply #'setf.dot:context-put-graph setf.dot:*context*
                   name
                   #'(lambda () (encode-component-graph system))
                   :pretty pretty
                   graph-properties)
            (values (count-if #'(lambda (c) (typep c 'asdf:system)) *system-components*)
                    (count-if #'(lambda (c) (and (typep c 'asdf:module) (not (typep c 'asdf:system)))) *system-components*)
                    (count-if #'(lambda (c) (typep c 'asdf:source-file)) *system-components*)))))))

  (defgeneric encode-component-graph (component)
    (:documentation "Add the graph elements for the given component to the on-going graph.
 Under the constraint of *system-graph-predecessor-depth*, trace dependency relations and include predecessors.
 Accept systems, elementary components and sequences of same. ")

    (:method ((designator symbol))
      (encode-component-graph (asdf:find-system designator)))

    (:method ((components cons))
      (map nil 'encode-component-graph components))

    (:method :around ((component asdf:module))
      (typecase component
        (asdf:system
         (let ((*system-graph-system-depth* (1+ *system-graph-system-depth*)))
           (call-next-method)))
        (t
         (let ((*system-graph-module-depth* (1+ *system-graph-module-depth*)))
           (call-next-method)))))

    (:method :around ((component asdf:source-file))
      (let ((*system-graph-file-depth* (1+ *system-graph-file-depth*)))
        (call-next-method)))

    (:method :around ((component definition))
      (let ((*system-graph-definition-depth* (1+ *system-graph-definition-depth*)))
        (call-next-method)))

    (:method :around ((component asdf:component))
      (declare (special *system-components*))
      (let ((mode (if (system-graph-subgraph-p component) :graph :node))
            (emitted (getf *system-components* component)))
        (case  emitted
          ((nil) (setf (getf *system-components* component) mode)
                (call-next-method))
          (:graph )
          (:node (ecase mode
                   (:graph (setf (getf *system-components* component) mode)
                           (call-next-method))
                   (:node ))))))
    
    (:method ((component asdf:system))
      (when (system-graph-system-p (asdf:component-name component))
        (call-next-method)))

    (:method ((component asdf:component))
      (when (system-graph-predecessors-p component)
        (let ((*system-graph-predecessor-depth* (1+ *system-graph-predecessor-depth*)))
          (map nil #'encode-component-graph (component-predecessors component))))
      (if (system-graph-subgraph-p component)
        (encode-component-subgraph component)
        (encode-component-node component))
      (when (system-graph-predecessors-p component)
        ;; encode edges after nodes
        (encode-component-edges component))))

  (defgeneric encode-component-node (component)

    (:method ((component asdf:component))
      (setf.dot:node component
        :label (asdf:component-name component)
        :shape (graph-component-shape component)))
    
    (:method ((definition definition))
      (setf.dot:node definition
        :label (case (definition-type definition)
                 (class (format nil "class ~a" (definition-name definition)))
                 (t (definition-name definition))))))

  (defgeneric encode-component-subgraph (component)

    (:method ((component asdf:component))
      (cond (*system-graph-compound* 
             (setf.dot:subgraph (setf (setf.dot:id component) (cluster-name component))
                                ()
               (setf.dot:node (cluster-node-id component)
                 :label (format nil "~a ~a" (type-of component) (asdf:component-name component))
                 :shape (graph-component-shape component))
               (map nil #'encode-component-graph (asdf:module-components component))))
            (t
             (setf.dot:node (setf.dot:id component)
               :label (format nil "~a ~a" (type-of component) (asdf:component-name component))
               :shape (graph-component-shape component))
             (dolist (sub-component (asdf:module-components component))
               (encode-component-graph sub-component)
               (encode-component-edge component sub-component nil *graph-constituent-color*)))))
             

    (:method ((definition definition))
      (encode-component-node definition)))

  (defgeneric encode-component-edges (component)
    (:documentation
     "recurse down through the component tree. emit inter-component edges
 for compound components with the respective nodes as the head and tail.
 at the definition/leaf level emit edges betweent he respective definitions.")

    (:method ((component asdf:component))
      (let ((subgraph-p (and *system-graph-compound* (system-graph-subgraph-p component))))
        (dolist (predecessor (component-direct-predecessors component))
          (encode-component-edge component predecessor subgraph-p *graph-predecessor-color*))))
    
    (:method ((definition definition))
      (dolist (predecessor (depends-on-ids definition))
        (setf.dot:edge definition predecessor :color *graph-predecessor-color*))))

  (defgeneric encode-component-edge (component predecessor sub-c color)

    (:method ((component t) (predecessor t) (sub-c t) color)
      (encode-component-pair-edge component predecessor
                                  sub-c
                                  (and *system-graph-compound* (system-graph-subgraph-p predecessor))
                                  color))

    (:method :around ((component t) (predecessor asdf:module) (sub-c t) (color t))
      (typecase component
        (asdf:system
         (let ((*system-graph-system-depth* (1+ *system-graph-system-depth*)))
           (call-next-method)))
        (t
         (let ((*system-graph-module-depth* (1+ *system-graph-module-depth*)))
           (call-next-method)))))

    (:method :around ((component t) (predecessor asdf:source-file) (sub-c t) (color t))
      (let ((*system-graph-file-depth* (1+ *system-graph-file-depth*)))
        (call-next-method)))

    (:method :around ((component t) (predecessor definition) (sub-c t) (color t))
      (let ((*system-graph-definition-depth* (1+ *system-graph-definition-depth*)))
        (call-next-method))))


  (defgeneric encode-component-pair-edge (component predecessor sub-c sub-p color)
    (:method ((component asdf:component) (predecessor asdf:component) (sub-c (eql t)) (sub-p (eql t)) color)
      (let* ((component-id (cluster-name component))
             (component-node-id (cluster-node-id component))
             (predecessor-id (cluster-name predecessor))
             (predecessor-node-id (cluster-node-id predecessor)))
        (setf.dot:edge component-node-id predecessor-node-id :ltail component-id :lhead predecessor-id :color color)
        (format setf.dot:*context* " /* ~a -> ~a */ " component predecessor)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c (eql t)) (sub-p null) color)
      (let* ((component-id (cluster-name component))
             (component-node-id (cluster-node-id component))
             (predecessor-id (setf.dot:id predecessor)))
        (setf.dot:edge component-node-id predecessor-id :ltail component-id :color color)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c null) (sub-p (eql t)) color)
      (let* ((component-id (setf.dot:id component))
             (predecessor-id (cluster-name predecessor))
             (predecessor-node-id (cluster-node-id predecessor)))
        (setf.dot:edge component-id predecessor-node-id :lhead predecessor-id :color color)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c null) (sub-p null) color)
      (let* ((component-id (setf.dot:id component))
             (predecessor-id (setf.dot:id predecessor)))
        (setf.dot:edge component-id predecessor-id :color color))))
  )
       
           


#|
;;; patch logical hosts for source file pathnames

(setf (logical-pathname-translations "LISP")
    `(("**;*.*" "ECONORAVEN:lisp;**;*.*")
      ("**;*.*.*" "ECONORAVEN:lisp;**;*.*.*")))

(setf (logical-pathname-translations "ROOT")
    `(("**;*.*" "ECONORAVEN:root;**;*.*")
      ("**;*.*.*" "ECONORAVEN:root;**;*.*.*")))

(defparameter *components*
  (let ((table (make-hash-table :test 'equal)))
    (mapcar #'(lambda (p)
                (setf (gethash p table)
                      (make-instance 'asdf:system :pathname p :name (first (last (pathname-directory p))))))
            '(#P"yoda:Development:Source:dev:Library:com:ravenpack:econoravenlisp:"
              #P"yoda:Development:Source:dev:Library:com:ravenpack:econoravenvisualization:"
              #P"yoda:Development:Source:dev:Library:com:ravenpack:MisLisp:"))
    table))

(setq *components*
  (compute-modules (module-packages '("COM.RAVENPACK" "ECONORAVEN"))
                   :components *components*))
(let ((*print-case* :downcase))
  (pprint (compute-system-definition *components*) (make-instance 'ccl:fred-window)))

(inspect (loop for component being the hash-value of *components*
               when (typep component 'asdf:system)
               collect component))

(encode-system-graph (loop for component being the hash-value of *components*
                           when (typep component 'asdf:system)
                           collect component)
                     #p"HOME:system-graph.dot"
                     :name "tsl"
                     :pretty t)

(encode-system-graph :de.setf.utility.dot
                     #p"LIBRARY:dot-system-graph.dot"
                     :name "dot"
                     :pretty t
                     :properties '(:file (:expand nil) :system (:depth 2))
                     )

(encode-system-graph :org.datagraph.spocq
                     #p"LIBRARY:spocq-system-graph-2.dot"
                     :name "spocq"
                     :pretty t
                     :properties '(:file (:expand nil) :system (:depth 2))
                     :graph-properties '(:compound "false" :ranksep "2.2")
                     )

(encode-system-graph :org.datagraph.spocq
                     #p"LIBRARY:spocq-system-graph.dot"
                     :name "spocq"
                     :pretty t
                     :properties '(:file (:expand nil))
                     :graph-properties '(:compound "false" :ranksep "2.5")
                     )

(encode-system-graph :org.datagraph.spocq
                     #p"LIBRARY:spocq-system-graph-compound.dot"
                     :name "spocq"
                     :pretty t
                     :properties '(:file (:expand nil))
                     :graph-properties '(:compound "true" :rankdir "LR")
                     )

(encode-system-graph :org.datagraph.spocq
                     #p"LIBRARY:spocq-system-graph-compound-2.dot"
                     :name "spocq"
                     :pretty t
                     :properties '(:file (:expand nil) :system (:depth 2))
                     :graph-properties '(:compound "true" :rankdir "LR")
                     )
|#