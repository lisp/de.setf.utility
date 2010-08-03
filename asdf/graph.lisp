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
(defparameter *system-graph-predecessor-depth* 0)
(defparameter *system-graph-component-depth* 0)
(defparameter *system-graph-components* t)

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

(defgeneric system-graph-components-p (scope)
  (:documentation
   "return true if either the components are not constrained.
 properties are scoped by generic component type.")
  (:method ((scope symbol))
    (getf (getf *system-graph-properties* scope) :components *system-graph-components*))
  (:method ((scope asdf:system))
    (system-graph-components-p :system))
 (:method ((scope asdf:source-file))
    (system-graph-components-p :file))
  (:method ((scope asdf:module))
    (system-graph-components-p :module))
  (:method ((scope definition))
    (system-graph-components-p :definition)))

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
                                     (rankdir "LR")
                                     ((:properties *system-graph-properties*) *system-graph-properties*))
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
          (setf.dot:digraph name (:pretty pretty :rankdir rankdir :compound "true")
            (encode-component-graph system))
          (values (count-if #'(lambda (c) (typep c 'asdf:system)) *system-components*)
                  (count-if #'(lambda (c) (and (typep c 'asdf:module)
                                               (not (typep c 'asdf:system))
                                               (not (typep c 'asdf:source-file)))) *system-components*)
                  (count-if #'(lambda (c) (typep c 'asdf:source-file)) *system-components*))))))

  (defgeneric encode-component-graph (component)
    (:documentation "Add the graph elements for the given component to the on-going graph.
 Under the constraint of *system-graph-predecessor-depth*, trace dependency relations and include predecessors.
 Accept systems, elementary components and sequences of same. ")

    (:method ((components list))
      (map nil 'encode-component-graph components))

    (:method :around ((component asdf:component))
      (declare (special *system-components*))
      (unless (find component *system-components*)
        (let ((*system-graph-component-depth* (1+ *system-graph-component-depth*)))
          (push component *system-components*)
          (call-next-method))))

    (:method ((component asdf:system))
      (when (system-graph-system-p (asdf:component-name component))
        (when (system-graph-predecessors-p component)
          ;; this step should add nodes for the systems only, all child components
          ;; should already has appeared
          (let ((*system-graph-predecessor-depth* (1+ *system-graph-predecessor-depth*)))
            (map nil #'encode-component-graph (component-predecessors component))))
        (call-next-method)))

    (:method ((component asdf:component))
      (if (system-graph-components-p component)
        (encode-component-subgraph component)
        (encode-component-node component))
      (when (system-graph-predecessors-p component)
        (encode-component-edges component))))

  (defgeneric encode-component-node (component)

    (:method ((component asdf:component))
      (setf.dot:node component :label (asdf:component-name component)))
    
    (:method ((definition definition))
      (setf.dot:node definition
        :label (case (definition-type definition)
                 (class (format nil "class ~a" (definition-name definition)))
                 (t (definition-name definition))))))

  (defgeneric encode-component-subgraph (component)

    (:method ((component asdf:component))
      (setf.dot:subgraph (setf (setf.dot:id component) (cluster-name component))
                         ()
        (setf.dot:node (cluster-node-id component)
          :label (format nil "~a ~a" (type-of component) (asdf:component-name component))
          :shape "hexagon")
        (map nil #'encode-component-graph (asdf:module-components component))))

    (:method ((definition definition))
      (encode-component-node definition)))

  (defgeneric encode-component-edges (component)
    (:documentation
     "recurse down through the component tree. emit inter-component edges
 for compound components with the respective nodes as the head and tail.
 at the definition/leaf level emit edges betweent he respective definitions.")

    (:method ((component asdf:component))
      (when (system-graph-predecessors-p component)
        (let ((components-p (system-graph-components-p component)))
          (dolist (predecessor (component-direct-predecessors component))
            (encode-component-pair-edge component predecessor
                                        components-p
                                        (system-graph-components-p predecessor))))))
    
    (:method ((definition definition))
      (dolist (predecessor (depends-on-ids definition))
        (setf.dot:edge definition predecessor))))

  (defgeneric encode-component-pair-edge (component predecessor sub-c sub-p)
    (:method ((component asdf:component) (predecessor asdf:component) (sub-c (eql t)) (sub-p (eql t)))
      (let* ((component-id (cluster-name component))
             (component-node-id (cluster-node-id component))
             (predecessor-id (cluster-name predecessor))
             (predecessor-node-id (cluster-node-id predecessor)))
        (setf.dot:edge component-node-id predecessor-node-id :ltail component-id :lhead predecessor-id)
        (format setf.dot:*context* "/* ~a -> ~a */" component predecessor)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c (eql t)) (sub-p null))
      (let* ((component-id (cluster-name component))
             (component-node-id (cluster-node-id component))
             (predecessor-id (setf.dot:id predecessor)))
        (setf.dot:edge component-node-id predecessor-id :ltail component-id)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c null) (sub-p (eql t)))
      (let* ((component-id (setf.dot:id component))
             (predecessor-id (cluster-name predecessor))
             (predecessor-node-id (cluster-node-id predecessor)))
        (setf.dot:edge component-id predecessor-node-id :lhead predecessor-id)))

    (:method ((component asdf:component) (predecessor asdf:component) (sub-c null) (sub-p null))
      (let* ((component-id (setf.dot:id component))
             (predecessor-id (setf.dot:id predecessor)))
        (setf.dot:edge component-id predecessor-id))))
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
|#