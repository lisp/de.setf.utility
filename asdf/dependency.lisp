;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: cl-user; -*-

;;;  This file is is a constituent of the 'de.setf.utility' library component.
;;;  It adds system definition generation to ASDF.
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
;;; add a higher-resolution components to the asdf model for definition forms -
;;; functions, parameters, macros. use the additional resolution to derive system descriptions
;;; from a running image.

;;; COMPUTE-MODULES (packages)
;;;   derives a dependency model from the definitions in the designated packages. returns a hastable
;;;   of pathname-indexed file components, each of which contains the entries for its respective
;;;   definition forms.
;;;
;;; SYSTEM-DECLARATION (system)
;;;   given an asdf system or system component, return a definition form


;;;
;;; compatibility / utilities

#+ccl
(defun compute-definition-source-information (symbol)
  "Given a symbol return an a-list for the relation between definition type and files.
 The keys can be objects or type names. The values should be pathnames."
  (ccl::get-source-files-with-types&classes symbol))


;;;
;;; extensions to asdf the model

(defmethod component-definitions ((component asdf:cl-source-file))
  (asdf:component-property component :definitions))

(defmethod (setf component-definitions) (definitions (component asdf:cl-source-file))
  (setf (asdf:component-property component :definitions) definitions))

(defmethod asdf:module-components ((file asdf:cl-source-file))
  (component-definitions file))
(defmethod asdf:module-components ((file asdf:source-file))
  ())

(defclass definition (asdf:component)
  ((asdf::name
    :initform (error "name required")
    :reader definition-name)
   (type
    :initform (error "type required")
    :initarg :type
    :reader definition-type)
   (asdf::parent
    :initform (error "parent required")
    :reader definition-parent)
   (depends-on
    :initform nil
    :initarg :depends-on
    :accessor definition-depends-on)))


;;;

(defun component-least-common-parent (c1 c2)
  (cond ((component-ancestor-p c1 c2)
         c1)
        ((component-ancestor-p c2 c1)
         c2)
        ((asdf:component-parent c1)
         (component-least-common-parent (asdf:component-parent c1) c2))))

(defun component-ancestor-p (parent? component)
  (if (eq parent? component)
    parent?
    (when (asdf:component-parent component)
      (component-ancestor-p parent? (asdf:component-parent component)))))

(defun record-dependency (dependent component)
  "record the dependency between DEPENDENT and COMPONENT in the least common parent."
  (flet ((add-depends-on (dep com &aux (com-name (asdf:component-name com)))
           (setf (slot-value dep 'asdf::in-order-to)
            (asdf::union-of-dependencies
             (slot-value dep 'asdf::in-order-to)
             `((asdf:compile-op (asdf:compile-op ,com-name))
               (asdf:load-op (asdf:load-op ,com-name))))))
         (almost-parent (module component)
           (or (find-if #'(lambda (child) (component-ancestor-p child component))
                        (asdf:module-components module))
               (error "lost almost-parent: ~s ~s" module component))))
    (let ((parent (component-least-common-parent dependent component)))
      (if parent
        (add-depends-on (almost-parent parent dependent)
                        (almost-parent parent component))
        (add-depends-on (asdf:component-system dependent)
                        (asdf:component-system component))))))


(defun compute-modules (packages &key (components (make-hash-table :test 'equal))
                                 (module-class 'asdf:module)
                                 (system-class 'asdf:system)
                                 (source-file-class 'asdf:cl-source-file))

  (labels ((record-function (symbol file)
             (definition symbol file 'function))
           
           (record-method (symbol file)
             (definition symbol file 'method))
           
           (record-macro (symbol file)
             (definition symbol file 'macro))
           
           (record-class (symbol file)
             (definition symbol file 'class))

           (definition (symbol pathname type)
             (let ((component (file-component pathname)))
               (or (find-if #'(lambda (d) (and (eq (definition-type d) type)
                                               (eq (definition-name d) symbol)))
                            (component-definitions component))
                   (let ((definition (make-instance 'definition
                                       :name symbol
                                       :parent component
                                       :type type)))
                     (push definition (component-definitions component))
                     (when (eq type 'function)
                       (mapcar #'(lambda (sym)
                                   (dolist (other-definition (record-definitions sym '(function)))
                                     (unless (eq component (definition-parent other-definition))
                                       (record-dependency component other-definition))))
                               (de.setf.utility.clos.graph:function-calls symbol)))
                     definition))))
           
           (module (pathname)
             ;; return a known module, or create one for this directory -
             ;; as well as all parents up to the root
             (or (gethash pathname components)
                 (let* ((parent-directory (butlast (pathname-directory pathname)))
                        (parent-pathname (when (rest parent-directory)
                                           (make-pathname :directory parent-directory
                                                          :defaults pathname)))
                        (parent (when parent-pathname (module parent-pathname)))
                        (module (make-instance (if parent module-class system-class)
                                  :name (first (last (pathname-directory pathname)))
                                  :parent parent)))
                   (push module (asdf:module-components parent))
                   (setf (gethash pathname components) module)
                   (format *trace-output* "~&module: ~s [~s]" module parent)
                   module)))
           
           (file-component (pathname)
             ;; return a known source file, based on the resolved pathname, or
             ;; 
             (setf pathname (or (ignore-errors (truename pathname))
                                (translate-logical-pathname pathname)
                                pathname))
             (or (gethash pathname components)
                 (let* ((module (module (make-pathname :name nil :type nil :defaults pathname)))
                        (component (make-instance source-file-class
                                     :name (pathname-name pathname)
                                     :parent module)))
                   (push component (asdf:module-components module))
                   (setf (gethash pathname components) component)
                   (format *trace-output* "~&component ~s [~s]" component module)
                   component)))

           (record-definitions (symbol &optional (types nil))
             (when (member (symbol-package symbol) packages)
               (remove nil
                       (mapcar #'(lambda (t&f)
                                   (destructuring-bind (type . file) t&f
                                     (cond
                                      ((or (typep type 'function) (member type '(function ccl::accessor setf)))
                                       (when (or (null types) (member 'function types))
                                         (record-function symbol file)))
                                      ((or (typep type 'method) (eql type 'method))
                                       (when (or (null types) (member 'function types))
                                         (record-method symbol file)))
                                      ((or (typep type 'class) (eql type 'class))
                                       (when (or (null types) (member 'class types))
                                         (record-class symbol file)))
                                      ((eq type 'structure)
                                       (when (or (null types) (member 'class types))
                                         (record-class symbol file)))
                                      ((eq type 'macro)
                                       (when (or (null types) (member 'function types))
                                         (record-macro symbol file)))
                                      ((member type '(ccl::slot-name type package))
                                       nil)
                                      ((eq type 'variable)
                                       nil)
                                      ((eq type 'method-combination)
                                       nil)
                                      (t
                                       (warn "unknown definition type: ~s." type)
                                       nil))))
                               (compute-definition-source-information symbol))))))

      (dolist (package packages)
        (format *trace-output* "~&package: ~a" package)
        (with-package-iterator (next package :external :internal)
          (loop (multiple-value-bind (more? symbol) (next)
                  (unless more? (return))
                  (when (or (fboundp symbol) (find-class symbol nil) (macro-function symbol))
                    (record-definitions symbol))))))

      components))

(defgeneric dependency-packages (stem)
  (:documentation
   "return all packages of which the name begins with the given STEM or list of stems.")
  (:method ((package package))
    (dependency-packages (package-name package)))
  (:method ((pattern string))
           (remove-if-not #'(lambda (package)
                              (string-equal (package-name package) pattern
                                            :end1 (min (length (package-name package)) (length pattern))))
                          (list-all-packages)))
  (:method ((patterns list))
           (reduce #'union patterns :key #'dependency-packages)))

(defmethod component-predecessor-names ((component asdf:component))
  (remove-duplicates (reduce  #'union
                              (slot-value component 'asdf::in-order-to)
                              :key #'(lambda (to-do)
                                       (reduce #'union (rest to-do)
                                               :key #'rest :initial-value nil))
                              :initial-value nil)
                     :test #'string-equal))

(defgeneric component-predecessors (component)
  (:method ((component asdf:system))
    (flet ((_find-system (designator)
             (handler-case (asdf:find-system designator) 
               (asdf:missing-component (condition)
                 ;; if someone else hadnles it, ok. otherwise, a continuable error
                 (restart-case (progn (signal condition)
                                      (cerror "Ignore the system."
                                              "System not found: ~s~%~a" designator condition))
                   (use-value (value)
                              (return-from _find-system value)))))))
      (declare (dynamic-extent #'_find-system))
      (remove nil (mapcar #'_find-system (component-predecessor-names component)))))

  (:method ((component asdf:component))
    (let ((parent (asdf:component-parent component)))
      (mapcar #'(lambda (name) (asdf:find-component parent (asdf::coerce-name name)))
              (component-predecessor-names component)))))

(defun component-direct-predecessors (component)
  (let ((predecessors (component-predecessors component)))
    (remove-if #'(lambda (c1)
                   (some #'(lambda (c2) (find c1 (component-predecessors c2)))
                         predecessors))
               predecessors)))

;;; (component-predecessors (ccl:top-inspect-form))

(labels ((find-id (parent name)
           (if parent
             (setf.dot:id (asdf:find-component parent name))
             (format nil "CLUSTER-~a" name)))
         (depends-on-ids (component)
           (mapcar #'(lambda (name) (find-id (asdf:component-parent component) name))
                   (component-predecessor-names component)))
         (sorted-components (component)
           (de.setf.utility:partial-order-sort (copy-list (asdf:module-components component))
                          #'required-p))
         (required-p (other dependent?)
           (or (and (null (component-predecessor-names other))
                    (component-predecessor-names dependent?))
               (find other (component-predecessor-names dependent?))))
         (cluster-node-id (component)
           (format nil "n_~a" (if (or (symbolp component) (stringp component))
                                component
                                (setf.dot:id component)))))
           
  (defgeneric system-declaration (system)
    (:documentation
     "return declaration form(s) for the given system(s).")

    (:method ((systems sequence))
      `(progn ,@(map 'list #'system-declaration systems)))

    (:method ((system hash-table))
      (system-declaration
       (de.setf.utility:partial-order-sort (loop for component being the hash-value of system
                                                 when (typep component 'asdf:system)
                                                 collect component)
                                           #'required-p)))

    (:method ((system asdf:system))
             `(asdf:defsystem ,(asdf:component-name system)
                :class asdf:system
                :pathname ,(asdf:component-pathname  system)
                ,@(let ((names (component-predecessor-names system))) (when names `(:depends-on ,names)))
                :components ,(mapcar #'system-declaration (sorted-components system)))) 

    (:method ((module asdf:module))
             `(:module ,(asdf:component-name module)
               ,@(let ((names (component-predecessor-names module))) (when names `(:depends-on ,names)))
               :components ,(mapcar #'system-declaration (sorted-components module))))

    (:method ((file asdf:source-file))
             `(:file ,(asdf:component-name file)
               ,@(let ((names (component-predecessor-names file))) (when names `(:depends-on ,names)))))))

