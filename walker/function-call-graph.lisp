;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

(in-package :de.setf.utility.implementation)

(:documentation "This file defines a graphviz-based grapher for function calls/called relations for the
 'de.setf.utility' library."
  
  (copyright
   "Copyright 2003, 2010, 220 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
   "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")
  
  (history
   (delta 20201010 "janderson" "use the utility operators to extract function relationships
    and properties, but encode with graphviz api directly, without the abstract walking code.")))

(require :sb-introspect)

(defun graph-function-call-graph (extent &key (stream *standard-output*)
                                 function (root function)
                             (name nil)
                             (count nil)
                             (size *graph-size*)
                             (rankdir *graph-rankdir*)
                             (ratio *graph-ratio*)
                             (margin *graph-margin*)
                             (graph-attributes '())
                             (graph-arguments graph-attributes))
  (let ((walk-count 0)
        (packages (mapcar #'ensure-package extent))
        (nodes (make-hash-table :test #'eq)))
    (labels ((function-label (name)
               (let* ((package (symbol-package name))
                      (p-nick (or (first (package-nicknames package))
                                  (package-name package))))
                 (if (eq package *package*)
                   (symbol-name name)
                   (concatenate 'string p-nick ":" (symbol-name name)))))
             (ensure-package (object)
               (etypecase object
                 (package object)
                 ((or string symbol)
                  (or (find-package object)
                      (error "object does not designate a package: ~s" object)))))
             (put-package (package)
               (flet ((put-package-content ()
                        (loop for name being each external-symbol in package
                          when (fboundp name)
                          do (progn
                               (setf.dot:put-node name)
                               (setf (gethash name nodes) (function-label name))))
                        (loop for name being each present-symbol in package
                          when (fboundp name)
                          do (progn
                               (setf.dot:put-node name)
                               (setf (gethash name nodes) (function-label name))))))
                 (setf.dot:context-put-subgraph setf.dot:*context*
                                        (package-name package)
                                        #'put-package-content)))
             (put-calls (function-name function-label)
               (let* ((function (fdefinition function-name))
                      ;; callers is too slow
                      (callees (find-calls function)))
                 (loop for called in callees
                   for called-name = (sb-kernel:%fun-name called)
                   when (symbolp called-name)
                   do (progn
                        (setf.dot:put-edge function-label (function-label called-name))
                        (incf walk-count)))))
             (find-calls (function)
               (typecase function
                 ((or sb-kernel:closure generic-function)
                  nil
                  #+(or)(loop for method in (sb-mop:generic-function-methods function)
                          append (SB-INTROSPECT:FIND-FUNCTION-CALLEES (sb-mop:method-function method))))
                 (function (SB-INTROSPECT:FIND-FUNCTION-CALLEES function)))))
                          
        
      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                                &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream
               (or name
                   (if (functionp root)
                       (function-label root)
                       "?"))
               #'(lambda ()
                   (loop for package in packages
                     do (put-package package))
                   (print nodes)
                   (loop for name being each hash-key of nodes
                     using (hash-value label)
                     do (progn (put-calls name label)
                          (when (and count (>= walk-count count)) (return)))))
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               :pretty t
               graph-arguments)))
    
    walk-count))

(defparameter *call-path* ()
  "Binds a dynamically constructed call tree to add to links as classes")

(defun dot-function-caller-graph (functions &key
                                              (stream *standard-output*)
                                              (extent nil)
                                              (limit nil)
                                              (name nil)
                                              (count nil)
                                              (size *graph-size*)
                                              (rankdir *graph-rankdir*)
                                              (ratio *graph-ratio*)
                                              (margin *graph-margin*)
                                              (graph-attributes '())
                                              (graph-arguments graph-attributes))
  "Emit a graphviz representation of the function caller graph.
   This traces backwards, from end-functions to start names.
   That direction is supported by sbcl - as opposed to the call graph"
  (flet ((coerce-to-name (designator)
           (cond ((functionp designator)
                  (sb-kernel:%fun-name designator))
                 ((and (symbolp designator) (fboundp designator))
                  designator)
                 ((and (consp designator) (eq (first designator) 'setf) (fboundp designator))
                  designator)
                 (t
                  (warn "~a is unbound" designator)
                  nil)))
         (coerce-to-function (designator)
           (cond ((functionp designator)
                  designator)
                 ((and (symbolp designator) (fboundp designator))
                  (fdefinition designator))
                 (t
                  (warn "~a is unbound" designator)
                  nil))))
    (let ((walk-count 0)
          (called-functions (loop for designator in functions
                           for function = (coerce-to-function designator)
                           when function collect function))
          (start-names (loop for designator in limit
                           for name = (coerce-to-name designator)
                           when name collect name))
          (nodes (make-hash-table :test #'equalp))
          (edges (make-hash-table :test #'equalp))
          (package-labels (make-hash-table :test 'equal))
          (package-nodes (make-hash-table :test 'eq)))
      (setf extent
            (loop for designator in extent
              for package = (etypecase designator
                              (package designator)
                              ((or string symbol) (find-package designator)))
              when package
              collect package))
      (labels ((package-label (package)
                 (or (gethash (package-name package) package-labels)
                     (setf (gethash (package-name package) package-labels)
                           (first (sort (copy-list (cons (package-name package) (package-nicknames package)))
                                        #'string-lessp)))))
               (function-name (function)
                 (labels ((coerce-name (name)
                            (typecase name
                              (symbol name)
                              (cons
                               (case (first name)
                                 (setf ;; skip setf operators
                                  nil)
                                 ((flet labels)
                                  (destructuring-bind (subname &key in)
                                                      (rest name)
                                    (declare (ignore subname))
                                    (coerce-name in)))
                                 (sb-pcl::fast-method
                                  (destructuring-bind (generic-function-name &rest qualifiers-and-specializers)
                                                      (rest name)
                                    (declare (ignore qualifiers-and-specializers))
                                    (coerce-name generic-function-name)))
                                 (lambda
                                     (destructuring-bind (args  &key in)
                                                         (rest name)
                                       (declare (ignore args))
                                       (coerce-name in)))
                                 ((SB-IMPL::%HANDLER-BIND MACRO-FUNCTION
                                                          compiler-macro
                                                          SB-C::TOP-LEVEL-FORM
                                                          SB-IMPL::SETF-EXPANDER
                                                          SB-EXT:CAS)
                                  nil)
                                 (t
                                  (warn "unknown function name type: ~s" (first name))
                                  nil)))
                              (t
                               (warn "unknown function name: ~s" name)
                               nil))))
                   (coerce-name (sb-kernel:%fun-name function))))
             (find-callers (function)
               (sb-introspect:find-function-callers function))
             (put-function (function)
               (let* ((name (function-name function)))
                 ;; (print (list :name name))
                 (when (and name (member (symbol-package name) extent))
                   (multiple-value-bind (known) (gethash name nodes)
                     (cond (known
                            (when (functionp known)
                              name))
                           ((member name start-names :test #'equal)
                            ;; stop tracing callers at boundary
                            (setf (gethash name nodes) (fdefinition name))
                            (push name (gethash (symbol-package name) package-nodes))
                            (incf walk-count)
                            name)
                           (t
                            (let ((abstract-function (fdefinition name))
                                  (*call-path* (cons (name-id name) *call-path*)))
                              (setf (gethash name nodes) t) ;; stop traversal
                              (when (plusp (loop for caller in (find-callers abstract-function)
                                             count (let ((caller-name (put-function caller)))
                                                     (when caller-name
                                                       (push caller-name (gethash (symbol-package caller-name) package-nodes))
                                                       (unless (gethash (list caller-name name) edges)
                                                         (setf (gethash (list caller-name name) edges)
                                                               (cons (name-id caller-name) *call-path*))
                                                         )))))
                                (push name (gethash (symbol-package name) package-nodes))
                                (incf walk-count)
                                ; (setf (get :put-function name) t)
                                (setf (gethash name nodes) abstract-function) ;; update, to indicate that it was a caller
                                name))))))))
               (name-id (name)
                 (substitute-if #\_ #'(lambda (c) (not (or (alphanumericp c) (eql c #\-) (eql c #\_))))
                                (format nil "~a:~a" (package-name (symbol-package name)) name))))

      (destructuring-bind (&key (size size) (rankdir rankdir) (margin margin) (ratio ratio)
                                &allow-other-keys)
                          graph-arguments
        (apply #'setf.dot:context-put-graph stream
               (or name "?")
               #'(lambda ()
                   (loop for function in called-functions
                     until (and count (>= walk-count count))
                     do (put-function function))
                   (loop for package being each hash-key of package-nodes
                     using (hash-value nodes)
                     for count from 0
                     do (setf.dot:context-put-subgraph setf.dot:*context*
                                                       (format nil "cluster_~a" (package-name package))
                                                       #'(lambda ()
                                                           (setf nodes (remove-duplicates nodes))
                                                           (format *trace-output* "~%cluster ~a x ~s"
                                                                   (package-name package)
                                                                   (length nodes))
                                                           (loop for name in nodes
                                                             for label = (symbol-name name)
                                                             for id = (name-id name)
                                                             do (setf.dot:put-node id
                                                                                   :penwidth (if (or (member name functions)
                                                                                                     (member name limit))
                                                                                                 "3.0" "1.0")
                                                                                   :label label
                                                                                   :class id)))
                                                       :label (package-name package)
                                                       :color (format nil "/spectral8/~a" (1+ (mod count 8)))
                                                       :node `(:color ,(format nil "/spectral8/~a" (1+ (mod count 8))))))
                   ;; must emit the edges after the nodes in order for the subgraph defaults to work
                   (loop for edge being each hash-key of edges
                     using (hash-value call-path)
                     for (caller-name called-name) = edge
                     do (setf.dot:put-edge (name-id caller-name) (name-id called-name)
                                           :class (format nil "~{~a~^ ~}" call-path))))
               :size size
               :ratio ratio
               :rankdir rankdir
               :margin margin
               :pretty t
               graph-arguments)))

    (values stream
            walk-count))))


;;; this requires a runnable dot utility.
;;; it should be relatively recent in order to support the css class attributes for svg generation

(defun html-function-caller-graph (functions &rest args)
  (multiple-value-bind (stream count)
                       (apply #'dot-function-caller-graph functions args)
    (sb-ext:run-program "/usr/bin/dot" (list "-Tsvg" "-o" (namestring (make-pathname :type "svg" :defaults stream))
                                      (namestring stream))
                 :wait t)
    (with-open-file (html (make-pathname :type "html" :defaults stream)
                          :direction :output :if-does-not-exist :create :if-exists :supersede)
      (with-open-file (svg (make-pathname :type "svg" :defaults stream) :direction :input)
        (write-string "<html>
<html>
<head>
<style id='style'>
 path { stroke: grey }
 ellipse { pointer-events: mousedown; fill: white }
 </style>
</head>
<body>
 <div style='transform: translate(100%,100%) scale(2.0);'>
"
                      html)
        (alexandria:copy-stream svg html)
        (write-string "
 </div>
 <script id='script'>
 function highlight(event) {
   event.preventDefault();
   var element = event.target;
   var g = element.closest('g');
   var className = g.className;
   className = className.baseVal.split(' ')[1];
   document.querySelector('#style').textContent =
    `path {stroke:gray}
     ellipse { pointer-events: mousedown; fill: white }
     g.${className} ellipse { stroke-width: 4px; stroke: blue; fill: lightcyan}
     g.${className} path { stroke-width: 4px; stroke: blue; stroke-dasharray: 1 2}`
   //console.log('style', document.querySelector('#style').textContent);
 }

 document.querySelectorAll('ellipse').forEach(function(element) {
   element.addEventListener('mousedown', highlight, true);
 });
 </script>
</body>
</html>
"
                      html)
        (values html
                count)))))
    

#+(or)
(progn
  (load "/development/source/library/de/setf/utility/walker/parameters.lisp")
  (load "de/setf/utility/walker/function-call-graph.lisp")
  (graph-function-call-graph '(:spocq.i :spocq :spocq.si :dydra :rdfcache :dydra-ndk)
                             :stream #p"/tmp/spocq-call-graph.dot"
                             :name "spocq-call-graph")
  (dot-function-caller-graph '(rlmdb:map-repository-statements rlmdb::map-index-statements)
                             :stream #p"/tmp/spocq-caller-graph.dot"
                             :name "spocq-caller-graph"
                             :extent '(:spocq.i :spocq :spocq.si :dydra :rdfcache :dydra-ndk
                                       :rlmdb rlmdb.i)
                             :limit '(spocq.si::graph-store-response)
                             )

  (html-function-caller-graph '(rlmdb:map-repository-statements rlmdb::map-index-statements)
                             :stream #p"/tmp/spocq-caller-graph.dot"
                             :name "spocq-caller-graph"
                             :extent '(:spocq.i :spocq :spocq.si :dydra :rdfcache :dydra-ndk
                                       :rlmdb rlmdb.i)
                             :limit '(spocq.si::graph-store-response)
                             :graph-arguments '(:ranksep "2in")
                             )



  (defun cl-user::leaf1 ()) (defun cl-user::leaf2 ())
  (defun branch () (cl-user::leaf2))
  (defun limit-function () (cl-user::leaf1) (branch))
  (html-function-caller-graph '(cl-user::leaf1 cl-user::leaf2)
                              :stream #p"/tmp/test-caller-graph.dot"
                              :name "spocq-caller-graph"
                              :extent `(:cl-user ,(package-name *package*))
                              :limit '(limit-function)
                              :graph-arguments '(:ranksep "2in")
                             )


  (SB-INTROSPECT:FIND-FUNCTION-CALLEES #'spocq.si::graph-store-response)
  )



