;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  <p>This file is part of the 'de.setf.utility.dot' (or 'setf.dot') library component.
  It implements generation and construction operators and graphviz shell operators.
  The generation interfaces are various:
  <ul><li>STREAM-WRITE-statement encoding operators which combine an initial context argument with
    respective required and optional arguments;</li>
   <li>ENCODE-statement operators which expect a dynamic binding for setf.dot:*context* to specify
    the context</li>
   <li>statement macro operators which interpret graph model forms directly.</li></ul>
  The model represents graphs as lists with statement identifiers as operators. The arguments and
  attributes appear (roughly) analogus to the abstract syntax structure.</p>
  </DESCRIPTION>
 <COPYRIGHT YEAR='2009' AUTHOR='james adam anderson' href='mailto:james.anderson@setf.de'>
  'setf.dot' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'setf.dot' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'setf.dot'.  If not, see the GNU <a href='http://www.gnu.org/licenses/'>site</a>.
  </COPYRIGHT>
 <CHRONOLOGY>
  <DELTA DATE='20021212'>abstracted from xqdm-graph</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#


(in-package :de.setf.utility.implementation)

;;;
;;; parameters

(deftype setf.dot:statement-type ()
  '(member setf.dot:graph setf.dot:digraph setf.dot:subgraph setf.dot:node setf.dot:edge))
(deftype setf.dot:graph-type ()
  '(member setf.dot:graph setf.dot:digraph))

(defvar setf.dot:*pretty* nil
  "when true, stream encoding introduces an eol and indentation before every constituent statement.")

(defvar setf.dot:*level* 0
  "the nesting level determines the indentation when encoding graph statements prettily.")

(defvar setf.dot:*eol*
  (make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed))
  "The 'windows-compatible' eol form must include both CR and LF.")

(defvar setf.dot:*type* 'setf.dot:digraph
  "the default graph type")

(defvar setf.dot:*context* nil
  "binds the current dot processing context during the dynamic extent of a
 setf.dot:context-put-graph and/or setf.dot:call-with-context invocation.")

(defvar setf.dot:*content* nil
  "binds graph/subgraph constituents while constructing a model.")

(defvar setf.dot:*graphviz-pathname*
  (make-pathname :host nil
                 :directory
                 #+digitool `(:absolute ,(second (pathname-directory (truename "CCL:"))) "opt" "local" "bin")
                 #-digitool '(:absolute "opt" "local" "bin")
                 :name nil :type nil)
  "Pathname location for the Graphviz binary root.")

(defvar setf.dot::*restricted-labels*
  '("graph")
  "certain labels just fail to parse")


;;;
;;; Classes
;;;  delegate-stream
;;;  setf.dot:context
;;;  setf.dot:stream
;;;  setf.dot:constructor

(defclass delegate-stream (#+ccl stream
                           #+lispworks stream:fundamental-stream
                           #+sbcl sb-gray:fundamental-stream)
  ((stream
    :initform (error "stream is required")
    :initarg :stream
    :reader delegate-stream-stream
    :type stream))
  (:documentation
   "a <code>DELEGATE-STREAM</code> mixes-in standard stream methods by
 delegating them to another stream"))

#+mcl
(defmethod stream-write-sequence ((delegate delegate-stream) (string string) &rest args)
  (apply #'stream-write-sequence (delegate-stream-stream delegate) string args))

#+sbcl
(defmethod stream-write-sequence ((delegate delegate-stream) (sequence sequence) &optional (start 0) (end nil))
  (write-sequence sequence (delegate-stream-stream delegate) :start start :end end))


(defmethod stream-write-string ((delegate delegate-stream) (string string) #-digitool &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length string)))
  ;; some lisps are particular about which streams get stream-write-string, thus ...
  (write-string string (delegate-stream-stream delegate) :start start :end end))

;;; support whichever one the implementation uses
(defmethod stream-tyo ((delegate delegate-stream) (datum t))
  (stream-tyo (delegate-stream-stream delegate) datum))

(defmethod stream-write-char ((delegate delegate-stream) (datum t))
  ;; some lisps are particular about which streams get stream-write-char, thus ...
  (write-char datum (delegate-stream-stream delegate)))

(defmethod stream-line-column ((delegate delegate-stream))
  nil)

(defmethod stream-line-length ((delegate delegate-stream))
  nil)

#+digitool
(defmethod stream-line-column ((stream stream))
  (stream-column stream))

#+sbcl
(defmethod sb-GRAY:STREAM-LINE-LENGTH ((stream delegate-stream))
  nil)


(defClass setf.dot:context ()
  ((setf.dot:name
    :initform (gensym "GRAPH-") :initarg :graph-name
    :accessor setf.dot:name
    :documentation "names the dot graph")
   (setf.dot:type
    :initform setf.dot:*type* :initarg :type :type setf.dot:graph-type
    :reader setf.dot:type
    :documentation "specified whether the graph is directed.")
   (setf.dot:strict
    :initform nil :initarg :strict
    :reader setf.dot:strict
    :documentation
    "Indicates that the graph should be interpreted strictly respectively as
 bi-directional or non-directional, as given by the type. in that case, the
 link form of the individual edges is ignored.")
   (setf.dot:pretty
    :initarg :pretty :initform nil
    :reader setf.dot:pretty)
   (setf.dot:eol
    :initarg :eol :initform setf.dot:*eol*
    :reader setf.dot:eol
    :documentation
    "network-standard eol sequence for use with .dot files. otherwise windows
 interpreters can't parse them.")
   (setf.dot:cache :initform (make-hash-table) :reader setf.dot:cache))
  (:documentation
   "A <code>CONTEXT</code> combines global graph attributes with a node
 unification cache. It contributes to one specialization for the
 <code>CONTEXT-PUT-*</code> operators to map <code>STANDARD-OBJECT</code>
 node and edge arguments to id values."))


(defgeneric setf.dot::context-id (context object &optional prefix)
  (:documentation
   "Return a (possibly new) unified id for an object within a given context.
 Accept an optional prefix  for generated ids, which defaults to the object's
 type name.")

  (:method ((context setf.dot:context) (object standard-object) &optional prefix)
    (let ((cache (setf.dot:cache context)))
      (or (gethash object cache)
          (setf (setf.dot::context-id context object) (gensym (string (or prefix (type-of object)))))))))


(defgeneric (setf setf.dot::context-id) (id context object)
  (:documentation
   "Set the id for an object within a given context.")
 
  (:method (id (context setf.dot:context) (object standard-object))
    (setf (gethash object (setf.dot:cache context)) id)))


(defclass setf.dot:stream (setf.dot:context delegate-stream)
  ()
  (:documentation
   "A <code>STREAM</code> combines stream delegation with <code>CONTEXT</code>
 support for id unification."))


(defclass setf.dot:constructor (setf.dot:context)
  ()
  (:documentation
   "<code>CONSTRUCTOR</code> specializes methods for <code>CONTEXT-PUT-*</code>
 to build an in-memory model for the graph."))

(defmethod setf.dot:eol ((stream stream))
  "for an unspecialzied stream, return the default eol value."
  setf.dot:*eol*)

(defgeneric setf.dot:write-eol (stream)
  (:method ((stream stream))
    (write-string (setf.dot:eol stream) stream))
  (:method ((destination t))
    ;; do nothing
    nil))

(defgeneric setf.dot:fresh-line (&optional stream)
  (:method (&optional (stream setf.dot:*context*))
    (unless (eql (stream-line-column stream) 0)
      (setf.dot:write-eol stream)
      (dotimes (i setf.dot:*level*) (write-char #\space stream)))))

(defun setf.dot:call-with-context (op context)
  (let ((setf.dot:*context* context)) (funcall op)))

(defmacro setf.dot:with-context (context &rest body)
  "bind a new dot context and execute the body."
  (let ((op (gensym "body")))
    `(flet ((,op () ,@body)) (declare (dynamic-extent #',op))
           (setf.dot:call-with-context #',op ,context))))

;;;
;;; .dot terminal operators, both primitive and variations

(defgeneric setf.dot:stream-write-escaped-string (stream string &optional case)
  (:method ((stream stream) (string string) &optional (case nil))
    "write out the string with returns and quotes escaped"
    (let ((char #\0)
          (downcase-p (ecase case
                        ((nil :upcase) nil)
                        ((t :downcase) t))))
      (do ((i 0 (1+ i)))
          ((>= i (length string)))
        (case (setf char (char string i))
          (#.(code-char #x0A) (write-string "\\n" stream))
          (#\" (write-string "\\\"" stream))
          (t
           (write-char (if (and downcase-p (upper-case-p char))
                         (char-downcase char) char)
                       stream)))))))

(defun stream-write-casefolded-string (stream string &optional case)
  (ecase case
    (:upcase (format stream "~:@(~a~)" string))
    ((t :downcase) (format stream "~(~a~)" string))
    ((nil) (write-string string stream))))


(defgeneric setf.dot:stream-write-id (stream id &optional case)
  (:method ((stream stream) (id cons) &optional (case nil))
    "support subgraph references"
    (destructuring-bind (tag . id-id) id
      (ecase tag
        (setf.dot:subgraph
          (unless (atom id-id) (error "invalid id: ~s" id))
          (write-string "subgraph " stream)
          (setf.dot:stream-write-id stream id-id case))
        (:port
         (setf.dot:stream-write-id stream (first id-id))
         (write-char #\: stream)
         (setf.dot:stream-write-id stream (second id-id))))))
  (:method ((stream stream) (id string) &optional (case nil))
    (flet ((id-char-p (c)
             (or (alphanumericp c)
                 (eql c #\_))))
      (cond ((and (every #'id-char-p id)
                  (plusp (length id))
                  (not (digit-char-p (char id 0)))
                  (not (member id setf.dot::*restricted-labels* :test #'string-equal)))
             (stream-write-casefolded-string stream id case))
            (t
             (write-char #\" stream)
             (setf.dot:stream-write-escaped-string stream id case)
             (write-char #\" stream)))))
  (:method ((stream stream) (id-generator function) &optional case)
           (declare (ignore case))
           (write-char #\" stream)
           (funcall id-generator stream)
           (write-char #\" stream))
  (:method ((stream stream) (id null) &optional case)
           (declare (ignore case))
           (write-string "\" \"" stream))
  (:method ((stream stream) (id symbol) &optional case)
           (setf.dot:stream-write-id stream (string id) case))
  (:method ((stream stream) (id t) &optional case)
    (write-char #\" stream)
    (stream-write-casefolded-string stream (princ-to-string id) case)
    (write-char #\" stream)))

(defstruct setf.dot:rgb red green blue)
(defmethod make-load-form ((rgb setf.dot:rgb) &optional env)
  (declare (ignore env))
  (values `(setf.dot:rgb ,(setf.dot:rgb-red rgb)
                         ,(setf.dot:rgb-green rgb)
                         ,(setf.dot:rgb-blue rgb))
          nil))
(defun setf.dot:rgb (red green blue) (setf.dot:make-rgb :red red :green green :blue blue))
(defun setf.dot::rgb-lighten (rgb)
  (flet ((integer-value (value)
           (etypecase value
             (integer value)
             (float (floor (* value 255)))
             (rational (floor (* value 255)))))
         (lighten (value)
           (+ value (ash (- 255 value) -1))))
    (setf.dot:rgb (lighten (integer-value (setf.dot:rgb-red rgb)))
                  (lighten (integer-value (setf.dot:rgb-green rgb)))
                  (lighten (integer-value (setf.dot:rgb-blue rgb))))))

        

(defgeneric setf.dot:stream-write-attribute (stream name value)
  (:documentation
   "write the attribute name / value equation with provisions to downcase the name.")
  (:argument-precedence-order stream value name)
  (:method ((stream stream) (name string) (value t))
           (setf.dot:stream-write-id stream name
                                (if (string-equal name :url)
                                  :upcase
                                  :downcase))
           (write-char #\= stream)
           (setf.dot:stream-write-id stream value))

  (:method ((stream stream) (name symbol) (value t))
           (setf.dot:stream-write-id stream name
                                (if (string-equal name :url)
                                  :upcase
                                  :downcase))
           (write-char #\= stream)
           (setf.dot:stream-write-id stream value))

  (:method ((stream stream) (name t) (value-generator function))
           (setf.dot:stream-write-id stream name :downcase)
           (write-char #\= stream)
           (setf.dot:stream-write-id stream value-generator))

  (:method ((stream stream) (name t) (value t))
           (setf.dot:stream-write-id stream name :downcase)
           (write-char #\= stream)
           (setf.dot:stream-write-id stream value))

  (:method ((stream stream) (name t) (value setf.dot:rgb))
    (labels ((write-value (value)
               (etypecase value
                 (integer (assert (<= 0 value 255) () "invalid color channel value: ~s." value)
                          (format stream "~2,'0X" value))
                 (float (write-value (floor (* value 255))))
                 (rational (write-value (floor (* value 255))))))
             (write-color (stream)
               (write-char #\# stream)
               (write-value (setf.dot:rgb-red value))
               (write-value (setf.dot:rgb-green value))
               (write-value (setf.dot:rgb-blue value))))
      (declare (dynamic-extent #'write-color))
      (setf.dot:stream-write-attribute stream name #'write-color))))


(defgeneric setf.dot:stream-write-node-id (stream name &key location angle)
  (:documentation
   "a node id permits a location and an angle in addition to the id itself.")
  (:method ((stream stream) (name t) &key location angle)
           (setf.dot:stream-write-id stream name)
           (when location
             (write-char #\: stream)
             (cond ((consp location)
                    (write-char #\( stream)
                    (setf.dot:stream-write-id stream (first location))
                    (write-char #\, stream)
                    (setf.dot:stream-write-id stream (rest location))
                    (write-char #\) stream))
                   (t
                    (setf.dot:stream-write-id stream location))))
           (when angle
             (write-char #\@ stream) (setf.dot:stream-write-id stream angle)))
  (:method ((stream stream) (node cons) &rest args)
    "encode the id from an s-exp node"
    (declare (dynamic-extent args))
    (apply #'setf.dot:stream-write-node-id stream (first node) args)))

(defgeneric setf.dot:stream-write-head-id (stream id)
  (:method ((stream stream) (id t))
           (setf.dot:stream-write-id stream id))
  (:method ((context setf.dot:context) (head standard-object))
           (setf.dot:stream-write-node-id context (setf.dot::context-id context head))))

(defgeneric setf.dot:stream-write-tail-id (stream id)
  (:method ((stream stream) (id t))
           (setf.dot:stream-write-id stream id))
  (:method ((context setf.dot:context) (head standard-object))
           (setf.dot:stream-write-tail-id context (setf.dot::context-id context head))))


;;;
;;; iteration support

(defgeneric setf.dot:map-graph-subgraphs (stream function graph)
  (:method ((stream stream) (function t) (graph t)) ))

(defgeneric setf.dot:map-graph-nodes (stream function graph)
  (:method ((stream stream) (function t) (graph t)) ))

(defgeneric setf.dot:map-graph-edges (stream function graph)
  (:method ((stream stream) (function t) (graph t)) ))

(defgeneric setf.dot:map-graph-attributes (stream function graph)
  (:method ((stream stream) (function t) (graph t)) ))

(defun setf.dot:map-attributes (function attributes)
  (dolist (attribute attributes)
    (destructuring-bind (key . value) attribute (funcall function key value)))
  attributes)

(defgeneric setf.dot:map-edge-attributes (stream edge function)
  (:documentation "generate dot edge attributes from a concrete or generated edge.")
  (:method ((stream stream) (edge t) (function t))
           "the general method does nothing"
           edge)
  (:method ((stream stream) (edge cons) (function t))
           "generate the dot node attributes from a node of the form (name . ((key . value) . rest))"
           (destructuring-bind (tag out-node in-node . attributes) edge
             (declare (ignore tag out-node in-node))
             (setf.dot:map-attributes function attributes))
           edge))

(defgeneric setf.dot:map-node-attributes (stream node function)
  (:documentation "generate dot node attributes from a concrete or generated node.")
  (:method ((stream stream) (node t) (function t))
           "the general method does nothing"
           node)
  (:method ((stream stream) (node cons) (function t))
           "generate the dot node attributes from a node of the form (name . ((key . value) . rest))"
           (destructuring-bind (tag name . attributes) node
             (declare (ignore tag name))
             (setf.dot:map-attributes function attributes))
           node))

;;;
;;; basic graph constituent encoding
;;;   context-put-graph
;;;   context-put-subgraph
;;;   context-put-node
;;;   context-put-edge
;;;
;;; each requires those arguments corresponding to fixed grapah content and
;;; accepts additional keywords for the respective element's attributes.
;;; where the element includes further content elements (graph and subgraph
;;; the required parameters are specialized for either a basic stream, or
;;; one mixed with setf.dot:context behaviour, combined with
;;;
;;; nb. the older write-{graph,node,edge} interface constituted an alternative
;;; control structure, in which the operator was abstracted over the node-as-id and node-as-attributes
;;; using generic write-*-id and map-*-attributes to process literal, s-expression, and instance data.
;;; the present version abstracts the id-extraction only and requires that attributes be martialed
;;; by a specialized methods in order to be passed as keyword arguments.
;;; this is simpler on several counts:
;;;  - there are fewer operators;
;;;  - the permitted and used attributes are more apparent in the code.

(defgeneric setf.dot:context-put-graph (stream name statements
                                     &key
                                     if-exists if-does-not-exist       ; file creation
                                     type strict name statement-type pretty ; global properties
                                     graph node edge    ; default attributes, by respective statement type
                                     aspect bb bgcolor center charset clusterrank color colorscheme comment
                                     compound concentrate
                                     damping defaultdist dim dimen diredgeconstraints dpi epsilon esep
                                     fillcolor fontcolor fontname fontnames fontpath fontsize 
                                     id label labeljust labelloc landscape layers layersep layout levels levelsgap lp
                                     margin maxiter mclimit mode model mosek
                                     nodesep nojustify normalize nslimit nslimit1
                                     ordering orientation outputorder overlap overlap_scaling
                                     pack packmode pad page pagedir pencolor penwidth peripheries
                                     quadtree quantum
                                     rank rankdir ranksep ratio remincross repulsiveforce resolution root rotate
                                     samplepoints searchsize sep showboxes size smoothing sortv splines start
                                     style stylesheet target truecolor URL viewport voro_margin)

  (:argument-precedence-order name statements stream)

  (:documentation "Encode a graph with NAME and given STATEMENTS to the given STREAM.
 Accept the 'N' attributes per [http://www.graphviz.org/doc/info/attrs.html].
 In addition, accept and sequester keywords which apply to stream instantiation rather than
 graph content, such as IF-EXISTS and IF-DOES-NOT-EXIST, and expand global attributes for
 GRAPH, NODE, and EDGE.
 Accept attributes such as COLOR and FILLCOLOR for use with clusters.")

  (:method ((pathname string) (name t) (statements t) &rest args)
    (apply #'setf.dot:context-put-graph (pathname pathname) name statements args))

  (:method ((pathname pathname) (name t) (statements t) &rest args
            &key (if-does-not-exist :create) (if-exists :supersede) &allow-other-keys)
    (with-open-file (stream pathname :direction :output :if-does-not-exist if-does-not-exist
                            :if-exists if-exists)
      (apply #'setf.dot:context-put-graph (make-instance 'setf.dot:stream :stream stream) name statements args)))

  (:method ((context setf.dot:context) name statements &rest args
            &key (strict (setf.dot:strict context)) (type (setf.dot:type context))
            (pretty (setf.dot:pretty context))
            &allow-other-keys)
    (setf (setf.dot:name context) name)
    (let ((*gensym-counter* (hash-table-count (setf.dot:cache context))))
      (apply #'call-next-method context name statements
             :strict strict :type type
             :pretty pretty
             args)))

  (:method ((setf.dot:*context* stream) (name t) (statement-generator function) &rest attributes)
    (declare (dynamic-extent statement-generator attributes))
    (flet ((write-graph-attribute (name value)
             (when setf.dot:*pretty*
               (setf.dot:fresh-line setf.dot:*context*))
             (write-char #\space setf.dot:*context*)
             (setf.dot:stream-write-attribute setf.dot:*context* name value)
             (write-string ";" setf.dot:*context*))
           (write-attribute-list (scope attributes &aux (attributes-p nil))
             (format setf.dot:*context* "~(~a~) [" scope)
             (loop for (name value) on attributes by #'cddr
                   do (progn (setf.dot:stream-write-attribute setf.dot:*context* name value)
                             (if attributes-p
                               (write-string ", " setf.dot:*context*)
                               (setf attributes-p t))))
             (write-string "];" setf.dot:*context*)))
      (destructuring-bind (&key strict ((:type setf.dot:*type*) setf.dot:*type*)
                                (statement-type setf.dot:*type*)
                                graph node edge
                                ((:pretty setf.dot:*pretty*) setf.dot:*pretty*)
                                &allow-other-keys)
                          attributes
        (when (and setf.dot:*pretty* (plusp setf.dot:*level*))
          (setf.dot:fresh-line setf.dot:*context*))
        (let ((setf.dot:*level* (1+ setf.dot:*level*)))
          (format setf.dot:*context* "~:[~;strict ~]~(~a~) " strict statement-type)
          (setf.dot:stream-write-id setf.dot:*context* name)
          (write-string " {" setf.dot:*context*)
          (loop for (name value) on attributes by #'cddr
                unless (member name '(:name :strict :type :graph :node :edge :pretty
                                      :statement-type
                                      :if-exists :if-does-not-exist))
                do (write-graph-attribute name value))
          (when setf.dot:*pretty* (setf.dot:write-eol setf.dot:*context*))
          (when graph (write-attribute-list :graph graph))
          (when setf.dot:*pretty* (setf.dot:write-eol setf.dot:*context*))
          (when node (write-attribute-list :node node))
          (when setf.dot:*pretty* (setf.dot:write-eol setf.dot:*context*))
          (when edge (write-attribute-list :edge edge))
          (funcall statement-generator)
          (when setf.dot:*pretty* (setf.dot:write-eol setf.dot:*context*))
          (write-char #\} setf.dot:*context*)
          (when setf.dot:*pretty* (setf.dot:write-eol setf.dot:*context*))))))

  (:method ((stream stream) (name t) (graph-instance standard-object) &rest args)
    (flet ((encode-content ()
             (flet ((write-attribute (name value)
                      (setf.dot:stream-write-attribute stream name value)
                      (write-string "; " stream))
                    (write-node (node &rest args)
                      (declare (dynamic-extent args))
                      (apply #'setf.dot:context-put-node stream node args))
                    (write-edge (edge &rest args)
                      (declare (dynamic-extent args))
                      (apply #'setf.dot:context-put-edge stream edge args)))
               (declare (dynamic-extent #'write-attribute #'write-node #'write-edge))
               (setf.dot:map-graph-attributes stream #'write-attribute graph-instance)
               (setf.dot:map-graph-subgraphs stream #'write-attribute graph-instance)
               (setf.dot:map-graph-nodes stream #'write-node graph-instance)
               (setf.dot:map-graph-edges stream #'write-edge graph-instance))))
      (apply #'setf.dot:context-put-graph stream name #'encode-content args)))

  (:method ((stream stream) (name t) (statements cons) &rest args)
    "delegate to the function specialization with an encoder which iterates over the content
 and encodes each constituent. requirethe content form
 ( [ ((setf.dot:subgraph name attributes . statements) |
      (setf.dot:node name . attributes) |
      (setf.dot:edge name1 name2 . attributes) ]* )"
    (flet ((statement-encoder ()
             (dolist (statement statements)
               (setf.dot:write-eol stream)
               (ecase (car statement)
                 (setf.dot:node (apply #'setf.dot:context-put-node stream (rest statement)))
                 (setf.dot:edge (apply #'setf.dot:context-put-edge stream (rest statement)))
                 (setf.dot:subgraph
                  (destructuring-bind (name attributes &rest statements)
                                      (rest statement)
                    (declare (dynamic-extent statements))
                    (apply #'setf.dot:context-put-subgraph stream name statements attributes)))))))
      (declare (dynamic-extent #'statement-encoder))
      (apply #'setf.dot:context-put-graph stream #'statement-encoder name args))))

(defun setf.dot:context-put-subgraph (context name content &rest attributes)
  (declare (dynamic-extent attributes))
  (apply #'setf.dot:context-put-graph context name content
         :strict nil
         :statement-type 'setf.dot:subgraph
         attributes))
 
(defgeneric setf.dot:context-put-node (stream id
                                              &key
                                              location angle     ; id arguments
                                              bottomlabel
                                              color colorscheme comment distortion
                                              fillcolor fixedsize fontcolor fontname fontsize
                                              group height image imagescale label labelloc layer margin nojustify
                                              orientation penwidth peripheries pos rechts regular root
                                              samplepoints shape shapefile showboxes sides skew sortv style
                                              target tooltip
                                              URL width z)
  (:argument-precedence-order id stream)

  (:documentation "Encode a graph node with ID to the given STREAM.
 Accept the 'N' attributes per [http://www.graphviz.org/doc/info/attrs.html].
 In addition, accept and sequester sub-attributes such as LOCATION and ANGLE to merge into
 the respective bases.")

  (:method ((stream stream) id &rest attributes &aux (attributes-p nil))
    (declare (dynamic-extent attributes))
    (flet ((write-attribute (name value)
                      (cond (attributes-p
                             (write-string ", " stream))
                            (t
                             (setf attributes-p t)))
                      (setf.dot:stream-write-attribute stream name value)))
      (destructuring-bind (&key location angle &allow-other-keys) attributes
        (when (and setf.dot:*pretty* (plusp setf.dot:*level*))
          (setf.dot:fresh-line stream))
        (setf.dot:stream-write-node-id stream id :location location :angle angle)
        (when attributes
          (write-char #\space stream)
          (write-char #\[ stream)
          (loop for (name value) on attributes by #'cddr
                unless (member name '(:location :angle))
                do (write-attribute name value))
          (write-char #\] stream))
        (write-char #\; stream))))

  (:method ((context t) (node standard-object) &rest args)
    (declare (dynamic-extent args))
    (apply #'setf.dot:context-put-node context (setf.dot::context-id context node) args)))


(defgeneric setf.dot:context-put-edge (stream head-id tail-id
                                              &key
                                              type         ; graph type determines link form
                                              arrowhead arrowsize arrowtail
                                              color colorscheme comment constraint decorate dir
                                              edgeURL edgehref edgetarget edgetooltip
                                              fontcolor fontname fontsize
                                              headclip headhref headlabel headport headtarget headtooltip headURL href
                                              id label labelangle labeldistance labelfloat labelfontcolor
                                              labelfontname labelfontsize labelhref labeltarget labeltooltip labelurl
                                              layer len lhead lp ltail minlen nojustify penwidth pos
                                              samehead sametail showboxes style
                                              tailclip tailhref taillabel tailport tailtarget tailtooltip tailURL
                                              target tooltip url weight)
  (:argument-precedence-order tail-id head-id stream)

  (:documentation "Encode a graph edge with HEAD-ID and TAIL-ID to the given STREAM.
 Accept the 'E' attributes per [http://www.graphviz.org/doc/info/attrs.html].
 In addition, accept and sequester a TYPE attribute, which defaults to the dynamic *TYPE*, to determine
 whether to emit a directed edge connector.")  

  (:method ((stream stream) (head t) (tail t) &rest attributes &aux (attributes-p nil))
    (declare (dynamic-extent attributes))
    (flet ((write-attribute (name value)
                      (cond (attributes-p
                             (write-string ", " stream))
                            (t
                             (setf attributes-p t)))
                      (setf.dot:stream-write-attribute stream name value)))
      (destructuring-bind (&key (type setf.dot:*type*) &allow-other-keys)
                          attributes
        (when (and setf.dot:*pretty* (plusp setf.dot:*level*))
          (setf.dot:fresh-line stream))
        ;;(setf.dot:stream-write-node-id stream head) ;; is this necessary?
        (setf.dot:stream-write-id stream head)
        (write-string (if (eq type 'setf.dot:digraph) " -> " " -- ") stream)
        ;;(setf.dot:stream-write-tail-id stream tail)
        (setf.dot:stream-write-id stream tail)
        (write-char #\space stream)
        (when attributes
          (write-char #\[ stream)
          (loop for (name value) on attributes by #'cddr
                unless (member name '(:type))
                do (write-attribute name value))
          (write-char #\] stream))
        (write-char #\; stream))))

  (:method ((context t) (head standard-object) (tail t) &rest args)
    (declare (dynamic-extent args))
    (apply #'setf.dot:context-put-edge context (setf.dot::context-id context head) tail args))

  (:method ((context t) (head t) (tail standard-object) &rest args)
    (declare (dynamic-extent args))
    (apply #'setf.dot:context-put-edge context head (setf.dot::context-id context tail) args)))

;;; (setf.dot:context-put-edge *trace-output* :asdf :qwer)
;;; (setf.dot:with-context (make-instance 'setf.dot:stream :stream *trace-output*) (setf.dot:context-put-edge setf.dot:*context* setf.dot:*context* setf.dot:*context*))


;;;
;;; macro layer

(defmacro setf.dot:graph (name attributes &rest elements)
  `(setf.dot:context-put-graph setf.dot:*context* ,name #'(lambda () ,@elements) :type 'setf.dot:graph ,@attributes))

(defmacro setf.dot:digraph (name attributes &rest elements)
  `(setf.dot:context-put-graph setf.dot:*context* ,name #'(lambda () ,@elements) :type 'setf.dot:digraph ,@attributes))

(defmacro setf.dot:subgraph (name attributes &rest elements)
  `(setf.dot:context-put-subgraph setf.dot:*context* ,name #'(lambda () ,@elements) ,@attributes))

(defmacro setf.dot:node (name &rest attributes)
  `(setf.dot:context-put-node setf.dot:*context* ,name ,@attributes))

(defmacro setf.dot:edge (head tail &rest attributes)
  `(setf.dot:context-put-edge setf.dot:*context* ,head ,tail ,@attributes))

#+digitool
(setf (assq 'setf.dot:subgraph ccl:*fred-special-indent-alist*) 2
      (assq 'setf.dot:digraph ccl:*fred-special-indent-alist*) 2
      (assq 'setf.dot:graph ccl:*fred-special-indent-alist*) 2
      (assq 'setf.dot:node ccl:*fred-special-indent-alist*) 1
      (assq 'setf.dot:edge ccl:*fred-special-indent-alist*) 2)


;;;
;;; dot construction context

(defmethod setf.dot:context-put-graph ((context setf.dot:constructor) name (generator function) &rest attributes)
  (destructuring-bind (&key (type nil type-s) &allow-other-keys) attributes
    (when type-s
      (setf attributes (copy-list attributes))
      (remf attributes :type))
    (let ((setf.dot:*content* nil))
      (funcall generator)
      `(,(ecase type
           ((nil setf.dot:graph) 'setf.dot:graph)
           (setf.dot:digraph 'setf.dot:digraph))
        ,name
        ,attributes
        ,@(reverse setf.dot:*content*)))))

(defmethod setf.dot:context-put-node ((context setf.dot:constructor) name &rest attributes)
  (let ((node `(setf.dot:node ,name ,@attributes)))
    (push node setf.dot:*content*)
    node))

(defmethod setf.dot:context-put-edge ((context setf.dot:constructor) head tail &rest attributes)
  (let ((edge `(setf.dot:edge ,head ,tail ,@attributes)))
    (push edge setf.dot:*content*)
    edge))

;;;
;;; implicit context

(defun setf.dot:put-graph (name content &rest attributes)
  (declare (dynamic-extent attributes))
  (apply #'setf.dot:context-put-graph setf.dot:*context* name content attributes))

(defun setf.dot:put-subgraph (name content &rest attributes)
  (declare (dynamic-extent attributes))
  (apply #'setf.dot:context-put-subgraph setf.dot:*context* name content attributes))

(defmethod setf.dot:put-node (name &rest attributes)
  (declare (dynamic-extent attributes))
  (apply #'setf.dot:context-put-node setf.dot:*context* name attributes))

(defmethod setf.dot:put-edge (head tail &rest attributes)
  (declare (dynamic-extent attributes))
  (apply #'setf.dot:context-put-edge setf.dot:*context* head tail attributes))

(defmethod setf.dot:put-attribute (name value)
  (setf.dot:stream-write-attribute setf.dot:*context* name value))

(defun setf.dot:put-eol ()
  (setf.dot:write-eol setf.dot:*context*))

(defun setf.dot:put-id (id &optional case)
  (setf.dot:stream-write-id setf.dot:*context* id case))

(defun setf.dot:put-string (string)
  (write-string string setf.dot:*context*))

(defun setf.dot:id (datum)
  (setf.dot:context-id setf.dot:*context* datum))

(defun (setf setf.dot:id) (value datum)
  (setf (setf.dot:context-id setf.dot:*context* datum) value))

;;;
;;; external program interface

(flet ((graphviz (&key graph stream (type "jpg") (pretty nil)
                       (graphviz-program "dot")
                       (display-program "safari"))
         "encode a graph as a .dot file, process the file, and display the result."
         (when graph
           (setf.dot:context-put-graph stream graph graph :pretty pretty))
         (let ((dot-pathname (ignore-errors (pathname stream))))
           (when (and dot-pathname graphviz-program)
             (let ((asdf::*verbose-out* nil)
                   (typed-pathname (make-pathname :type type :defaults dot-pathname)))
               (when (and (zerop (asdf:run-shell-command "~a -o ~a -T~a ~a"
                                                         (make-pathname :name graphviz-program
                                                                        :defaults setf.dot:*graphviz-pathname*)
                                                         typed-pathname
                                                         type
                                                         dot-pathname))
                          display-program)
                 (asdf:run-shell-command "open -a ~a ~a" display-program typed-pathname)))))))
  (defun setf.dot:dot (&rest args) (apply #'graphviz :graphviz-program "dot" args))
  (defun setf.dot:neato (&rest args) (apply #'graphviz :graphviz-program "neato" args))
  (defun setf.dot:fdp (&rest args) (apply #'graphviz :graphviz-program "fdp" args))
  (defun setf.dot:circo (&rest args) (apply #'graphviz :graphviz-program "circo" args))
  (defun setf.dot:twopi (&rest args) (apply #'graphviz :graphviz-program "twopi" args)))
      
    

#|
this is the equivalent to the example in 
  http://www.martin-loetzsch.de/S-DOT/

(let ((graph '(setf.dot:graph "test" (:rankdir "lr")
              (setf.dot:node "a" :label "node1" :fontsize "9" :fontname "Arial")
              (setf.dot:node "b" :label "node2" :fontsize "9" :fontname "Arial")
              (setf.dot:node "c" :label "node3" :fontsize "9" :fontname "Arial")
              (setf.dot:node "d" :label "node4" :fontsize "9" :fontname "Arial")
              (setf.dot:edge "a" "b" :fontname "Arial" :fontsize "9" :label "edge1")
              (setf.dot:edge "a" "c" :fontname "Arial" :fontsize "9" :label "edge2")
              (setf.dot:edge "b" "c" :fontname "Arial" :fontsize "9" :label "edge3")
              (setf.dot:edge "b" "d" :fontname "Arial" :fontsize "9" :label "edge4")
              (setf.dot:edge "c" "b" :fontname "Arial" :fontsize "9" :label "edge5"))))
  (equalp  graph
          (let ((setf.dot:*context* (make-instance 'setf.dot:constructor)))
            (eval graph))))
|#


:de.setf.utility.dot

