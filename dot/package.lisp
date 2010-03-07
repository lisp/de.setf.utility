;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  This file is part of the 'de.setf.utility.dot' (or 'setf.dot') library component.
  It defines a stream specialization with a functional interface 
  to generate dot files.
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
  <DELTA DATE='20090216'>refactored with simpler interface and tests</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#


(in-package :common-lisp-user)

(defpackage :de.setf.utility.dot
  (:use )
  (:nicknames :setf.dot :dot)           ; the latter is expendable
  (:documentation
   "<p>The DE.SETF.UTILITY.DOT library component implements interfaces to generate Graphviz '.dot'-encoded graphs.</p>
 <p>'dot' is one component of <a href='http://www.graphviz.org/'>Graphviz</a>, the open source
  graph visualization software from AT&amp;T Research.  The 'dot' program renders directed graphs
  to various encodings - eg, JPG, SVG, and GIF. The 'dot' name also designates the file type for the
  graph description files which that software uses. This library implements operators which
  generate '.dot' encoded graphs through a functional interface and from graph models.</p>
 <p>The Graphviz package includes other layout renderers in addition to 'dot'.
  'neato', 'fdp', 'twopi', and 'circo' each produces an alternative layout from the same '.dot' description files.
  This library includes shell operators to invoke any of the various Grahviz programs to process generated '.dot' files.</p>
 <p>The base library layer is a functional interface for generating the various graph statements (graph/digraph, subgraph, node,
  edge) through context-specialized operators, <code>CONTEXT-PUT-<i>statement</i></code>.
  The respective base method accepts a context, the given statement's required parameters, and keyword arguments
  for the respective attributes.
  The names follow the abstract graph grammar from the '<a href='http://www.graphviz.org/pdf/dotguide.pdf'>dot
  User's Manual</a>'.
  Methods are specialized for
 <ul>
  <li><code>STREAM</code> to emit statements to a stream,</li>
  <li><code>STANDARD-OBJECT</code> and <code>DOT:CONTEXT</code> combinations to unify node ids and to support
   class-specific graph constituent enumeration, and</li>
  <li><code>DOT:CONSTRUCTOR</code> to construct an in-memory graph model.</li>
  </ul>
 Convenience operators are also defined, <code>PUT-<i>statement</i></code> to use the dynamic binding for 
 <code>DOT:*CONTEXT*</code> instead of the required initial argument.</p>
 <p>The second level comprises macros which implement processing for the abstract data model.
  These operators are named <code>DOT:<i>statement</i></code> and exhibit statement forms as dictated by the model structure:
   <ul><li><code>(dot:subgraph <i>name</i> <i>attributes</i> . <i>statements</i>)</code></li>
    <li><code>(dot:node <i>name<i> . <i>attributes<i>)</code></li>
    <li><code>(dot:edge <i>name1<i> <i>name2<i> . <i>attributes<i>)</code></li>
    </ul>
   Attributes are represented as property lists, with keyword indicators, in order that they are directly amenable to use as operator arguments.
   </p>
  <p>Each of the alternative Common-Lisp Graphviz libraries manifests a distinct variation of the possible approaches to the generation process,
   the data model representation, and the api control-flow:
  <table>
   <tr><th>module</th><th>process</th><th>model</th><th>api control-flow</th></tr>
   <tr><td>dot</td>
       <td>single-phase - specialized for construction or generation</td>
       <td>s-expression is program/data dual</td>
       <td>iteration and walking</td></tr>
   <tr><td><a href='http://www.foldr.org/~michaelw/projects/cl-dot/'>CL-DOT</a></td>
       <td>two-phase - construction and generation</li>
       <td>clos-based</td>
       <td>collections</td></tr>
   <tr><td><a href='http://www.martin-loetzsch.de/S-DOT/'><code>S-Dot</code></a></td>
       <td>three-phase - construction, validation, generation
       <td>s-expression is data only</td>
       <td>collections (complete graphs)<td></tr>
   <tr><td>cl-graph/cl-graphviz</td>
       <td>two-phase - construction and generation</td>
       <td>foreign objects manipulated through ff-operators</td>
       <td>iteration and walking</td></tr>
   </table>
  <p>setf.dot elects a list-based model for several reasons.
   <ul><li>It suffices to capture the graph relations with minimum resources.<li>
    <li>It supports program/data duality.</li>
    <li>It offers intrinsic externalization support.</li>
    <li>It avoids the foreign-interface impedence of a foreign model.</li></ul>
   The attributes are represented in the model as property lists rather than association linsts in order that
   they serve directly as arguments to interpretation operators. In this regard it differs from the S-DOT model.
   This is because the attribute sets tend to be fixed for a given domain model class, which means that the
   interface can rely on standard keyword validation rather than explicit membershipd tests.</p>

  <p>setf.dot elects a single-phase process with an iterative control-flow as that corresponds best to the
   application use-case. Where a domain model already exists, the entities and relations are already known.
   As such are immediately available for iteration and/or enumeration can be externalized directly.
   It is not necessary to interleave statement construction,
   It is required only to unify identity and permit forward references among nodes in order that the '.dot' graph be correct.
   If sub-graphs are present, the encoding ordering constraints can require two passes over a domain model
   in order to ensure that nodes appear before edges, but this is not a problem where the domain model already exists. <br />
 
   CL-DOT offers the alternative, to construct the model implicitly, by instantiating nodes and edges in relation to domain data.
   while this does suggest the advantage, that the instantiation order is decoupled from the generation order, it requires a
   more complex intermediate model to record these relations.
   While the construction phase is not strictly necessary, as the generation phase can be performed directly upon an
   arbitrary data model, so long as the constituents support the respective node and edge interface, the api still requires
   a caller to provide collections of all node, edge components. <br />
   
   cl-graphviz offers the even more pronounced alternative, that the api manipulates individual graph elements which correspond
   to statements without any support to unify them woth domain data.</p>

  <p>For those cases where an autonomous model is necessary, the list-based model is available and can be interpreted
 or evaluated directly, depending on whether it contains includes symbolic references or just literals.</p>
  </ul>
 </p>")

  ;; no, one does not want to do this,
  ;; (:import-from :common-lisp :type)
  (:export :*context*
           :*eol*
           :*pretty*
           :*level*
           :*content*
           :*type*
           :*graphviz-pathname*
           :call-with-context
           :with-context
           :eol
           :fresh-line
           :write-name-id
           :cache
           :dot
           :graphviz
           :neato
           :fdp
           :circo
           :twopi
           :context
           :pretty
           :stream-write-attribute
           :stream-write-head-id
           :stream-write-node-id
           :stream-write-tail-id
           :write-eol

           :context-put-edge
           :context-put-graph
           :context-put-subgraph
           :context-put-node
           :context-id

           :put-edge
           :put-eol
           :put-graph
           :put-node
           :put-subgraph
           :put-attribute
           :put-id
           :put-string


           :id
           :map-edge-attributes
           :map-graph-attributes
           :map-graph-edges
           :map-graph-nodes
           :map-graph-subgraphs
           :map-node-attributes
           :stream
           ;; :write not necessary
           :write-attribute-statement
           :write-edge
           :write-edge-attribute-list
           :write-edge-id-in
           :write-edge-id-out
           :write-edge-statement
           :stream-write-escaped-string
           :write-graph
           :write-graph-link
           :write-graph-node
           :stream-write-id
           :write-node
           :write-node-attribute-list
           :encode-node-id
           :write-node-statement
           ;; :write-sequence not necessary
           ;; :write-string not necessary
           :write-subgraph-statement
           :edge
           :graph
           :subgraph
           :digraph
           :directed-p
           :graph-type
           :stream-graph-name
           :stream-graph-type
           :map-attributes
           :node
           :name
           :type
           :statement-type
           :stream-node-annotations
           :stream-reference
           :strict
           :constructor

           :rgb
           :rgb-blue
           :rgb-green
           :rgb-p
           :rgb-red
           :make-rgb
           ))

;;; as these are required by defstruct, make them available explicitly
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(setf.dot:rgb-blue setf.dot:rgb-green setf.dot:rgb-p setf.dot:rgb-red setf.dot:make-rgb)
           :de.setf.utility.implementation))