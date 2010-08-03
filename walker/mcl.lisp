;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-
;;;


(in-package :de.setf.utility.implementation)


(:documentation "This file implements mcl ide tools to use the class/function/package grapher."
 
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
  (delta 20031101 "janderson" "combined ide interface to various graphers")))

(defparameter *graph-menu-packages* nil
  "binds a list of known packages. this set of packages is compared with
the result of list-all-packages when updating the graph menu. when
they differ the set of menu entires is rebuilt.")

(defparameter *graph-packages* (list (find-package :common-lisp))
  "binds the list of packages for the symbols which name objects to be
included in in graph output. the initial value is a list which contains the 
common-lisp package. the constituency is modified according to selections
from the entries in the packages submenu of the graphs menu." )

(defClass graph-package-menu (menu) ()
  (:documentation
   "a specialized menu class to support an update method which rebuilds
the menu to reflect the set of all known packages."))


(:documentation
  "add a submen to the Tools menu with entries for graphing classes,
packages and functions, and a final entry for a further submenu which
displays all known packages and lets one specify whether they should be
included in graphs.")

(defparameter *graph-package-menu*
  (make-instance 'graph-package-menu
    :menu-title "Packages"))

(defParameter *graph-menu*
  (make-instance 'menu
    :menu-title "Graph"
    :menu-items (list (make-instance 'window-menu-item
                        :menu-item-title "Classes"
                        :menu-item-action 'ide-graph-classes)
                      (make-instance 'window-menu-item
                        :menu-item-title "Packages"
                        :menu-item-action 'ide-graph-packages)
                      (make-instance 'window-menu-item
                        :menu-item-title "Functions"
                        :menu-item-action 'ide-graph-functions)
                      (make-instance 'menu-item
                        :menu-item-title "-")
                      *graph-package-menu*)))


(defMethod menu-update ((menu graph-package-menu))
  "check of the set of known packages has changed. if so, then rebuild the
packages submenu. continue with the general method in order to update the
individual menu entries."
  (let ((packages (list-all-packages)))
    (when (or (/= (length packages) (length *graph-menu-packages*))
              (/= (length packages) (length (intersection packages *graph-menu-packages*))))
      (setf *graph-menu-packages* (sort (copy-list packages)
                                        #'string-lessp :key #'package-name))
      (apply #'remove-menu-items menu
             (menu-items menu))
      (apply #'add-menu-items menu
             (mapcar #'(lambda (package)
                         (make-instance 'menu-item
                           :menu-item-title (package-name package)
                           :menu-item-action #'(lambda ()
                                                 (if (find package *graph-packages*)
                                                   (setf *graph-packages* (remove package *graph-packages*))
                                                   (push package *graph-packages*)))
                           :update-function #'(lambda (item)
                                                (set-menu-item-check-mark item (if (find package *graph-packages*) t nil)))))
                     *graph-menu-packages*))))
  (call-next-method))

(let* ((menu (find-menu "Tools")))
  (unless (find-menu-item menu "Graph")
    (add-menu-items menu *graph-menu*)))

;(remove-menu-items (find-menu "Tools") (find-menu-item (find-menu "Tools") "Graph"))



(:documentation
  "allow two alternative means to specify the initial designator.
 if the active window exhibits a selected symbol, use that.
 otherwise prompt the user for a symbol.")


(defMethod window-selected-symbol ((window window))
  "read the current selection and require that it be a symbol."
  (multiple-value-bind (start end) (selection-range window)
    (unless (= start end)
      (let ((datum (ignore-errors (ccl::stream-position window start) (read window))))
        (when (and datum (symbolp datum))
          datum)))))



(:documentation
  "provide an abstract mechanism to save a result in a temporary file
and start a program to present the graph. if no application is provided
the os makes the choice. omnigraffle works well with .dot files.
<br />
each of the ide-graph-* functions extracts a symbol and invokes the
respective graphing function to generate a graph file which is then
opened.")

(flet ((call-with-dot-file (function &key class (type "jpg"))
         (let* ((pathname (make-pathname :host "home"
                                         :name (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
                                                 (format nil "~@[~a-~]~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
                                                         class year month day hour min sec))
                                         :type "dot")))
           (prog1 (funcall function pathname)
             (set-mac-file-creator pathname (intern (make-string 4 :initial-element #\null) :keyword))
             (setf.dot:dot :stream pathname :type type))))
       (get-symbol-from-user (&optional (prompt "enter a symbol"))
         "prompt the user for a symbol and read the returned string"
         (read-from-string (get-string-from-user prompt))))
  
  (defMethod ide-graph-classes ((window fred-window))
    (let* ((*package* (window-package window))
           (class-symbol (or (window-selected-symbol window) (get-symbol-from-user "enter class name"))))
      (call-with-dot-file
       #'(lambda (pathname)
           (graph-classes :stream pathname :class class-symbol
                          :packages (cons (symbol-package class-symbol) *graph-packages*)
                          ; :packages (list-all-packages)
                          :level 10
                          ))
       :class 'class)))
  
  (defMethod ide-graph-functions ((window fred-window))
    (let* ((*package* (window-package window))
           (function-symbol (or (window-selected-symbol window) (get-symbol-from-user "enter function name"))))
      (call-with-dot-file
       #'(lambda (pathname)
           (graph-functions (cons (symbol-package function-symbol) *graph-packages*)
                            :stream pathname :function function-symbol
                            ; :packages (list-all-packages)
                            :level 3))
       :class 'function)))
  
  (defMethod ide-graph-packages ((window fred-window))
    (let* ((package-designator (or (window-selected-symbol window)
                                   (get-string-from-user "package-name"
                                                         :initial-string (package-name (window-package window))))))
      (typecase package-designator
        (string (setf package-designator (find-package package-designator)))
        (symbol (setf package-designator (symbol-package package-designator))))
      (call-with-dot-file
       #'(lambda (pathname)
           (graph-packages :stream pathname
                           :root package-designator
                           :packages (cons package-designator *graph-packages*)))
       :class 'package)))
  )


    

:de.setf.utility.walker
