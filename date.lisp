;;; -*- Package: de.setf.utility.implementation; -*-

;;;  This file is part of the 'de.setf.utility' Common Lisp library.
;;;  It implements universal time conversion functions.

;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
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

;;; contents : universal time conversion
;;;
;;; Implements universal-time data conversion according to java2se date and time patterns
;;; see http://java.sun.com/j2se/1.4.2/docs/api/java/text/SimpleDateFormat.html
;;;
;;; date::define-date-conversion-function
;;; date:decode
;;; date:encode

;;; Copyright 2003 [james anderson](mailto:james.anderson@setf.de)
;;; Copyright 2004 Ravenpack International
;;; 20040222.jaa decode-iso-time
;;; Copyright 2009 [james anderson](mailto:james.anderson@setf.de)
;;; 20090304.jamderson  repackaged
;;; 20091025.janderson  added iso as shorthand op
;;; 20091220.janderson  corrected treatment of single quote; made 'Z' decoding optional
;;; 20100210.janderson  cleaned up ignored variables, types


(in-package :de.setf.utility.implementation)

#-( or sbcl allegro lispworks ccl)
(cerror "Continue anyway" "Conditionalization required for funcallable-standar-class")

(defPackage :de.setf.date
  (:nicknames :date)
  (:use)
  (:documentation
   "This is the home package for data format patterns and for data conversion
 operators")
  (:export
   :day-and-month-to-day-in-year
   :day-in-month
   :day-in-month-name
   :day-in-quarter
   :day-in-week
   :day-in-week-name
   :day-in-year
   :day-in-year-to-day-and-month
   :day-name
   :decode
   :decode-am-pm
   :decode-day-name
   :decode-month-day-name
   :decode-month-name
   :encode
   :format-iso-time
   :format-excel-time
   :iso
   :leap-p
   :month-days
   :month-in-year
   :month-name
   :month-quarter
   :quarter-in-year
   :year
   :year-in-century
   :|yyyyMMdd|
   :|yyyyMMddTHH:mm:ss|
   :|ddMMyy|
   :|dddddddd MMM yyyy|
   :|yyyyMMddTHHmmss|
   :|yyMMdd.HHmm|
   :|EEE, dd.MM.yyyy|
   :|DDyy|
   :|ddMMyyyy|
   :|yyyyMMddTHHmmssZZ|
   :|yyyy-MM-ddTHH:mm:ss|
   :|ddMMyy.HHmm|
   :|yyyy-MM-ddTHH:mm:ssZZ|
   :|yyyy.MM.dd HH:mm:ss|
   ))

(modPackage :de.setf.utility
  (:export
   :*current-year*
   :+ordinal-month-days+
   :+ordinal-month-quarter+
   :+seconds-in-week+
   :+seconds-in-day+
   :*day-names*
   :*month-names*
   :universal-time
   :decode-iso-time
   :iso-time
   :date-conversion-function)
  (:export-from :de.setf.date))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'de.setf.utility::format-iso-time :cl-user))


(deftype universal-time () 'integer)

(defparameter *current-year* 0)

(defvarconstant +ordinal-month-days+ #(0 31 28 31 30 31 30 31 31 30 31 30 31)
  "a constant array of the (1-based) days in each (1-based) month.")

(defvarconstant +ordinal-month-quarter+ #(0 1 1 1 2 2 2 3 3 3 4 4 4)
  "a constant array of the respective quarter for each (1-based) month.")

(defvar *day-names* #(nil
                      "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "a static array of the (1-based) day names.")

(defvar *month-day-names*
  (let ((names (make-array 32 :initial-element nil)))
    (dotimes (i 31) (setf (aref names (1+ i)) (format nil "~:(~:R~)" (1+ i))))
    names))

(defvar *month-names* #(nil
                        "january" "february" "march" "april" "may" "june"
                        "july" "august" "september" "october" "november" "december")
  "a static array of the (1-based) month names.")

(defvarconstant +day-of-19000101+ 2
  "1 january 1900 was a monday")
(defvarconstant +seconds-in-day+ (* 60 60 24))
(defvarconstant +seconds-in-week+ (* 60 60 24 7))

(defparameter *date-package* (find-package :date))


(defun date:year
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h d m))
    y))

(defun date:year-in-century
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h d m))
    (mod y 100)))

(defun date:month-in-year
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h d y))
    m))

(defun date:quarter-in-year
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h d y))
    (svref +ordinal-month-quarter+ m)))

(defun date:day-in-week-name (day &optional length &aux name)
  "returns the number name of a (1-based) day."
  (assert (< 0 day 8))
  (setf name (svref *day-names* day))
  (if (and length (< length (length name)))
    (subseq name 0 length)
    name))

(defun date:day-in-month-name (day &optional length &aux name)
  "returns the number name of a (31-based) day."
  (assert (< 0 day 32))
  (setf name (format nil "~:R" day))
  (if (and length (< length (length name)))
    (subseq name 0 length)
    name))

(defun date:month-name (month &optional length &aux name)
  "returns the name of a (1-based) month."
  (assert (< 0 month 13))
  (setf name (svref *month-names* month))
  (if (and length (< length (length name)))
    (subseq name 0 length)
    name))

(defun date:leap-p (&optional (year (date:year-in-century)))
  (and (zerop (mod year 4)) (not (zerop (mod year 100)))))

(defun date:month-days (month)
  "returns the number of days in a (1-based) month."
  (assert (< 0 month 13))
  (svref +ordinal-month-days+ month))

(defun date:month-quarter (month)
  "returns the respective (zero-based) quarter of a (1-based) month."
  (assert (< 0 month 13))
  (svref +ordinal-month-quarter+ month))

(defun date:day-in-month
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d mon y) (decode-universal-time date)
    (declare (ignore s min h mon y))
    d))

(defun date:day-in-year
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h))
    (+ d (reduce #'+ +ordinal-month-days+ :end m)
       (if (and (date:leap-p y) (> m 2)) 1 0))))

(defun date:day-in-quarter
       (&optional (date (get-universal-time)))
  (multiple-value-bind (s min h d m y) (decode-universal-time date)
    (declare (ignore s min h))
    (+ d (reduce #'+ +ordinal-month-days+ :start (svref #(0 1 1 1 4 4 4 7 7 7 10 10 10) m)  :end m)
       (if (and (date:leap-p y) (= m 3)) 1 0))))

(defun date:day-in-week
       (&optional (date (get-universal-time)))
  (1+ (mod (1- (+ +day-of-19000101+ (floor date +seconds-in-day+))) 7)))
       
(defun date:decode-month-name (month &key (start 0) (end (length month)))
  (flet ((test-name (name)
           (and (> (length month) 2)
                (string-equal name month :end1 (min (length name) end) :start2 start :end2 end))))
    (declare (dynamic-extent #'test-name))
    (position-if #'test-name *month-names* :start 1)))

(defun date:decode-month-day-name (day &key (start 0) (end (length day)))
  (flet ((test-name (name)
           (string-equal name day :end1 (min (length name) end) :start2 start :end2 end)))
    (declare (dynamic-extent #'test-name))
    (position-if #'test-name *month-day-names* :start 1)))


(defun date:decode-day-name (day &key (start 0) (end (length day)))
  (flet ((test-name (name)
           (and (> (length day) 2)
                (string-equal name day :end1 (min (length name) end) :start2 start :end2 end))))
    (declare (dynamic-extent #'test-name))
    (position-if #'test-name *day-names* :start 1)))

(defun date:decode-am-pm (string &key (start 0))
  (let ((end (+ start 2)))
    (and (<= end (length string))
         (cond ((string-equal "am" string :start1 start :end1 end) :am)
               ((string-equal "pm" string :start1 start :end1 end) :pm)))))


(defun date:day-in-year-to-day-and-month (day &optional (leap-p nil))
  (let ((month 1) (month-days 0))
    (when (numberp leap-p) (setf leap-p (date:leap-p leap-p)))
    (assert (<= 0 day (if leap-p 366 365)))
    (loop (setf month-days (aref +ordinal-month-days+ month))
          (when (and leap-p (= month 2)) (incf month-days))
          (when (or (>= month 12) (<= day month-days))
            (return (values day month)))
          (decf day month-days)
          (incf month))))

(defun date:day-and-month-to-day-in-year (dim miy &optional (leap-p nil))
  (assert (<= 1 miy 12))
  (assert (<= 1 dim (aref +ordinal-month-days+ miy)))
  (when (numberp leap-p) (setf leap-p (date:leap-p leap-p)))
  (+ dim (reduce #'+ +ordinal-month-days+ :end miy) (if leap-p 1 0)))


(eval-when (:execute :compile-toplevel :load-toplevel)
  
  (defun date-pattern-components (pattern-string)
    "Translate the pattern string into a list of component specifications.
 These serve in the respective encode/decode operator generation to indicate the
 proper element, variable, and parameters. Each component has one of the form:
  ((time-component variable) . encoded-length)
  string-literal
  character-literal
 "
    (let ((position 0)
          (length (length pattern-string))
          (components nil)
          (letter #\null))
      (flet ((not-supported-error (letter)
               (error "date aspect not supported: ~a: ~s" letter pattern-string)))
        (loop (when (>= position length) (return (reverse components)))
              (setf letter (char pattern-string position))
              (if (eql letter #\')
                (let ((component-end (or (position letter pattern-string :start (1+ position))
                                         (error "unbalanced quote: ~s" pattern-string))))
                  (if (= component-end (1+ position))
                    (push #\' components)
                    (push (subseq pattern-string (1+ position) component-end) components))
                  (setf position (1+ component-end)))
                (let* ((component-end (or (position letter pattern-string :start position :test-not #'char=)
                                          length))
                       (component-length (- component-end position)))
                  (push (case letter
                          (#\g (not-supported-error letter))
                          (#\y (if (= component-length 2)
                                 `((.year-in-century. year) . ,component-length)
                                 `((.year. year) . ,component-length)))
                          (#\M (if (> component-length 2)
                                 `((.month-name. month-in-year ,component-length) . ,component-length)
                                 `((.month-in-year. month-in-year) . ,component-length)))
                          (#\w (not-supported-error letter))
                          (#\W (not-supported-error letter))
                          (#\D `((.day-in-year. day-in-month month-in-year) . ,component-length))
                          (#\d (if (> component-length 2)
                                 `((.day-in-month-name. day-in-month ,component-length) . ,component-length)
                                 `((.day-in-month. day-in-month) . ,component-length)))
                          (#\E `((.day-in-week-name. day-in-week ,component-length) . ,component-length))
                          (#\F `((.day-in-week. day-in-week) . ,component-length))
                          (#\a `((.am-pm. hour-in-day) . ,component-length))
                          (#\H `((.hour-24-0-based. hour-in-day) . ,component-length))
                          (#\k `((.hour-24-1-based. hour-in-day) . ,component-length))
                          (#\K `((.hour-12-0-based. hour-in-day) . ,component-length))
                          (#\h `((.hour-12-1-based. hour-in-day) . ,component-length))
                          (#\m `((.minute-in-hour. minute-in-hour) . ,component-length))
                          (#\s `((.second-in-minute. second-in-minute) . ,component-length))
                          (#\S (not-supported-error letter))
                          ((#\z #\Z) `((.time-zone. time-zone) . ,(ecase component-length
                                                                    ((1 4 5) 4)
                                                                    ((2 3) 2))))
                          (t   letter))
                    components)
                  (incf position component-length)))))))
  

  (defGeneric compute-date-encoder (pattern)
    (:documentation "Generate a date encoding function given a PATTERN string or specification.
 PATTERN : (or STRING LIST) : the pattern

  Given a string, delegate the parsing to date-pattern-components and continue. Given a
 specification list, use the time components to construct a format string, and to
 generate conversions from the decoded time constituents.")

    (:method ((pattern string))
      (compute-date-encoder (date-pattern-components pattern)))

    (:method ((components list) &aux (references nil) (format-string nil) (time-zone-p nil)
              (to-ignore '(second-in-minute minute-in-hour hour-in-day day-in-month month-in-year year
                           day-in-week daylight-savings-time-p time-zone)))
      (setf format-string
            (with-output-to-string (stream)
              (dolist (component components)
                (etypecase component
                  (cons
                   (destructuring-bind (component-operation . length) component
                     (destructuring-bind (time-component variable &optional arg) component-operation
                       (case time-component
                         ((.year-in-century. .week-in-year. .day-in-year. .day-in-week.
                                             .hour-24-0-based. .hour-24-1-based. .hour-12-0-based. .hour-12-1-based.)
                          (push component-operation references)
                          (format stream "~~~d,'0d" length))
                         ((.day-in-month-name. .day-in-week-name.)
                          (push component-operation references)
                          (format stream "~~~da" length))
                         (.month-name.
                          (push component-operation references)
                          (format stream "~~~da" length))
                         (.month-in-year.
                          (push component-operation references)
                          (if (> length 2)
                            (format stream "~~~da" length)
                            (format stream "~~~d,'0d" length)))
                         (.am-pm.
                          (push component-operation references)
                          (format stream "~~~da" length))
                         (.time-zone.
                          (push component-operation references)
                          (write-string (ecase length (2 "Z~2,'0d") (4 "Z~2,'0d00")) stream)
                          (setf time-zone-p t))
                         (t
                          (push variable references)
                          (format stream "~~~d,'0d" length)))
                       (setf to-ignore (remove variable to-ignore))
                       (when (symbolp arg)
                         (setf to-ignore (remove arg to-ignore))))))
                  (string
                   (write-string component stream))
                  (character
                   (if (char= component #\~)
                     (write-string "~~" stream)
                     (write-char component stream)))))))
      `(lambda (time &optional stream)
         (macrolet ((.year-in-century. (x) `(mod ,x 100))
                    (.year. (x) x)
                    (.month-name. (x l) `(date:month-name ,x ,l))
                    (.month-in-year. (x) x)
                    (.day-in-month-name. (x l) `(date:day-in-month-name ,x ,l))
                    (.day-in-month. (x) x)
                    (.day-in-week-name. (x l) `(date:day-in-week-name ,x ,l))
                    (.day-in-week. (x) x)
                    (.day-in-year. (dim miy) `(date:day-and-month-to-day-in-year ,dim ,miy year))
                    (.am-pm. (x) `(if (>= ,x 12) "pm" "am"))
                    (.hour-24-0-based. (x) x)
                    (.hour-24-1-based. (x) `(1+ ,x))
                    (.hour-12-0-based. (x) `(mod ,x 12))
                    (.hour-12-1-based. (x) `(1+ (mod ,x 12)))
                    (.minute-in-hour. (x) x)
                    (.second-in-minute. (x) x)
                    (.time-zone. (x) x))
           (multiple-value-bind (second-in-minute minute-in-hour hour-in-day day-in-month
                                                  month-in-year year day-in-week daylight-savings-time-p time-zone)
                                (decode-universal-time time ,@(when time-zone-p '(0)))
             ,@(when to-ignore `((declare (ignore ,@to-ignore))))
             (format stream ,format-string ,@(reverse references)))))))

  
  (defGeneric compute-date-decoder (pattern)
    (:documentation "Generate a date decoding function given a PATTERN string or specification.
 PATTERN : (or STRING LIST) : the pattern
  Given a string, delegate the parsing to date-pattern-components and continue. Given a
 specification list, use the time components to assemble the parsing steps, cache the intermediate values, and
 combine them as decoded time constituents.")

    (:method ((pattern string))
      (compute-date-decoder (date-pattern-components pattern)))
    (:method ((components list) &aux (position 0) 
              (time-zone-p (find-if #'(lambda (component)
                                        (and (consp component) (consp (car component))
                                             (or (eq (caar component) '.time-zone.))))
                                    components))
              (am-pm-p (find-if #'(lambda (c) (and (consp c) (consp (first c))
                                                         (eq (first (first c)) '.am-pm.)))
                                      components))
              (day-in-year-p (find-if #'(lambda (c) (and (consp c) (consp (first c))
                                                         (eq (first (first c)) '.day-in-year.)))
                                      components)))

             `(lambda (string)
                (let ((second-in-minute 0)
                      (minute-in-hour 0)
                      (hour-in-day 0)
                      (day-in-month 1)          ; initial values just in case the pattern
                      (month-in-year 1)         ; includes none. ?
                      ,@(when day-in-year-p '((day-in-year 0)))
                      (year 0)
                      ,@(when am-pm-p '((am-pm :am)))
                      ,@(when time-zone-p `((time-zone 0))))
                  ,@(mapcar #'(lambda (component)
                                (etypecase component
                                  (cons
                                   (destructuring-bind ((time-component variable &optional arg) . length) component
                                     (declare (ignore  arg))
                                     (prog1
                                       (case time-component
                                         (.year-in-century.
                                          `(setf year
                                                 (+ 1900 (parse-integer string :start ,position :end ,(+ position length)))))
                                         ((.day-in-week. .day-in-week-name.)
                                          ;; do nothing, the text is for information only
                                          )
                                         (.day-in-month-name.
                                          `(date:decode-month-day-name string :start ,position
                                                                       :end ,(+ position length)))
                                         (.week-in-year.
                                          `(setf month-in-year
                                                 (week-in-year-to-month
                                                  (parse-integer string :start ,position
                                                                 :end ,(+ position length)))))
                                         (.day-in-year.
                                          `(setf day-in-year
                                                 (parse-integer string :start ,position
                                                                :end ,(+ position length))))
                                         ((.hour-24-0-based. .hour-12-0-based.)       ; 12/24 distinction is handled below
                                          `(setf hour-in-day
                                                 (parse-integer string :start ,position
                                                                :end ,(+ position length))))
                                         ((.hour-24-1-based. .hour-12-1-based.)
                                          `(setf hour-in-day
                                                 (1- (parse-integer string :start ,position
                                                                    :end ,(+ position length)))))
                                         (.month-in-year.
                                          `(setf month-in-year
                                                 (parse-integer string :start ,position
                                                                :end ,(+ position length))))
                                         (.month-name.
                                          `(date:decode-month-name string  :start ,position
                                                                   :end ,(+ position length)))
                                         (.am-pm.
                                          `(setf am-pm (date:decode-am-pm string  :start ,position
                                                                          :end ,(+ position length))))
                                         (.time-zone.
                                          ;; (incf position) ;; don't always increment, allow optional 'Z'
                                          `(let ((position (if (digit-char-p (char string ,position))
                                                             ,position (1+ ,position))))
                                             (setf time-zone (parse-integer string  :start position
                                                                            :end (+ position 2)))))
                                         (t     ; simple components
                                          `(setf ,variable
                                                 (parse-integer string :start ,position
                                                                :end ,(+ position length)))))
                                       (incf position length))))
                                  (string
                                   (prog1 `(assert (eql (string-equal string ,component :start1 ,position :end1 ,(+ position (length component)))))
                                     (incf position (length component))))
                                  (character
                                   (prog1 `(assert (eql (char string ,position) ,component))
                                     (incf position)))))
                            components)
                  ,@(when am-pm-p
                      `((case am-pm (:am) (:pm (incf hour-in-day 12)))))
                  ,@(when day-in-year-p
                      `((multiple-value-setq (day-in-month month-in-year)
                          (date:day-in-year-to-day-and-month day-in-year year))))
                  (encode-universal-time second-in-minute minute-in-hour hour-in-day
                                         day-in-month month-in-year year
                                         ,@(when time-zone-p '(time-zone)))))))
  
  (defClass date-conversion-function (standard-generic-function)
    ()
    (:metaclass #+:sbcl sb-mop:funcallable-standard-class
                #+:allegro mop:funcallable-standard-class
                #+lispworks hcl:funcallable-standard-class
                #+:ccl ccl:funcallable-standard-class)
    (:documentation "A function class to distinguish conversion function
      for find-date-format."))
  
  (defMacro date::define-date-conversion-function (pattern)
    (let ((name (intern pattern :date)))
      `(progn
         (eval-when (:execute :compile-toplevel :load-toplevel) (export ',name :date))
         (defGeneric ,name (datum &optional arg)
           (:generic-function-class date-conversion-function)
           (:method ((datum integer) &optional arg)
             (,(compute-date-encoder pattern) datum arg))
           (:method ((datum string) &optional arg) (declare (ignore arg))
                    (,(compute-date-decoder pattern) datum))))))
  ) ; eval-when



(date::define-date-conversion-function "DDyy")
(date::define-date-conversion-function "ddMMyy")
(date::define-date-conversion-function "ddMMyyyy")
(date::define-date-conversion-function "yyyyMMdd")
(date::define-date-conversion-function "yyMMdd.HHmm")
(date::define-date-conversion-function "ddMMyy.HHmm")
(date::define-date-conversion-function "yyyyMMddTHHmmss")
(date::define-date-conversion-function "yyyyMMddTHHmmssZZ")
(date::define-date-conversion-function "yyyyMMddTHH:mm:ss")
(date::define-date-conversion-function "yyyy-MM-ddTHH:mm:ss")
(date::define-date-conversion-function "yyyy-MM-ddTHH:mm:ssZZ")
(date::define-date-conversion-function "EEE, dd.MM.yyyy")
(date::define-date-conversion-function "dddddddd MMM yyyy")
(date::define-date-conversion-function "yyyy.MM.dd HH:mm:ss")


(defGeneric date::find-date-format (name &key if-does-not-exist)
  (:method ((name string) &key (if-does-not-exist :error))
           (let ((symbol (find-symbol name *date-package*)))
             (if symbol
               (date::find-date-format symbol)
               (ecase if-does-not-exist
                 (:error (error "date format not found: ~s." name))
                 (:create (eval `(date::define-date-conversion-function ,name)))
                 ((nil) nil)))))
  (:method ((name symbol) &key (if-does-not-exist :error))
           (let ((function nil))
             (if (and (fboundp name)
                      (typep (setf function (symbol-function name)) 'date-conversion-function))
               function
               (ecase if-does-not-exist
                 (:error (error "date format not found: ~s." name))
                 (:create (eval `(date::define-date-conversion-function ,name)))
                 ((nil) nil))))))

(defGeneric date:encode (format &optional universal-time stream)
  (:method ((format (eql 'date:iso)) &optional (time (get-universal-time)) stream)
           (date:|yyyyMMddTHHmmss| time stream))
  (:method ((format symbol) &optional (time (get-universal-time)) stream)
           (funcall (date::find-date-format format :if-does-not-exist :error)
                    time stream))
  (:method ((format string) &optional (time (get-universal-time)) stream)
           (funcall (date::find-date-format format :if-does-not-exist :error)
                    time stream)))

(defGeneric date:decode (format string)
  (:method ((format (eql 'date:iso)) (time string))
           (date:|yyyyMMddTHHmmss| time))
  (:method ((format symbol) (time string))
           (funcall (date::find-date-format format :if-does-not-exist :error)
                    time))
  (:method ((format string) (time string))
           (funcall (date::find-date-format format :if-does-not-exist :error)
                    time)))

                                  ; one version only
;;(defun format-iso-time (&optional (time (get-universal-time)) stream)
;;  (date:|yyyyMMddTHHmmssZZ| time stream))

(defgeneric date::format-iso-time (time stream &optional colon at var)
  (:method ((stream stream) (time integer) &optional colon at var)
    (declare (ignore colon at var))
    (date:|yyyyMMddTHHmmssZZ| time stream)))

(defgeneric date::format-excel-time (time stream &optional colon at var)
  (:method ((stream stream) (time integer) &optional colon at var)
    (declare (ignore colon at var))
    (date::|yyyy.MM.dd HH:mm:ss| time stream)))

;;; (let ((time (get-universal-time))) (format nil ">>~/date:format-excel-time/<<" time))
;;; (let ((time (get-universal-time))) (format nil ">>~/date:format-iso-time/<<" time))


(defun decode-iso-time (string)
  (if (> (length string ) 15)
    (date:|yyyyMMddTHHmmssZZ| string)
    (date:|yyyyMMddTHHmmss| string)))

(defun iso-time (&optional (time (get-universal-time))) (date:|yyyyMMddTHHmmss| time))


;(let ((time (get-universal-time))) (= (decode-iso-time (iso-time time)) time))

(setq *current-year* (date:year-in-century (get-universal-time)))


(let ((*test-unit-situation* :define))
  (test date-pattern-components/1 (date-pattern-components "yyyyMMddTHHmmssZZ")
        '(((.YEAR. YEAR) . 4) ((.MONTH-IN-YEAR. MONTH-IN-YEAR) . 2) ((.DAY-IN-MONTH. DAY-IN-MONTH) . 2) #\T
          ((.HOUR-24-0-BASED. HOUR-IN-DAY) . 2) ((.MINUTE-IN-HOUR. MINUTE-IN-HOUR) . 2) ((.SECOND-IN-MINUTE. SECOND-IN-MINUTE) . 2)
          ((.TIME-ZONE. TIME-ZONE) . 2)))
  (test date-pattern-components/2 (date-pattern-components "yyyyMMdd'T'HHmmssZZ")
        '(((.YEAR. YEAR) . 4) ((.MONTH-IN-YEAR. MONTH-IN-YEAR) . 2) ((.DAY-IN-MONTH. DAY-IN-MONTH) . 2) "T"
          ((.HOUR-24-0-BASED. HOUR-IN-DAY) . 2) ((.MINUTE-IN-HOUR. MINUTE-IN-HOUR) . 2) ((.SECOND-IN-MINUTE. SECOND-IN-MINUTE) . 2)
          ((.TIME-ZONE. TIME-ZONE) . 2)))
  (test date/1 (date:|ddMMyy| "010101") (encode-universal-time 0 0 0 01 01 1901))
  (test date/2 (date:|ddMMyy| (encode-universal-time 0 0 0 01 01 1901)) "010101")
  (test date/3 (date:|yyyyMMdd| "19010101") (encode-universal-time 0 0 0 01 01 1901))

  (test date/4 (date:|ddMMyy.HHmm| (encode-universal-time 01 02 03 04 05 1906)) "040506.0302")
  (test date/5 (date:|ddMMyy.HHmm| (encode-universal-time 01 02 03 04 05 1906) nil) "040506.0302")
  (test date/7en (date:|yyyyMMddTHHmmss| (encode-universal-time 01 02 03 04 05 1906)) "19060504T030201")
  (test date/7de (date:|yyyyMMddTHHmmss| "19060504T030201") (encode-universal-time 01 02 03 04 05 1906))
  (test date/7err (type-of (nth-value 1 (ignore-errors (date:|yyyyMMddTHHmmss|  "19060504X030201")))) 'simple-error)
  (test date/8 (date:|EEE, dd.MM.yyyy| (encode-universal-time 01 02 03 07 05 1955)) "Thu, 07.05.1955")
  (test date/9 (date:|dddddddd MMM yyyy| (encode-universal-time 01 02 03 08 08 1955)) "eighth   aug 1955"))




:de.setf.utility
