;;; -*- Package: de.setf.utility.implementation; -*-

;;; test utilities

(in-package :de.setf.utility.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless  (intersection '(:digitool :clozure) *features*)
    (cerror "Continue anyway." "This file must be conditionalized for ~a." (lisp-implementation-type))))

(modpackage :de.setf.utility
  (:export :*function-profiler-rate*
           :function-profiler
           :function-profiler-name
           :function-profiler-count
           :function-profiler-time
           :function-profiler-bytes
           :function-profiler-last-time
           :function-profiler-last-bytes
           :function-profiler-test
           :function-profiler-history
           :function-profiler-rate
           :profiling
           :report-profilers))

(defparameter *function-profiler-rate* 60)

(defstruct function-profiler
  name
  count
  time
  bytes
  last-time
  last-bytes
  test
  (rate *function-profiler-rate*)
  (history (make-array 32 :fill-pointer 0 :adjustable t)))

(defvar *function-profilers* (make-hash-table))


(defun function-profiler (function-name &rest initargs)
  "Given a function-name, adjust and return an existing instance or contruct and cache a new one."
  (assert (fboundp function-name) () "Cannot profile '~s'." function-name)

  (let ((profiler (gethash function-name *function-profilers*)))
    (cond (profiler
           (destructuring-bind (&key (test nil test-s) (rate nil rate-s)) initargs
             ;; emulate reinitialize-instance
             (when test-s (setf (function-profiler-test profiler) test))
             (when rate-s (setf (function-profiler-rate profiler) rate)))
           profiler)
          (t
           (setf (function-profiler function-name)
                 (apply #'make-function-profiler :name function-name initargs))))))


(defgeneric (setf function-profiler) (profiler name)
  (:method ((profiler null) (function-name t))
    (ccl::unadvise-1 function-name :around 'function-profiler)
    nil)

  (:method ((profiler null) (function-name (eql t)))
    (maphash #'(lambda (key profiler) (declare (ignore profiler)) (remhash key *function-profilers*))
             *function-profilers*)
    profiler)

  (:method ((profiler (eql t)) (function-name t))
    (setf (function-profiler function-name) (function-profiler function-name)))

  (:method ((profiler function-profiler) (function-name t))
    (assert (fboundp function-name) () "Cannot profile '~s'." function-name)
    #+mcl
    (let ((handle-name (gensym))
          (function (fdefinition function-name)))
      (ccl::advise-2 (lambda (&rest arglist)
                      (declare (dynamic-extent arglist))
                      (let ((time-before (get-internal-run-time))
                            (bytes-before (ccl::total-bytes-allocated)))
                        (multiple-value-prog1 (apply handle-name arglist)
                          (let ((test (function-profiler-test profiler)))
                            (when (or (null test) (apply test function arglist))
                              (let* ((bytes (- (ccl::total-bytes-allocated) bytes-before))
                                     (time (- (get-internal-run-time) time-before))
                                     (count (function-profiler-count profiler))
                                     (history (function-profiler-history profiler))
                                     (last-count (if (plusp (length history))
                                                   (first (aref history (1- (length history))))
                                                   0)))
                                (decf bytes 32) ; by observation
                                (incf (function-profiler-bytes profiler) bytes)
                                (incf (function-profiler-time profiler) time)
                                (setf (function-profiler-last-bytes profiler) bytes)
                                (setf (function-profiler-last-time profiler) time)
                                (setf (function-profiler-count profiler) (1+ count))
                                (when (>= count (+ last-count (function-profiler-rate profiler)))
                                  (vector-push-extend (list count
                                                            (function-profiler-bytes profiler)
                                                            (function-profiler-time profiler))
                                                      history))))))))
                    handle-name
                    nil
                    function-name
                    :around
                    'function-profiler
                    nil))
    (setf (function-profiler-count profiler) 0
          (function-profiler-time profiler) 0
          (function-profiler-bytes profiler) 0
          (function-profiler-last-time profiler) 0
          (function-profiler-last-bytes profiler) 0
          (fill-pointer (function-profiler-history profiler)) 0)
    (setf (gethash function-name *function-profilers*) profiler)))


(defun report-profilers (names &optional (stream *trace-output*))
  (when (eq names t)
    (setf names (sort (loop for name being the hash-key of *function-profilers* collect name)
                      #'string-lessp)))
  (dolist (name names)
    (let ((m (gethash name *function-profilers*)))
      (if m
        (cond ((plusp (function-profiler-count m))
               (format stream "~&~s: x ~d: ~20t(total ~d ~d) ~40t(avg ~d ~d) ~60t(last ~d ~d)"
                       name 
                       (function-profiler-count m)
                       (function-profiler-bytes m)
                       (float (/ (function-profiler-time m) internal-time-units-per-second))
                       (round (/ (function-profiler-bytes m) (function-profiler-count m)))
                       (float (/ (/ (function-profiler-time m) internal-time-units-per-second) (function-profiler-count m)))
                       (function-profiler-last-bytes m)
                       (float (/ (function-profiler-last-time m) internal-time-units-per-second)))
               (when (plusp (length (function-profiler-history m)))
                 (let ((last-count 0) (last-bytes 0) (last-time 0) (entries 0))
                    (map nil #'(lambda (entry) 
                                 (destructuring-bind (count bytes time)  entry
                                   (let ((delta-count (- count last-count))
                                         (delta-bytes (- bytes last-bytes))
                                         (delta-time (- time last-time)))
                                     (setf last-count count last-bytes bytes last-time time)
                                     (format stream "~:[~;~%    ~](~d ~d) "
                                             (zerop (mod entries 8))
                                             (round (/ delta-bytes delta-count))
                                             (round (/ delta-time delta-count)))
                                     (incf entries))))
                         (function-profiler-history m)))))
              (t
               (format stream "~&~a: x 0" name)))
        (format stream "~&~a: ?" name)))))

(defmacro profiling (names &body body)
  `(call-profiling (function (lambda () ,@body)) ',names :report t))

(defun call-profiling (op names &key report)
  (let ((args ()))
    (loop (if (keywordp (first names))
            (destructuring-bind (key value . rest) names
              (setf (getf args key) value)
              (setf names rest))
            (return)))
    (unwind-protect (progn (dolist (name names) (setf (function-profiler name)
                                                      (apply #'function-profiler name args)))
                           (funcall op))
      (dolist (name names) (setf (function-profiler name) nil))
      (when report (report-profilers names)))))


(defmacro dsu:time-and-memory (form)
  "Execute the from recording and returing time and memory usage in addition to the form's initial value."

  `(flet ((.time-and-memory. () ,form))
     (declare (dynamic-extent #'.time-and-memory.))
     (call-with-time-and-memory #'.time-and-memory.)))

(defun call-with-time-and-memory (operator)
  (let ((time-before (get-internal-run-time))
        (bytes-before (ccl::total-bytes-allocated))
        (value (funcall operator)))
    (let* ((bytes (- (ccl::total-bytes-allocated) bytes-before))
           (time (- (get-internal-run-time) time-before)))
      (decf bytes 24) ; by observation
      (values value
              time bytes))))


;;; (defun test-fm (arg1 arg2) (+ arg1 arg2))
;;; (profiling (test-fm) (dotimes (x 1000) (test-fm 1 2)))