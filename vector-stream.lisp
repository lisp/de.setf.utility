;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-

#|
<DOCUMENTATION>
 <DESCRIPTION>
  simple vector streams for use with cl-xml decoding
  </DESCRIPTION>
 <COPYRIGHT YEAR='2001' AUTHOR='james adam anderson' MARK='(C)'
            href='file://xml/sysdcl.lisp' />
 <CHRONOLOGY>
  <DELTA DATE='20010605' AUTHOR='MS'>lispworks conformance</DELTA>
  <DELTA DATE='20010702'>moved from xparser to xutils to support data url
   </DELTA>
  <DELTA DATE='20020118'>some CormanLisp</DELTA>
  </CHRONOLOGY>
 </DOCUMENTATION>
|#
(in-package :de.setf.utility.implementation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "this is ibsolete. see codecs/vector-streamlisp"))

;;;
;;; abstract

(defClass vector-stream ()
  ((position :initform 0)
   (vector :initarg :vector :reader vector-stream.vector)
   #+(or CMU sbcl lispworks) (direction :initarg :direction)
   )
  #+CormanLisp
  (:default-initargs :element-type 'character))

(defMethod stream-state ((stream vector-stream))
  (with-slots (position) stream
    (format nil "@~s" position)))

#+cmu
(let ((old-definition (fdefinition 'stream-element-type)))
  (unless (typep old-definition 'generic-function)
    (fmakunbound 'stream-element-type)
    (defgeneric stream-element-type (stream))
    (setf (documentation 'stream-element-type 'function)
          (documentation old-definition 'function))
    (defmethod stream-element-type (stream)
      (funcall old-definition stream))))

(defMethod stream-element-type ((stream vector-stream))
  '(unsigned-byte 8))

(defMethod stream-position
           ((stream vector-stream) &optional new)
  (with-slots (position) stream
    (if new
      (setf position new)
      position)))

(defMethod stream-eofp
           ((stream vector-stream))
  (with-slots (position vector) stream
    (>= position (length vector))))

(defMethod print-object
           ((vs vector-stream) (stream t)
            &aux (*print-array* t) (*print-length* 32) (*print-base* 16))
  ;;nb. length and base likely render the vector unreadable
  (print-unreadable-object (vs stream :type t)
    (princ (vector-stream.vector vs) stream)))


;;;
;;; input

(defClass vector-input-stream (vector-stream
                               #+ALLEGRO excl::fundamental-binary-input-stream
                               #+LispWorks stream:fundamental-stream
                               #+(and MCL digitool) ccl::input-binary-stream
                               #+(and MCL openmcl) fundamental-binary-input-stream
                               #+CMU extensions:fundamental-binary-input-stream
                               #+sbcl sb-gray:fundamental-binary-input-stream
                               #+CormanLisp stream
                               )
  ()
  (:default-initargs :direction :input))

(defMethod initialize-instance :after
           ((instance vector-input-stream) &key)
  (with-slots (vector) instance
    (etypecase vector
      (string (setf vector (map 'vector #'char-code vector)))
      (list (setf vector (map 'vector #'(lambda (datum)
                                          (etypecase datum
                                            (fixnum datum)
                                            (character (char-code datum))))
                              vector)))
      (vector t))))

(defMethod stream-tyi ((stream vector-input-stream))
  (with-slots (position vector) stream
    (when (< position (length vector))
      (prog1 (svref vector position)
        (incf position)))))

(defMethod stream-untyi ((stream vector-input-stream) (datum integer))
  (with-slots (position vector) stream
    (cond ((< position 0)
           (decf position)
           (setf (svref vector position) datum))
          (t
           (error 'end-of-file :stream stream)))))

(defMethod stream-reader ((stream vector-input-stream))
  (with-slots (vector position) stream
    (if (typep vector 'simple-vector)
      #'(lambda (ignore) (declare (ignore ignore))
         (when (< position (length vector))
           (prog1 (svref vector position)
             (incf position))))
      #'(lambda (ignore) (declare (ignore ignore))
         (when (< position (length vector))
           (prog1 (aref vector position)
             (incf position)))))))


;;;
;;; output

defMethod stream-tyo ((stream vector-output-stream) (datum integer) &aux next)
  (with-slots (position vector) stream
    (unless (< (setf next (1+ position)) (length vector))
      (adjust-array vector (+ next (floor (/ next 4)))
                    :element-type '(unsigned-byte 8)))
    (setf (aref vector position) datum)
    (setf position next)))

(defmethod stream-write-byte ((stream vector-output-stream) (datum integer))
  (stream-tyo stream datum))

(defMethod stream-writer ((stream vector-output-stream))
  (with-slots (vector position) stream
    (if (typep vector 'simple-vector)
      #'(lambda (next datum)
          (unless (< (setf next (1+ position)) (length vector))
            (setf vector (adjust-array vector (+ next (floor (/ next 4)))
                                       :element-type '(unsigned-byte 8))))
          (setf (svref vector position) datum)
          (setf position next))
      #'(lambda (next datum)
          (unless (< (setf next (1+ position)) (length vector))
            (setf vector (adjust-array vector (+ next (floor (/ next 4)))
                                       :element-type '(unsigned-byte 8))))
          (setf (aref vector position) datum)
          (setf position next)))))


(defmethod stream-write-sequence ((stream vector-output-stream) (sequence vector)
                                  &optional (start 0) (end nil))
  (unless end (setf end (length sequence)))
  (assert (typep start '(integer 0)))
  (assert (>= end start))
  (with-slots (vector position) stream
    (let* ((new-position (+ position (- end start))))
      (when (> new-position position)
        (unless (< new-position (length vector))
          (adjust-array vector (floor (+ new-position (floor (/ new-position 4))))
                        :element-type '(unsigned-byte 8)))
        (replace vector sequence
                 :start1 position :end1 new-position
                 :start2 start :end2 end)
        (setf position new-position))
      new-position)))

#+(or)
(progn
  (stream-tyo (make-instance 'vector-output-stream) 1)
  (let* ((data #(0 1 2 3 4 5 6 7 8 9 246 247 248 249 250 251 252 253 254 255))
         (buffer (make-array 2 :element-type '(unsigned-byte 8) :adjustable t))
         (outstream (make-instance 'vector-output-stream :vector buffer))
         (instream (make-instance 'vector-input-stream :vector buffer)))
    (write-sequence data outstream)
    (map nil #'(lambda (c) (stream-write-byte outstream (char-code c))) "asdf")

    (and (every #'eql (concatenate 'vector data (map 'vector #'char-code "asdf")) buffer)
         (let ((data2 (make-array (length data)))
               (data3 (make-array 4)))
           (and (eql (stream-read-sequence instream data2) (length data2))
                (equalp data2 data))
           (eql (stream-read-sequence instream data3) 4)
           (equal (map 'string #'code-char data3) "asdf"))))
  )

#|
(inspect
(mapcar #'(lambda (vector)
            (multiple-value-list 
             (decoding-stream-reader (make-instance 'vector-input-stream :vector vector) nil)))
        '(#(#x00 #x00 #x00 #x3c)
          #(#x3c #x00 #x00 #x00)
          #(#x00 #x00 #x3c #x00)
          #(#x00 #x3c #x00 #x00)
          #(#xff #xfe #x00 #x3c)
          #(#xfe #xff #x3c #x00)
          #(#x00 #x3c #x00 #x3f)
          #(#x3c #x00 #x3f #x00)
          #(#x3c #x3f #x78 #x60)
          #(#x12 #x12 #x3c #x3f))))
;("UCS-4-1234" "UCS-4-4321" "UTF-4-2143" "UCS-4-3412" "UTF-16-21" "UTF-16-12" "UTF-16-21" "UTF-16-12" "UTF-8" "UTF-8")

(defparameter *s* (encoded-stream "<?xml ?><x>asdfgh</x>"))
(inspect *s*)
(peek-char nil *s*)
(loop (unless (princ (encoded-stream-tyi *s*)) (return)))


(with-open-file (stream (choose-file-dialog) :direction :input
                        :element-type '(unsigned-byte 8))
  (let ((c nil)
        (w (make-instance 'fred-window))
        (cs (make-instance 'decoding-stream :stream stream)))
    (print (encoded-stream.encoding cs))
    (loop (unless (setf c (stream-tyi cs)) (return))
          (write-char c w))
    (fred-update w)))
|#             


:EOF
