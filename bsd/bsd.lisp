;;; -*- Package: de.setf.utility.bsd; -*-

;;;
;;;  This file implements the 'bsd' component of the 'de.setf.utility' Common Lisp library.
;;;  It implements a small numer of bsd environmental, process control and i/o operators for mcl.
;;;
;;;  Copyright 2003, 2004, 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;;  Copyright 2003 [Brendan Burns](mailto:jbburns@cs.umass.edu) All Rights Reserved
;;;  Copyright 2003 [Gary King](mailto:gwking@cs.umass.edu) All Rights Reserved
;;;  'bsd' is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU Lesser General Public License as published by
;;;  the Free Software Foundation, either version 3 of the License, or
;;;  (at your option) any later version.
;;;
;;;  'bsd' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the GNU Lesser General Public License for more details.
;;;
;;;  A copy of the GNU Lesser General Public License should be included with `bsd`, as `lgpl.txt`.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).
;;;
;;;  This version replaces Burns' implementation of a superset of these operators.
;;;  At least as of MCL-5.2b6, which introduced support for foreign-function calls to macho frameworks,
;;;  the original implementation, which was based on the <code>PPC-FF-CALL</code>-based interface failed,
;;;  and it made sense to reimplement it in terms of the MCL support in via with <code>DEFTRAP-INLINE</code>.
;;;  This implementation constitutes a complete rewrite of that earlier version, and several
;;;  operators present _incompatible_ interfaces.
;;;
;;;  The stream wrappers are ported from contributions to asdf-install based on the earlier bsd operators.
;;;
;;;  Among the notes to the original implementation:
;;;      Portions of this code were taken from code by Gary Byers.
;;;      This code is derived from Apple computer's MiniShell sample
;;;      application. This code is included below and can be found at
;;;      http://developer.apple.com/samplecode/Sample_Code/OS_Utilities/MiniShell.htm
;;;      This code is released under the LGPL, please see http://www.gnu.org for more info.
;;;      The author is not responsible for all of the Really Bad Things(tm)
;;;      you could do with this code.
;;;
;;; Operators
;;;   hostname
;;;   popen
;;;   pclose
;;;   feofp
;;;   fread
;;;   fwrite
;;;   run-command
;;;
;;; Classes
;;;   pipe-input-stream
;;;   pipe-output-stream

;;; ---------------------------------------------------------------------------
;;; Instructions
;;; ---------------------------------------------------------------------------

#-(and :ccl :digitool :CCL-5.2)
(error "These definitions depend on MCL's trap mechanism.")
 
(defpackage :de.setf.utility.bsd
  (:use :common-lisp :ccl)
  (:nicknames :bsd)
  (:export :*run-command-eol*
           :gethostname
           :popen
           :pclose
           :run-command
           :feofp
           :fread
           :fwrite
           :file-bsd-namestring
           :file-mac-namestring
           :getenv
           :pipe-input-stream
           :pipe-io-stream
           :pipe-output-stream
           :with-popen)
  (:documentation
   "The `:de.setf.utility.bsd` library component implements low-level support for a limited number
 of 'Berkeley' UNIX library primitive operators:
 - getenv
 - gethostname
 - popen
 - pclose
 - run-command
 - fread
 - fwrite

 The i/o operators are used to implement the stream specialization which wrap the block i/o streams:
 - pipe-input-stream
 - pipe-output-stream
 "))


(in-package :de.setf.utility.bsd)

;;;
;;; parameters

(defvar *run-command-eol* #\return
  "Indicates the intended EOL encoding in results from RUN-COMMAND.
 #\return yields MAC classic eocoding, #\linefeed leaves the 'native' UNIX encoding.")


;;;
;;; namestring utilities

(defmethod bsd:file-mac-namestring ((path pathname))
  (bsd:file-mac-namestring (namestring path)))

(defmethod bsd:file-mac-namestring ((path string))
  (let ((real-path (substitute #\: #\/ path)))
    (if (not (eq (elt real-path (1- (length path))) #\:)) ;;;---??? jaa
      (setf real-path (concatenate 'string real-path ":")))
    (if (eq (elt real-path 0) #\:)
      (if (string-equal (subseq real-path 0 8) ":volumes")
        (subseq real-path 9)
        (concatenate 'string (namestring (ccl::boot-directory)) 
                     (subseq real-path 1)))
      (concatenate 'string (namestring (ccl::mac-default-directory))
                   real-path))))


(defmethod bsd:file-bsd-namestring ((path pathname))
  (bsd:file-bsd-namestring (namestring path)))

(defmethod bsd:file-bsd-namestring ((path logical-pathname))
  (bsd:file-bsd-namestring (translate-logical-pathname path)))

(defmethod bsd:file-bsd-namestring ((path string))
  (let ((boot-path (namestring (ccl::boot-directory))))
    (when (string-equal  boot-path path :end2 (min (length boot-path) (length path)))
      (setf path (subseq path (length boot-path)))))
  (let ((unix-path (substitute #\/ #\: path)))
    (if (eql #\/ (char unix-path 0))
      ;; insert the working directory
      (concatenate 'string (bsd:file-bsd-namestring (namestring (ccl::mac-default-directory)))
                   unix-path)
      ;; assert to be the root
      (concatenate 'string "/" unix-path))))
;;; (bsd:file-bsd-namestring #p"LIBRARY:de;setf;bsd.lisp")

;;;
;;; FFI

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftrap-inline "_feof"
    ((file-pointer :pointer))
    :signed-word
    ())
  (deftrap-inline "_fread"
    ((buffer :pointer)
     (length :unsigned-word)
     (count :unsigned-word)
     (file-pointer :pointer))
    :signed-word
    ())
  (deftrap-inline "_fwrite"
    ((buffer :pointer)
     (length :unsigned-word)
     (count :unsigned-word)
     (file-pointer :pointer))
    :signed-word
    ())
  (deftrap-inline "_getenv"
    ((string :pointer))
    :pointer
    ())
  (deftrap-inline "_gethostname"
    ((returnArg :pointer)
     (length :unsigned-long))
    :unsigned-long
    ())
  (deftrap-inline "_pclose"
    ((file :pointer))
    :signed-word
    ())
  (deftrap-inline "_popen"
    ((command :pointer)
     (mode :pointer))
    :pointer
    ()))


(defun bsd:getenv (string)
  (ccl:with-cstrs ((%string string))
    (let ((%value (#_getenv %string)))
      (unless (ccl:%null-ptr-p %value)
        (ccl:%get-cstring %value)))))


(defun bsd:gethostname ()
  "Return the system hostname as a string.
 See gethostname(3)."
  (%stack-block ((buf 512))
    (if (eql 0 (#_gethostname buf 512))
      (ccl:%get-cstring buf))))


(defgeneric bsd:popen (command &key type)
  
  (:method ((command string) &key (type "r") &aux (types '("r" "w" "r+")))
    "Open a pipe to a command process. Return a FILE* if successful or zero upon error.

 COMAMND : STRING : a string comprising the executable path and the command-line arguments.
 VALUE : MACPTR : the pipe file pointer is returned in all cases; as the failure modes
  derive from fork and pipe only, errors may be inferred only from the pclose return value.

 See popen(3), fork(2), pipe(2)."
    
    (assert (member (setf type (string-downcase type)) types :test 'equal) ()
            "invalid pipe type; one of ~s is required." types) 
    (ccl:with-cstrs ((%command command)
                     (%mode type))
      (#_popen %command %mode)))
  
  (:method ((command cons) &rest args)
    (declare (dynamic-extent args))
    
    (flet ((coerce-if-pathname (object)
             (typecase object
               (pathname (bsd:file-bsd-namestring object))
               (t object))))
      (apply #'bsd::popen (format nil "~{~a ~}" (mapcar #'coerce-if-pathname command))
             args))))


(defun bsd:pclose (%file)
  "Close an open pipe file.
 %FILE : MACPTR : the open pipe file pointer."
   (#_pclose %file))

(defun bsd:feofp (%file)
  (not (zerop (#_feof %file))))

(defgeneric bsd:fread (buffer %file &key length)
  (:documentation
   "Read input from a designated file stream into a buffer.

 BUFFER : (or MACPTR (VECTOR BYTE) STRING : a buffer to accept input
 %FILE : MACPTR : a file stream pointer
 :LENGTH : INTEGER : the number of bytes to accept; default is the buffer length

 See fread(3).")

  (:method ((%buffer macptr) (%file macptr) &key (length (error "length required.")))
    "Read a buffer of the given length. Indicate a count of byte elements in order to read a partial buffer.
     Return the number of buffers read, which should be 1."
    (#_fread %buffer 1 length %file))

  (:method ((buffer vector) (%file macptr) &key (length (length buffer)))
    (ccl:%stack-block ((%buffer length))
      (let ((read-length (#_fread %buffer 1 length %file)))
        (when (> read-length 0)
          (dotimes (i read-length)
            (setf (aref buffer i) (%get-byte %buffer i))))
        read-length)))

  (:method ((buffer string) (%file macptr) &key (length (length buffer)))
    (ccl:%stack-block ((%buffer length))
      (let ((read-length (#_fread %buffer 1 length %file)))
        (when (> read-length 0)
          (dotimes (i read-length)
            (setf (aref buffer i) (code-char (%get-byte %buffer i)))))
        read-length))))
  

(defgeneric bsd:fwrite (buffer %file &key length)
  (:documentation
   "Write output from a buffer to a designated file stream.

 BUFFER : (or MACPTR (VECTOR BYTE) STRING : a buffer to accept input
 %FILE : MACPTR : a file stream pointer
 :LENGTH : INTEGER : the number of bytes to accept; default is the buffer length

 See fwrite(3).")

  (:method ((%buffer macptr) (%file macptr) &key (length (error "length required.")))
    (#_fwrite %buffer length 1 %file))

  (:method ((buffer string) (%file macptr) &key (length (length buffer)))
    (%stack-block ((%buffer length))
      (dotimes (i length)
        (ccl::%put-byte %buffer (char-code (aref buffer i)) i))
      (#_fwrite %buffer length 1 %file)))

  (:method ((buffer vector) (%file macptr) &key (length (length buffer)))
    (%stack-block ((%buffer length))
      (dotimes (i length)
        (ccl::%put-byte %buffer (aref buffer i) i))
      (#_fwrite %buffer length 1 %file))))


(defun bsd:run-command (program &rest args)
  "run a unix command and return the standard output as a string.

 PROGRAM : STRING : the absolute pathname of the executable
 ARGS : (LIST STRING) : a &rest list of the program's command-line arguments
 VALUES : STRING : the collected program standard output.
          INTEGER : the process exit code

 This is the main interface for Darwin access. It accepts a Unix command as
 string arguments and returns the results of that command as a string.
 The parameter <code>*run-command-eol*</code> indicates the desired EOL encoding. Use
 <code>#\return</code> the recode the EOL's to match MAC conventions, and
 <code>#\linefeed</code> to leave the 'native' UNIX encoding unchainged.
 For example:

? (run-command \"date\")
\"Sat Jan 11 23:45:32 EST 2003\"

? (run-command \"cal\")
\"    January 2003
 S  M Tu  W Th  F  S
          1  2  3  4
 5  6  7  8  9 10 11
12 13 14 15 16 17 18
19 20 21 22 23 24 25
26 27 28 29 30 31
\""
  (flet ((coerce-if-pathname (object)
             (typecase object
               (pathname (bsd:file-bsd-namestring object))
               (t object))))
    (let* ((command (format nil "~a~{ ~a~}" (coerce-if-pathname program) (mapcar #'coerce-if-pathname args)))
           (fp nil)
           (buffer-length 32)
           (buffer (make-string buffer-length))
           (result "")
           (exit-status nil))
      (unwind-protect
        (when (not (%null-ptr-p (setf fp (popen command))))
          (do ((read-length (bsd:fread buffer fp) (bsd:fread buffer fp)))
              ((zerop read-length))
            (setf result (concatenate 'string result
                                      (if (= read-length buffer-length)
                                        buffer
                                        (subseq buffer 0 read-length))))))
        (when fp (setf exit-status (pclose fp))))
      (values (ecase *run-command-eol*
                (#\return (nsubstitute #\return #\linefeed result))
                (#\linefeed result))
              exit-status))))

#+asdf
(let ((*warn-if-redefine* nil))
  (defun asdf:run-shell-command (control-string &rest args)
    "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
    (flet ((coerce-if-pathname (object)
             (typecase object
               (pathname (bsd:file-bsd-namestring object))
               (t object))))
      (let ((command (apply #'format nil control-string (mapcar #'coerce-if-pathname args))))
        (format asdf::*verbose-out* "~&; $ ~A~%" command)
        (multiple-value-bind (output exit-code)
                             (bsd:run-command command)
          (when (and (zerop exit-code) asdf::*verbose-out*)
            (write-string output asdf::*verbose-out*))
          exit-code)))))


;;; stream support

(defmacro bsd:with-popen ((stream command &rest args) &body body)
  (let ((body-op (gensym)))
    `(flet ((,body-op (,stream) ,@body))
       (declare (dynamic-extent #',body-op))
       (bsd::call-with-open-pipe #',body-op ,command ,@args))))


(defgeneric bsd::call-with-open-pipe (function command &key direction)

  (:method (function (command t) &key (direction (error "direction is required")))
    "Create the input/output pipe and invoke the operator. if the stream is subsequently
 still open, close it."
    (let ((done nil)
          (stream nil))
      (unwind-protect
        (multiple-value-prog1
          (funcall function (setf stream (make-instance (ecase direction
                                                          (:input 'bsd:pipe-input-stream)
                                                          (:output 'bsd:pipe-output-stream)
                                                          (:io 'bsd:pipe-io-stream))
                                           :command command)))
          (setf done t))
        (when stream (close stream :abort (null done)))))))


(defclass pipe-stream (stream)
  ((fp :initform nil )
   (script :initarg :script :initform (ccl::default-script nil))))


(defclass bsd:pipe-input-stream (pipe-stream ccl::input-stream)
  ((in-buffer :initform nil)
   (in-string-buffer :initform nil)
   (in-length :initform 0)
   (in-index :initform 0))
  (:default-initargs :direction :input))


(defclass bsd:pipe-output-stream (pipe-stream ccl::output-stream)
  ((out-buffer :initform nil)
   (out-string-buffer :initform nil)
   (out-length :initform 0)
   (out-index :initform 0))
  (:default-initargs :direction :output))

(defclass bsd:pipe-io-stream (bsd:pipe-input-stream bsd:pipe-output-stream)
  ()
  (:default-initargs :direction :io))


(defmethod stream-element-type ((stream pipe-stream))
  "Support just characters and leave encodings for another layer."
  'character)


(defmethod initialize-instance :after ((instance pipe-input-stream) &key)
  (with-slots (in-buffer in-string-buffer in-length) instance
    (setf in-buffer (#_NewPtr :errchk 512)
          in-length 512
          in-string-buffer (make-array 512 :element-type 'character :adjustable t))))

(defmethod initialize-instance :after ((instance pipe-output-stream) &key)
  (with-slots (out-buffer out-string-buffer out-length) instance
    (setf out-buffer (#_NewPtr :errchk 512)
          out-length 512
          out-string-buffer (make-array 512 :element-type '(unsigned-byte 32)))))

(defmethod initialize-instance :after ((instance pipe-stream)
                                       &key (command (error "command is required.")) direction)
  (with-slots (fp script) instance
    (setf fp (bsd:popen command :type (ecase direction (:input "r") (:output "w") (:io "r+"))))
    (when script (unless (ccl::get-char-byte-table script) (setf script nil)))))


(defmethod ccl::stream-close ((stream pipe-stream))
  (with-slots (fp ccl::direction) stream
    (when (and fp (not (ccl::%null-ptr-p fp)))
      (pclose fp)
      (setf fp nil))
    (call-next-method)
    (setf ccl::direction :closed)))

(defmethod ccl::stream-close ((stream pipe-input-stream))
  (with-slots (in-buffer) stream
    (when (and in-buffer (not (ccl::%null-ptr-p in-buffer)))
      (#_DisposePtr in-buffer)
      (setf in-buffer nil))
    (call-next-method)))

(defmethod ccl::stream-close ((stream pipe-stream))
  (with-slots (fp output-buffer out-string-buffer out-index out-length script) stream
    (when (and output-buffer (not (ccl::%null-ptr-p output-buffer)))
      (when (plusp out-index)
        (fwrite-encoded fp output-buffer out-index out-string-buffer script)
        (setf out-index 0))
      (#_DisposePtr output-buffer)
      (setf output-buffer nil))
    (call-next-method)))
    

(defun fread-decoded (fp io-buffer io-buffer-length string-buffer script)
  (cond ((bsd:feofp fp)
         (values nil string-buffer))
        (t
         (let ((io-count (bsd:fread io-buffer fp :length io-buffer-length)))
           (cond ((plusp io-count)
                  (if script
                    (multiple-value-bind (chars fatp) (ccl::pointer-char-length io-buffer io-count script)
                      (cond ((not fatp)
                             (ccl::%copy-ptr-to-ivector io-buffer 0 string-buffer 0 io-count))
                            (t
                             (unless (= (length string-buffer) chars)
                               (setf string-buffer (adjust-array string-buffer chars)))
                             (ccl::pointer-to-string-in-script io-buffer string-buffer io-count script)
                             (setf io-count chars))))
                    (ccl::%copy-ptr-to-ivector io-buffer 0 string-buffer 0 io-count))
                  (values io-count string-buffer))
                 (t
                  (values 0 string-buffer)))))))

(defun fwrite-encoded (fp out-buffer out-buffer-length char-code-buffer char-code-count)
  "MCL supports two-byte scripts only. thus the simplistic encoding."
  (let ((extended-length char-code-count)
        (extended-p nil))
    (dotimes (i char-code-count) (when (ccl::%i> (aref char-code-buffer i) #xff)
                                   (incf extended-length)
                                   (setf extended-p t)))
    (cond (extended-p
           (let ((char-code-offset 0) (buffer-offset 0))
             (loop (when (>= char-code-offset char-code-count)
                     (when (>= buffer-offset 0)
                       (bsd:fwrite out-buffer fp :length buffer-offset))
                     (return))
                   (when (>= buffer-offset out-buffer-length)
                     (bsd:fwrite out-buffer fp :length out-buffer-length))
                   (let ((code (aref char-code-buffer char-code-offset)))
                     (when (ccl::%i> code #xff)
                       (ccl:%put-byte out-buffer (ash code -8) buffer-offset)
                       (incf buffer-offset))
                     (ccl:%put-byte out-buffer (ccl::%ilogand code #xff) buffer-offset)
                     (incf buffer-offset)
                     (incf char-code-offset)))))
          (t
           (ccl::%copy-ivector-to-ptr char-code-buffer 0 out-buffer 0 extended-length)
           (bsd:fwrite out-buffer fp :length extended-length)))))

(defmethod ccl::stream-tyi ((stream bsd:pipe-input-stream))
  ;; despite the decoding provisions, unix input comes with linefeeds
  ;; and i don't know what decoding one would need.
  (with-slots (in-buffer fp in-string-buffer in-length in-index script) stream
    (when fp
      (when (>= in-index (length in-string-buffer))
        (multiple-value-bind (read-length read-buffer)
                             (fread-decoded fp in-buffer in-length in-string-buffer script)
          (unless (and read-length (plusp read-length))
            (setf in-length -1)
            (return-from ccl::stream-tyi nil))
          ;; in case it changes size
          (setf in-string-buffer read-buffer))
        (setf in-index 0))
      (let ((char (schar in-string-buffer in-index)))
        (incf in-index)
        (case char
          ((#\return #\linefeed) #\newline)
          (t char))))))
    
(defmethod ccl::stream-untyi ((stream bsd:pipe-input-stream) char)
  (with-slots (string-buffer length index) stream
    (unless (and (plusp index) (eql char (schar string-buffer (decf index))))
      (error "invalid tyi character: ~s." char))
    char))

(defmethod ccl::stream-tyo ((stream bsd:pipe-output-stream) char)
  (with-slots (out-buffer fp string-buffer out-length out-index script) stream
    (when fp
      (setf (aref string-buffer out-index) (char-code char))
      (when (>= out-index out-length)
        (unless (eql (fwrite-encoded fp out-buffer out-length string-buffer out-index) out-length)
          (setf out-length -1)
          (return-from ccl::stream-tyo nil))
        (setf out-index 0))
      char)))

(defmethod ccl::stream-eofp ((stream bsd:pipe-input-stream))
  (with-slots (length) stream
    (minusp length)))


(pushnew :bsd *features*)

;; (bsd:file-bsd-namestring "tmp:dot.jpg")
;; (hostname)
;; (run-command "ls" "-l")
;; (run-command "lsx") -> a zero length string and a non-zero exit-code
;; (run-command "pwd")
;; (run-command "cal")

;; (let ((asdf::*verbose-out* *trace-output*)) (asdf:run-shell-command "ls"))

#+(or)
(progn
  (bsd:with-popen (input "/bin/cat /etc/passwd" :direction :input)
    (loop (let ((line (read-line input nil nil)))
            (unless line (return))
            (print line))))

  )



