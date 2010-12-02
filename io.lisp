;;;;; IO-related utility functions
(in-package :org.facefault.clradio)

(defparameter *crlf* (format nil "~C~C" #\Return #\Linefeed))

(defmacro debug-print (&body body)
  "Evaluate the body exactly once and answer its value.  If *debug* is
   non-nil, also print the result to *standard-output*."
  (let ((resultvar (gensym)))
    `(let ((,resultvar (progn ,@body)))
       (when *debug*
	 (print ,resultvar))
       ,resultvar)))

(defun read-line-cr (stream)
  "Read a line from the given stream.  Answer the line
   with trailing CR characters removed."
  (debug-print
    (string-trim '(#\Return)
		 (read-line stream nil))))

(defun write-line-cr (line stream &key (read-echo t))
  "Write a line to the given stream, terminating it with
   CR only.  Afterwards, read one line from the device,
   which is assumed to be a copy of the output."
  (write-string line stream)
  (write-char #\Return stream)
  (read-line-cr stream)) ;; read back echo from device

(defun output-dump-format (str &optional
			   (num-octets-in-line 16)
			   (starting-address 0))
  "Given a string consisting of a memory dump in
   hex encoding, output it sixteen bytes at a time (by default)
   with the starting address prepended to each line."
  (with-output-to-string (output)
    (let ((strlen (length str))
	  (pos-increment (* num-octets-in-line 2)))
      (do ((pos 0 (+ pos pos-increment))
	   (addr starting-address (+ addr num-octets-in-line)))
	  ((>= pos strlen))
	(format output "~6,'0X ~a~%" addr
		(subseq str pos (min strlen (+ pos pos-increment))))))))