;;;;; Dump memory from an Alinco.

(in-package :org.facefault.clradio)

(defparameter *radio-tty* "/dev/ttyUSB0")

(defparameter *radio-baud* 57600)

(defparameter *radio-model* "DJ-G7T")

(defparameter *debug* nil)

(defun alinco-read-memory-command (start)
  "Return the radio command to read memory from the provided start location."
  (format nil "AL~~F~5,'0XR" start))

(defun alinco-read-from-position (pos tty)
  "Read from the radio's memory at position pos.  The radio
     has been opened on stream tty."
  (write-line-cr (alinco-read-memory-command pos) tty)
  (read-line-cr tty))

(defun alinco-dump-memory ()
  (with-serial-port (tty *radio-tty* :speed *radio-baud* :timeout 1.0)
    (write-line-cr (concatenate 'string "AL~" *radio-model*) tty)
    (let ((status (read-line-cr tty)))
      (if (string= status "OK")
	  (with-output-to-string (dump)
	    (do ((pos 0 (+ pos (/ (length line) 2)))
		 (line (alinco-read-from-position 0 tty)
		       (alinco-read-from-position pos tty)))
		;; exit when
		((string= line "NG"))
	      (debug-print
	       (format nil "To read from position ~5,'0X" pos))
	      (when (= 0 (mod pos #x1000))
		(format *standard-output* "~5,'0X " pos))
	      (write-sequence line dump)))
	  (progn 
	    (format t "Error: received ~a" status)
	    (let ((newstatus (read-line-cr tty)))
	      (format t "Further received ~a" newstatus)))))))