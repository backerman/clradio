(in-package :org.facefault.clradio)

;;;; Classes first
(defclass memory-channel ()
  ((frequency :documentation "The frequency of the channel, in Hertz.
If this channel describes a repeater, the frequency slot should contain
the output frequency."
	      :accessor frequency)
   (duplex :documentation "The offset between the output and input
frequencies, or 0 if a simplex channel."
	   :accessor duplex
	   :initform 0)
   tone dcs))

(defclass channel-bank ()
  (channels firstnumber))

(defclass pair-channel-bank (channel-bank)
  ((first-channel-name :reader first-channel-name
		       :initarg first-name)
   (second-channel-name :reader second-channel-name
			:initarg second-name)))

(defclass radio ()
  ((memory :reader radio-memory)
   (make :reader radio-make :allocation class)
   (model :reader radio-model :allocation class)))

;;;; Put any functions here that the macros require.
(define-condition invalid-channel-error (error)  
 ((text :initarg :text :reader text)))

(defun new-pair-bank (size params)
  "Create and return a new scan-edge bank.  This is... er,
   exactly like the regular ones right now, it's just that
   each channel is a cons cell with a high and a low."
  (new-memory-bank size))

(defun new-memory-bank (params)
  "Create and return a new memory bank based on the supplied parameters."
  (cond
    ;; Just the size of the bank
    ((numberp params) (make-array params 
				  :fill-pointer 0 
				  :initial-element nil))
    ;; An atom, but not a number
    ((atom params) (error 'not-implemented-yet-error))
    ;; We have an actual list with parameters and stuff.
    ;; The first is the size of the bank, and the rest
    ;; are keyword-value pairs.
    ((cdr params)
     (let ((size (first params))
	   (rest (rest params)))
       (cond
	 ((eql (getf rest :type) 'pair)
	  (new-pair-bank size rest))
	 (t (error 'not-implemented-yet-error)))))
    ;; Default -- a list with one element, that being the size
    (t (new-memory-bank (car params)))))

(defun init-memory (banks)
  "Set up blank memory banks for a new radio.  The banks parameter
should be a list with the names and sizes of the memory banks, e.g.
'((main 1000) (sub 50))."
  (loop for bank in banks
	   collect (cons (car bank) (new-memory-bank (cdr bank)))))

(defun parse-a-b-channel (channel-number num-edge-pairs)
  (flet ((is-valid-chan (num a-or-b)
	   (and (<= 0 num (- num-edge-pairs 1)) (or (string= a-or-b "A")
						   (string= a-or-b "B")))))
    (multiple-value-bind (num pos-of-letter)
	(parse-integer channel-number :junk-allowed t)
      (let ((letter (subseq channel-number pos-of-letter)))
	(if (is-valid-chan num letter)
	    (+ (* 2 num) (if (string= letter "A") 0 1))
	    (error 'invalid-channel-error :text channel-number))))))

;; Subclasses of RADIO are generated using the define-radio
;; macro.
;; FIXME: slot-form only does read-only slots.  Okay?
(defmacro define-radio (name configuration-forms)
  (labels ((config-form (param) (assoc param configuration-forms))
	   (config (param) (let ((x (cdr (config-form param))))
			     (if (cdr x) x (car x))))
	   (configured-p (param) (config-form param))
	   (reader-symbol (param)
	     (intern (string-upcase
		      (concatenate 'string "radio-" (symbol-name param)))))
	   (slot-form-noaccessor (param)
	     `(,param :initform ,(config param)))
	   (slot-form (param &optional (init-form (config param)))
	     (when (configured-p param)
	       `(,param :initform ,init-form
			:reader ,(reader-symbol param)))))
    `(defclass ,name (radio)
       ,(remove nil
		`(,(slot-form-noaccessor 'model)
		   ,(slot-form-noaccessor 'make)
		   ,(slot-form 'memory `(init-memory ',(config 'memory))))))))

;; TODO: Fix it so I don't have to keyword parameters.  Add support
;; for special memory types (to start: A-B).
(define-radio ic-r20
  ((make                   "Icom")
   (model                  "IC-R20")
   (memory                 (main 1000)
			   (scan-edge 25 :type pair :first #\A :second #\B))
   (emission-modes         :am :cw :usb :lsb :fm :wfm)
   (tone-squelch-modes     :ctcss :dcs)
   (frequency-coverage     (50000 2999999999))))

(defun memory-bank (radio bank)
  (cdr (assoc bank (radio-memory radio))))

(defun available-memory-banks (radio)
  (mapcar #'car (radio-memory radio)))

(defgeneric get-channel (radio bank number)
  (:documentation "Get the memory channel at a given location."))

(defgeneric (setf get-channel) (newchannel radio bank number)
  (:documentation "Replace the memory channel at a given location with a new
instance of MEMORY-CHANNEL."))

;;; Define generic implementation -- will be overridden
;;; for A/B banks etc.
(defmethod get-channel ((radio radio) bank number)
  (let ((bank-channels (memory-bank radio bank)))
    (aref bank-channels (1- number))))

(defmethod (setf get-channel) (newchannel (radio radio) bank number)
  (let ((bank-channels (memory-bank radio bank)))
    (setf (aref bank-channels (1- number)) newchannel)))

;;;; TODO: Write method to iterate through channels in a bank, doing a
;;;; multiple-value-bind providing the channel number.  As with
;;;; get-channel, the overridden forms should be included in the
;;;; define-radio macro's output.

;;; TODO: Sample upper-lower implementation -- macroize me.
;;; Take it as a string. 00A/B -> 24A/B in this case
(defmethod get-channel ((radio ic-r20) (bank (eql 'scan-edge)) channel-number)
  ;; Scan the input... two numbers and a letter A or B.
  (call-next-method radio bank
		    (parse-a-b-channel channel-number 25)))

(defmethod (setf get-channel) (newchannel (radio ic-r20) 
			       (bank (eql 'scan-edge)) channel-number)
  (error 'not-implemented-yet-error))