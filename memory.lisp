(in-package :org.facefault.clradio)

;;;; Classes first

;;; Memory channels
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
  ((numchannels :reader num-channels :initarg :num-channels)
   (channels :reader channels)
   (firstnumber :reader channel-bank-start :initform 1))
  (:documentation
   "A memory bank.  This parent class has a number of channels
   and that's it."))

(defmethod initialize-instance :after ((bank channel-bank) &rest args)
  (setf (slot-value bank 'channels)
        (make-array (num-channels bank)
                    :fill-pointer 0
                    :initial-element nil)))

(defclass pair-channel-bank (channel-bank)
  ((first-channel-name :reader first-channel-name
                       :initarg :first-name)
   (second-channel-name :reader second-channel-name
                        :initarg :second-name)
   (subchannel-pos :reader subchannel-pos
                   :initarg :subchannel-pos))
  (:documentation
   "A memory bank in which channels are divided into pairs, such as
   for scan boundaries; e.g., 00A-00B, 01A-01B..."))

;;; Initialize PAIR-CHANNEL-BANK... user passes in number
;;; of channel pairs n; we create array with 2n positions accordingly.
(defmethod initialize-instance :after ((bank pair-channel-bank) &rest args)
  (setf (slot-value bank 'channels)
        (make-array (* 2 (num-channels bank))
                    :fill-pointer 0
                    :initial-element nil)))
;;; Radio
(defclass radio ()
  ((memory :reader radio-memory)
   (make :reader radio-make :allocation class)
   (model :reader radio-model :allocation class)))

;;; Getting/setting memory channels
(defgeneric channel-from-bank (bank channel-number))

(defgeneric (setf channel-from-bank) (newchannel bank channel-number))

;; Standard method; relatively uncomplicated.
(defmethod channel-from-bank ((bank channel-bank) channel-number)
  (aref (channels bank) (- channel-number (channel-bank-start bank))))

(defmethod (setf channel-from-bank)
    (newchannel (bank channel-bank) channel-number)
  (setf (aref (channels bank)
              (- channel-number (channel-bank-start bank)))
        newchannel))

;; PAIR-CHANNEL-BANK: have to compute number and pass to parent.

(defun parse-pair-channel-num (bank channel-number)
   (flet ((parse-letter-after ()
           (multiple-value-bind (num pos-of-letter)
               (parse-integer channel-number :junk-allowed t)
             (cons num (subseq channel-number pos-of-letter))))
         (parse-letter-before ()
           (cons (parse-integer (subseq channel-number 1) :junk-allowed t)
                 (subseq channel-number 0 1))))
    (let* ((subchannel-pos (subchannel-pos bank))
           (parsed (cond
                     ((eql subchannel-pos :before)
                      (parse-letter-before))
                     ((eql subchannel-pos :after)
                      (parse-letter-after))
                     (t (error 'bad-position))))
           (chan-num (car parsed))
           (chan-let (cdr parsed)))
      ;; Re-add the start offset to the parsed channel number, as
      ;; it will be subtracted in the superclass.
      (+ (+ (* 2 (- chan-num (channel-bank-start bank)))
            (cond ((string= chan-let (first-channel-name bank)) 0)
                  ((string= chan-let (second-channel-name bank)) 1)
                  (t (error 'bad-subchannel))))
         (channel-bank-start bank)))))

(defmethod channel-from-bank ((bank pair-channel-bank) channel-number)
  (call-next-method bank (parse-pair-channel-num bank channel-number)))

(defmethod (setf channel-from-bank) (newchannel (bank pair-channel-bank)
                                     channel-number)
  (call-next-method newchannel bank
                    (parse-pair-channel-num bank channel-number)))

;;;; Put any functions here that the macros require.
(define-condition invalid-channel-error (error)  
  ((text :initarg :text :reader text)))

(defun new-pair-bank (size params)
  "Create and return a new scan-edge bank.
   Params is something like
      (:first #\A :second \#B :subchannel :before)"
  (let ((first-name (getf params :first))
        (second-name (getf params :second))
        (subchannel (getf params :subchannel)))
    (make-instance 'pair-channel-bank
                   :num-channels size
                   :first-name first-name
                   :second-name second-name
                   :subchannel-pos subchannel)))

(defun new-memory-bank (params)
  "Create and return a new memory bank based on the supplied parameters."
  (cond
    ;; Just the size of the bank
    ((numberp params) (make-instance 'channel-bank
                                     :num-channels params))
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
               `(,param :initform ',init-form
                        :reader ,(reader-symbol param))))
           (slot-form-is-list (param &optional
                                     (init-form (cdr (config-form param))))
             (slot-form param init-form)))
    `(defclass ,name (radio)
       ,(remove nil `(,(slot-form-noaccessor 'model)
                       ,(slot-form-noaccessor 'make)
                       ,(slot-form 'memory
                                   `(init-memory ',(config 'memory)))
                       ,(slot-form 'emission-modes)
                       ,(slot-form-is-list 'frequency-coverage))))))

(defun memory-bank (radio bank)
  (cdr (assoc bank (radio-memory radio))))

(defun available-memory-banks (radio)
  (mapcar #'car (radio-memory radio)))

(defgeneric get-channel (radio bank number)
  (:documentation "Get the memory channel at a given location."))

(defgeneric (setf get-channel) (newchannel radio bank number)
  (:documentation "Replace the memory channel at a given location with a new
                   instance of MEMORY-CHANNEL."))

;;; Define generic implementation, just in case we're overriding
;;; later.
(defmethod get-channel ((radio radio) bank number)
  (channel-from-bank bank (memory-bank radio bank)))

(defmethod (setf get-channel) (newchannel (radio radio) bank number)
  (setf (channel-from-bank bank (memory-bank radio bank)) newchannel))

;;; Frequency coverage check
(defun frequency-valid (radio frequency emission-mode)
  "Return true iff the given frequency (in Hertz) can be tuned
   by this radio using the given emission mode."
  (and (find emission-mode (radio-emission-modes radio))
  (loop
     for range in (radio-frequency-coverage radio)
       thereis (and (>= frequency (first range))
                    (<= frequency (second range))))))