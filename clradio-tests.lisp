;;;;; Tests for org.facefault.clradio

(in-package :org.facefault.clradio)

(define-test test-pair-channel-bank
  (let ((foo (make-instance 'pair-channel-bank
                            :num-channels 25
                            :first-name #\Q
                            :second-name #\W
                            :subchannel-pos :before))
        (bar (make-instance 'pair-channel-bank
                            :num-channels 25
                            :first-name #\E
                            :second-name #\R
                            :subchannel-pos :after)))
    (assert-equal 1  (parse-pair-channel-num foo "Q1"))
    (assert-equal 2  (parse-pair-channel-num foo "W1"))
    (assert-equal 18 (parse-pair-channel-num foo "W009"))
    (assert-equal 19 (parse-pair-channel-num foo "Q10"))
    (assert-equal 1  (parse-pair-channel-num bar "1E"))
    (assert-equal 2  (parse-pair-channel-num bar "1R"))
    (assert-equal 28 (parse-pair-channel-num bar "14R"))
    (assert-equal 29 (parse-pair-channel-num bar "15E"))))