(in-package :org.facefault.clradio)

(define-radio ic-r20
    ((make                   "Icom")
     (model                  "IC-R20")
     (memory                 (main 1000)
                             (scan-edge 25
                                        :type pair :first #\A :second #\B
                                        :subchannel-pos :after))
     (emission-modes         :am :cw :usb :lsb :fm :wfm)
     (tone-squelch-modes     :ctcss :dcs)
     (frequency-coverage     (50000 2999999999))))

