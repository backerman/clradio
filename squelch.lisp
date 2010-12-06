;;;;; Common vocabulary for squelch parameters.

(in-package :org.facefault.clradio)

(defparameter *dcs-words*
  (list "023" "025" "026" "031" "032" "036" "043" "047"
        "051" "053" "054" "065" "071" "072" "073" "074" "114" "115"
        "116" "122" "125" "131" "132" "134" "143" "145" "152" "155"
        "156" "162" "165" "172" "174" "205" "212" "223" "225" "226"
        "243" "244" "245" "246" "251" "252" "255" "261" "263" "265"
        "266" "271" "274" "306" "311" "315" "325" "331" "332" "343"
        "346" "351" "356" "364" "365" "371" "411" "412" "413" "423"
        "431" "432" "445" "446" "452" "454" "455" "462" "464" "465"
        "466" "503" "506" "516" "523" "526" "532" "546" "565" "606"
        "612" "624" "627" "631" "632" "654" "662" "664" "703" "712"
        "723" "731" "731" "734" "743" "754"))

(defparameter *dcs-polarities*
  (list :positive :negative))

(defparameter *ctcss-tones*
  (list "67.0" "69.3" "71.9" "74.4" "77.0" "79.7" "82.5"
        "85.4" "88.5" "91.5" "94.8" "97.4" "100.0" "103.5" "107.2"
        "110.9" "114.8" "118.8" "123.0" "127.3" "131.8" "136.5" "141.3"
        "146.2" "151.4" "156.7" "159.8" "162.2" "165.5" "167.9" "171.3"
        "173.8" "177.3" "179.9" "183.5" "186.2" "189.9" "192.8" "196.6"
        "199.5" "203.5" "206.5" "210.7" "218.1" "225.7" "229.1" "233.6"
        "241.8" "250.3" "254.1"))

(defparameter *squelch-modes*
  (list :off :tsql :dcs :vsc))

(defun valid-dcs-words () *dcs-words*)

(defun valid-dcs-polarities () *dcs-polarities*)

(defun valid-ctcss-tones () *ctcss-tones*)

(defun valid-squelch-modes () *squelch-modes*)

(defun valid-dcs-word (word polarity)
  (and
   (find polarity (valid-dcs-polarities))
   (find word (valid-dcs-words))))