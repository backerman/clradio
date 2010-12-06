(defpackage :org.facefault.clradio-system
  (:use :common-lisp :asdf))

(in-package :org.facefault.clradio-system)

(defsystem clradio
  :author "Brad Ackerman <brad@facefault.org>"
  :version "0.1"
  :maintainer "Brad Ackerman <brad@facefault.org>"
  :licence "ISC-like for serial.lisp; rest is WTFPL"
  :description "Radio memory stuff thingy."
  :components
  ((:file "packages")
   (:file "alinco-comm"   :depends-on ("packages" "serial" "io"))
   (:file "memory"        :depends-on ("packages"))
   (:file "serial"        :depends-on ("packages"))
   (:file "io"            :depends-on ("packages"))
   ;; Radio descriptions
   (:file "icom"          :depends-on ("packages"))
   ;; Tests
   (:file "clradio-tests" :depends-on ("packages")))
  :depends-on (:alexandria :lisp-unit))

