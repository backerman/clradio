(defpackage :org.facefault.clradio-system
  (:use :common-lisp :asdf))

(in-package :org.facefault.clradio-system)

(defsystem clradio
  :author "Brad Ackerman <brad@facefault.org>"
  :version "0.1"
  :maintainer "Brad Ackerman <brad@facefault.org>"
  :licence "BSD"
  :description "Radio memory stuff thingy."
  :components
  ((:file "packages")
   (:file "alinco" :depends-on ("packages" "serial"))
   (:file "memory" :depends-on ("packages"))
   (:file "serial" :depends-on ("packages"))
   (:file "io"     :depends-on ("packages")))
  :depends-on (:alexandria))
