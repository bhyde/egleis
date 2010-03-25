;; -*- mode: lisp; syntax: common-lisp; -*-

(defsystem market
  :version "0.2"
  :author "Ben Hyde <bhyde@pobox.com>"
  :licence "Apache 2.0"
  :depends-on (cl-etsy cl-who hunchentoot)
  :serial t
  :components ((:file "packages")
               (:file "base")
               (:file "sellers")
               (:file "server")
               (:file "upgrade")))

