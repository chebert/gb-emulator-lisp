;;;; gb-emulator.asd

(asdf:defsystem #:gb-emulator
  :description "Describe gb-emulator here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:modest)
  :serial t
  :components ((:file "package")
               (:file "gb-emulator")))
