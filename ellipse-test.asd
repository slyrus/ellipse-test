;;;; ellipse-test.asd

(asdf:defsystem #:ellipse-test
  :description "Describe ellipse-test here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim)
  :components ((:file "package")
               (:file "ellipse-test")))
