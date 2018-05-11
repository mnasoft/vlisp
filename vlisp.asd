;;;; vlisp.asd

(asdf:defsystem #:vlisp
  :description "Describe vlisp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mnas-string)
  :components ((:file "package")
               (:file "vlisp")
	       ))
