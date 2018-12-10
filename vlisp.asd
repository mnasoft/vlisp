;;;; vlisp.asd

(defsystem #:vlisp
  :description "Describe vlisp here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.1"
  :serial t
  :depends-on (#:mnas-string)
  :components ((:file "package")
               (:file "vlisp")
	       (:file "import-sp-line")))
