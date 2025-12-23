;;;; vlisp-tests.asd

(defsystem "vlisp-tests"
  :description "Тесты для vlisp (векторные операции, геометрические функции)"
  :version "0.1.0"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  
  :depends-on (:vlisp
               :vlisp/geometry-svg
               :fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :vlisp/tests :run-tests))
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "utils")
     (:file "vector-tests")
     (:file "geometric-tests")
     (:file "geometry-svg-tests")))))
