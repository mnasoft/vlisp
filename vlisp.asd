;;;; vlisp.asd

(defsystem "vlisp"
  :description "Vlisp
  Проект определяет функции и методы направленные на генерирование кода
  Visual Lisp предназначенные для:
  - создания примитивов;
  - манипулирования системными переменными.
"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.0.2"
  :depends-on ("vlisp/core" "vlisp/axis")
  :serial nil)

(defsystem "vlisp/core"
  :description "Core"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("vlisp/axis" "mnas-string")
  :serial t
  :components ((:module "src/core"
                :serial nil
                :components
                ((:file "vlisp")
                 (:file "geometric-functions")
                 #+nil (:file "import-sp-line")))))

(defsystem "vlisp/dr"
  :description "Axis"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string")
  :components ((:module "src/dr"
		:serial nil
                :components ((:file "dr")))))

(defsystem "vlisp/axis"
  :description "Axis"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-string")
  :components ((:module "src/axis"
		:serial nil
                :components ((:file "axis")))))

(defsystem "vlisp/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("vlisp" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))
