;;;; vlisp.asd

(defsystem "vlisp"
  :description "Vlisp
  Проект определяет функции и методы направленные на генерирование кода
  Visual Lisp предназначенные для:
  - создания примитивов;
  - манипулирования системными переменными.
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :version "0.1.0"
  :depends-on ("vlisp/core" "vlisp/axis" "vlisp/dr" "vlisp/geometry")
  :serial nil)

(defsystem "vlisp/core"
  :description "Core"
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
  :depends-on ("mnas-string" "vlisp/core")
  :components ((:module "src/dr"
		:serial nil
                :components ((:file "dr")))))

(defsystem "vlisp/axis"
  :description "Axis"
  :depends-on ("mnas-string")
  :components ((:module "src/axis"
		:serial nil
                :components ((:file "axis")))))

(defsystem "vlisp/docs"
  :description "Зависимости для сборки документации"
  :depends-on ("vlisp" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))

(defsystem "vlisp/geometry"
  :description "2D геометрические примитивы (отрезок, окружность, дуга)"
  :depends-on ()
  :serial t
  :components ((:module "src/geometry"
		:serial t
                :components ((:file "package")
                             (:file "primitives")))))
