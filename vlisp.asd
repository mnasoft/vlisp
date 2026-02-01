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
  :version "0.1.2"
  :depends-on ("vlisp/core" "vlisp/axis" "vlisp/dr" "vlisp/geometry")
  :serial nil)

(defsystem "vlisp/core"
  :description "Core"
  :depends-on ("vlisp/geometry" "vlisp/axis" "mnas-string")
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

(defsystem "vlisp/geometry-tests"
  :description "Тесты для модуля vlisp/geometry"
  :depends-on ("vlisp/geometry" "fiveam")
  :serial t
  :components ((:module "src/geometry/tests"
                :serial t
                :components ((:file "tests")))))

(defsystem "vlisp/geometry-svg"
  :description "SVG-рендеринг 2D примитивов vlisp/geometry"
  :depends-on ("vlisp/geometry")
  :serial t
  :components ((:module "src/geometry/svg"
                :serial t
                :components ((:file "package")
                             (:file "svg")))))

(defsystem "vlisp/geometry-svg-tests"
  :description "Тесты SVG-рендеринга"
  :depends-on ("vlisp/geometry-svg" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "geometry-svg-tests")))))

(defsystem "vlisp/geometry"
  :description "2D геометрические примитивы (отрезок, окружность, дуга)"
  :depends-on ()
  :serial t
  :components ((:module "src/geometry"
		:serial t
                :components ((:file "package")
                             (:file "primitives")
                             (:file "defgeneric")
                             (:module "methods"
                                      :serial t
                                      :components ((:file "print-object")
                                                   (:file "perimeter")
                                                   (:file "area")
                                                   (:file "distance")
                                                   (:file "parameters")
                                                   (:file "curve-dist-at-param")
                                                   (:file "curve-point-at-param")
                                                   (:file "curve-start-point")
                                                   (:file "curve-end-point")
                                                   (:file "curve-first-deriv")
                                                   (:file "curve-second-deriv")
                                                   (:file "curve-curvature")
                                                   (:file "curve-normal-at-param")))))))
