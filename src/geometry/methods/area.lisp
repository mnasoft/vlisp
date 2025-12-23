;;;; src/geometry/methods/area.lisp
;;;; Площади для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defmethod area ((circ <circle-2d>))
  "Возвращает площадь окружности"
  (* pi (<circle-2d>-radius circ) (<circle-2d>-radius circ)))
