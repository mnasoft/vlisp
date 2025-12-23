;;;; src/geometry/methods/parameters.lisp
;;;; Параметры начала и конца кривой для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defmethod curve-start-param ((p <point-2d>))
  "Точка не имеет протяженности, параметр равен 0."
  0.0)


(defmethod curve-end-param ((p <point-2d>))
  "Точка не имеет протяженности, параметр равен 0."
  0.0)

(defmethod curve-start-param ((seg <line-2d>))
  "Начальный параметр отрезка."
  0.0)

(defmethod curve-end-param ((seg <line-2d>))
  "Конечный параметр отрезка."
  1.0)

(defmethod curve-start-param ((circ <circle-2d>))
  "Начальный параметр окружности (радианы)."
  0.0)

(defmethod curve-end-param ((circ <circle-2d>))
  "Конечный параметр окружности (радианы)."
  (* 2 pi))

(defmethod curve-start-param ((a <arc-2d>))
  "Начальный параметр дуги (радианы)."
  (<arc-2d>-start-angle a))

(defmethod curve-end-param ((a <arc-2d>))
  "Конечный параметр дуги (радианы)."
  (<arc-2d>-end-angle a))
