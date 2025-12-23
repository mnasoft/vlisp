;;;; src/geometry/methods/curve-start-point.lisp
;;;; Получение точки кривой для её начального параметра

(in-package #:vlisp/geometry)

(defmethod curve-start-point ((p <point-2d>))
  ;; Точка без протяженности возвращает себя.
  p)

(defmethod curve-start-point ((seg <line-2d>))
  (<line-2d>-start-point seg))

(defmethod curve-start-point ((circ <circle-2d>))
  (curve-point-at-param circ (curve-start-param circ)))

(defmethod curve-start-point ((a <arc-2d>))
  (curve-point-at-param a (curve-start-param a)))
