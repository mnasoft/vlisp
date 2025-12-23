;;;; src/geometry/methods/curve-end-point.lisp
;;;; Получение точки кривой для её конечного параметра

(in-package #:vlisp/geometry)

(defmethod curve-end-point ((p <point-2d>))
  ;; Точка без протяженности возвращает себя.
  p)

(defmethod curve-end-point ((seg <line-2d>))
  (<line-2d>-end-point seg))

(defmethod curve-end-point ((circ <circle-2d>))
  (curve-point-at-param circ (curve-end-param circ)))

(defmethod curve-end-point ((a <arc-2d>))
  (curve-point-at-param a (curve-end-param a)))
