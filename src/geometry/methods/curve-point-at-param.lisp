;;;; src/geometry/methods/curve-point-at-param.lisp
;;;; Получение точки кривой по параметру

(in-package #:vlisp/geometry)

(defmethod curve-point-at-param ((p <point-2d>) param)
  (declare (ignore param))
  p)

(defmethod curve-point-at-param ((seg <line-2d>) param)
  (let* ((tau (%clamp param (curve-start-param seg) (curve-end-param seg)))
         (x0 (x (<line-2d>-start-point seg)))
         (y0 (y (<line-2d>-start-point seg)))
         (x1 (x (<line-2d>-end-point seg)))
         (y1 (y (<line-2d>-end-point seg)))
         (x (+ x0 (* tau (- x1 x0))))
         (y (+ y0 (* tau (- y1 y0)))))
    (make-instance '<point-2d> :x x :y y)))

(defmethod curve-point-at-param ((circ <circle-2d>) param)
  (let* ((angle (%clamp param (curve-start-param circ) (curve-end-param circ)))
         (r (<circle-2d>-radius circ))
         (cx (x (<circle-2d>-center circ)))
         (cy (y (<circle-2d>-center circ)))
         (px (+ cx (* r (cos angle))))
         (py (+ cy (* r (sin angle)))))
    (make-instance '<point-2d> :x px :y py)))

(defmethod curve-point-at-param ((a <arc-2d>) param)
  (let* ((start (curve-start-param a))
         (end (curve-end-param a))
         (angle (%clamp param (min start end) (max start end)))
         (r (<arc-2d>-radius a))
         (cx (x (<arc-2d>-center a)))
         (cy (y (<arc-2d>-center a)))
         (px (+ cx (* r (cos angle))))
         (py (+ cy (* r (sin angle)))))
    (make-instance '<point-2d> :x px :y py)))
