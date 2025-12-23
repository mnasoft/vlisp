;;;; src/geometry/methods/perimeter.lisp
;;;; Периметры для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defmethod perimeter ((seg <line-2d>))
  "Возвращает длину отрезка"
  (let ((dx (- (x (<line-2d>-end-point seg)) (x (<line-2d>-start-point seg))))
        (dy (- (y (<line-2d>-end-point seg)) (y (<line-2d>-start-point seg)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod perimeter ((circ <circle-2d>))
  "Возвращает длину окружности (периметр)"
  (* 2 pi (<circle-2d>-radius circ)))

(defmethod perimeter ((a <arc-2d>))
  "Возвращает длину дуги"
  (let* ((angle-diff (abs (- (<arc-2d>-end-angle a) (<arc-2d>-start-angle a))))
         ;; Используем наименьший угол между начальным и конечным
         (angle (min angle-diff (- (* 2 pi) angle-diff))))
    (* (<arc-2d>-radius a) angle)))
