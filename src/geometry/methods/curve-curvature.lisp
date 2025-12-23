;;;; src/geometry/methods/curve-curvature.lisp
;;;; Кривизна кривой в параметре (скаляр)

(in-package #:vlisp/geometry)

(defun %clamp (value min-value max-value)
  (max min-value (min max-value value)))

(defmethod curve-curvature ((p <point-2d>) param)
  (declare (ignore param))
  ;; Для точки не определена
  nil)

(defmethod curve-curvature ((seg <line-2d>) param)
  (declare (ignore param))
  ;; Линия: кривизна 0
  0.0)

(defmethod curve-curvature ((circ <circle-2d>) param)
  (declare (ignore param))
  (let ((r (<circle-2d>-radius circ)))
    (if (and r (> r 0.0))
        (/ 1.0 r)
        nil)))

(defmethod curve-curvature ((a <arc-2d>) param)
  (declare (ignore param))
  (let ((r (<arc-2d>-radius a)))
    (if (and r (> r 0.0))
        (/ 1.0 r)
        nil)))
