;;;; src/geometry/methods/curve-first-deriv.lisp
;;;; Нормализованный касательный вектор кривой в параметре

(in-package #:vlisp/geometry)

(defun %clamp (value min-value max-value)
  (max min-value (min max-value value)))

(defun %normalize (dx dy)
  (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (<= len 0.0)
        (values 0.0 0.0)
        (values (/ dx len) (/ dy len)))))

(defmethod curve-first-deriv ((p <point-2d>) param)
  (declare (ignore param))
  (multiple-value-bind (nx ny) (%normalize 0.0 0.0)
    (make-instance '<point-2d> :x nx :y ny)))

(defmethod curve-first-deriv ((seg <line-2d>) param)
  (declare (ignore param))
  (let* ((dx (- (x (<line-2d>-end-point seg)) (x (<line-2d>-start-point seg))))
         (dy (- (y (<line-2d>-end-point seg)) (y (<line-2d>-start-point seg)))))
    (multiple-value-bind (nx ny) (%normalize dx dy)
      (make-instance '<point-2d> :x nx :y ny))))

(defmethod curve-first-deriv ((circ <circle-2d>) param)
  (let* ((angle (%clamp param (curve-start-param circ) (curve-end-param circ)))
         (nx (- (sin angle)))
         (ny (cos angle)))
    (multiple-value-bind (tx ty) (%normalize nx ny)
      (make-instance '<point-2d> :x tx :y ty))))

(defmethod curve-first-deriv ((a <arc-2d>) param)
  (let* ((start (curve-start-param a))
         (end (curve-end-param a))
         (angle (%clamp param (min start end) (max start end)))
         (nx (- (sin angle)))
         (ny (cos angle)))
    (multiple-value-bind (tx ty) (%normalize nx ny)
      (make-instance '<point-2d> :x tx :y ty))))
