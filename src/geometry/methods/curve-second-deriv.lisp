;;;; src/geometry/methods/curve-second-deriv.lisp
;;;; Нормализованный вектор нормали (левая нормаль) в параметре

(in-package #:vlisp/geometry)

(defun %clamp (value min-value max-value)
  (max min-value (min max-value value)))

(defun %normalize (dx dy)
  (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (<= len 0.0)
        (values 0.0 0.0)
        (values (/ dx len) (/ dy len)))))

(defmethod curve-second-deriv ((p <point-2d>) param)
  (declare (ignore param))
  nil)

(defmethod curve-second-deriv ((seg <line-2d>) param)
  (declare (ignore param))
  nil)

(defmethod curve-second-deriv ((circ <circle-2d>) param)
  (let* ((tau (%clamp param (curve-start-param circ) (curve-end-param circ)))
         (r (<circle-2d>-radius circ))
         (vx (* r (- (cos tau))))
         (vy (* r (- (sin tau)))))
    (make-instance '<point-2d> :x vx :y vy)))

(defmethod curve-second-deriv ((a <arc-2d>) param)
    (let* ((s (curve-start-param a))
      (e (curve-end-param a))
      (tau (%clamp param (min s e) (max s e)))
      (r (<arc-2d>-radius a))
      (vx (* r (- (cos tau))))
      (vy (* r (- (sin tau)))))
    (make-instance '<point-2d> :x vx :y vy)))
