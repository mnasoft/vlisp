;;;; src/geometry/methods/curve-dist-at-param.lisp
;;;; Длина кривой до указанного параметра

(in-package #:vlisp/geometry)

(defun %clamp (value min-value max-value)
  (max min-value (min max-value value)))

(defmethod curve-dist-at-param ((p <point-2d>) param)
  (declare (ignore param))
  0.0)

(defmethod curve-dist-at-param ((seg <line-2d>) param)
  (let* ((tau (%clamp param (curve-start-param seg) (curve-end-param seg)))
         (total (perimeter seg)))
    (* total tau)))

(defmethod curve-dist-at-param ((circ <circle-2d>) param)
  (let* ((p (%clamp param (curve-start-param circ) (curve-end-param circ)))
         (r (<circle-2d>-radius circ)))
    (* r p)))

(defmethod curve-dist-at-param ((a <arc-2d>) param)
    (let* ((start (curve-start-param a))
      (end (curve-end-param a))
         (p (%clamp param (min start end) (max start end)))
         (r (<arc-2d>-radius a)))
    (* r (abs (- p start)))))
