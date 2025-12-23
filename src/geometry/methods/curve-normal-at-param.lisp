;;;; src/geometry/methods/curve-normal-at-param.lisp
;;;; Единичная нормаль кривой в параметре с выбором стороны

(in-package #:vlisp/geometry)

(defun %clamp (value min-value max-value)
  (max min-value (min max-value value)))

(defun %normalize (dx dy)
  (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (<= len 0.0)
        (values 0.0 0.0)
        (values (/ dx len) (/ dy len)))))

(defun %apply-side (nx ny side)
  (ecase side
    (:left (values nx ny))
    (:right (values (- nx) (- ny)))))

(defmethod curve-normal-at-param ((p <point-2d>) param &key (side :left))
  (declare (ignore param side))
  ;; Не определена
  nil)

(defmethod curve-normal-at-param ((seg <line-2d>) param &key (side :left))
  (declare (ignore param))
  (multiple-value-bind (tx ty)
      (%normalize (- (x (<line-2d>-end-point seg)) (x (<line-2d>-start-point seg)))
                  (- (y (<line-2d>-end-point seg)) (y (<line-2d>-start-point seg))))
    (multiple-value-bind (nx ny) (%apply-side (- ty) tx side)
      (make-instance '<point-2d> :x nx :y ny))) )

(defmethod curve-normal-at-param ((circ <circle-2d>) param &key (side :left))
  (let* ((tau (%clamp param (curve-start-param circ) (curve-end-param circ)))
         ;; Левая нормаль — внутрь
         (nx (- (cos tau)))
         (ny (- (sin tau))))
    (multiple-value-bind (ux uy) (%apply-side nx ny side)
      (multiple-value-bind (nx1 ny1) (%normalize ux uy)
        (make-instance '<point-2d> :x nx1 :y ny1)))))

(defmethod curve-normal-at-param ((a <arc-2d>) param &key (side :left))
  (let* ((s (curve-start-param a))
         (e (curve-end-param a))
         (tau (%clamp param (min s e) (max s e)))
         (nx (- (cos tau)))
         (ny (- (sin tau))))
    (multiple-value-bind (ux uy) (%apply-side nx ny side)
      (multiple-value-bind (nx1 ny1) (%normalize ux uy)
        (make-instance '<point-2d> :x nx1 :y ny1)))))
