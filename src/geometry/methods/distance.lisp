;;;; src/geometry/methods/distance.lisp
;;;; Методы расчёта расстояний для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defmethod distance ((p1 <point-2d>) (p2 <point-2d>))
  "Расстояние между двумя точками"
  (let ((dx (- (x p2) (x p1)))
        (dy (- (y p2) (y p1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod distance ((pt <point-2d>) (seg <line-2d>))
  "Расстояние от точки до отрезка"
  (let ((x0 (x pt))
        (y0 (y pt))
        (x1 (x (<line-2d>-start-point seg)))
        (y1 (y (<line-2d>-start-point seg)))
        (x2 (x (<line-2d>-end-point seg)))
        (y2 (y (<line-2d>-end-point seg))))
    ;; Параметр проекции точки на линию
    (let* ((dx (- x2 x1))
           (dy (- y2 y1))
           (len-sq (+ (* dx dx) (* dy dy))))
      (if (= len-sq 0)
          ;; Отрезок вырождается в точку
          (distance pt (<line-2d>-start-point seg))
          (let ((t-param (max 0 (min 1 (/ (+ (* (- x0 x1) dx) (* (- y0 y1) dy)) len-sq)))))
            ;; Точка на отрезке, ближайшая к (x0, y0)
            (let ((closest-x (+ x1 (* t-param dx)))
                  (closest-y (+ y1 (* t-param dy))))
              (sqrt (+ (expt (- x0 closest-x) 2) (expt (- y0 closest-y) 2)))))))))

(defmethod distance ((pt <point-2d>) (circ <circle-2d>))
  "Расстояние от точки до окружности"
  (abs (- (distance pt (<circle-2d>-center circ)) (<circle-2d>-radius circ))))
