;;;; src/geometry/primitives.lisp
;;;; Классы для 2D геометрических примитивов

(in-package #:vlisp/geometry)

;;; Базовый класс для всех геометрических объектов
(defclass geometric-object ()
  ()
  (:documentation "Базовый класс для всех геометрических примитивов"))

;;; Точка в 2D пространстве
(defclass point-2d (geometric-object)
  ((x :initarg :x
      :accessor x
      :type number
      :documentation "X-координата")
   (y :initarg :y
      :accessor y
      :type number
      :documentation "Y-координата"))
  (:documentation "Точка в двумерном пространстве"))

(defmethod print-object ((pt point-2d) stream)
  (print-unreadable-object (pt stream :type t :identity nil)
    (format stream "~F ~F" (x pt) (y pt))))

;;; Отрезок линии
(defclass line-segment (geometric-object)
  ((start-point :initarg :start-point
                :accessor start-point
                :type point-2d
                :documentation "Начальная точка отрезка")
   (end-point :initarg :end-point
              :accessor end-point
              :type point-2d
              :documentation "Конечная точка отрезка"))
  (:documentation "Отрезок прямой в двумерном пространстве"))

(defmethod print-object ((seg line-segment) stream)
  (print-unreadable-object (seg stream :type t :identity nil)
    (format stream "(~F ~F) -> (~F ~F)"
            (x (start-point seg))
            (y (start-point seg))
            (x (end-point seg))
            (y (end-point seg)))))

(defmethod perimeter ((seg line-segment))
  "Возвращает длину отрезка"
  (let ((dx (- (x (end-point seg)) (x (start-point seg))))
        (dy (- (y (end-point seg)) (y (start-point seg)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; Окружность
(defclass circle (geometric-object)
  ((center :initarg :center
           :accessor center
           :type point-2d
           :documentation "Центр окружности")
   (radius :initarg :radius
           :accessor radius
           :type number
           :documentation "Радиус окружности"))
  (:documentation "Окружность в двумерном пространстве"))

(defmethod print-object ((circ circle) stream)
  (print-unreadable-object (circ stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F"
            (x (center circ))
            (y (center circ))
            (radius circ))))

(defmethod perimeter ((circ circle))
  "Возвращает длину окружности (периметр)"
  (* 2 pi (radius circ)))

(defmethod area ((circ circle))
  "Возвращает площадь окружности"
  (* pi (radius circ) (radius circ)))

;;; Дуга
(defclass arc (geometric-object)
  ((arc-center :initarg :arc-center
               :accessor arc-center
               :type point-2d
               :documentation "Центр дуги")
   (arc-radius :initarg :arc-radius
               :accessor arc-radius
               :type number
               :documentation "Радиус дуги")
   (start-angle :initarg :start-angle
                :accessor start-angle
                :type number
                :documentation "Начальный угол в радианах")
   (end-angle :initarg :end-angle
              :accessor end-angle
              :type number
              :documentation "Конечный угол в радианах"))
  (:documentation "Дуга окружности в двумерном пространстве"))

(defmethod print-object ((a arc) stream)
  (print-unreadable-object (a stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F angles:[~F ~F]"
            (x (arc-center a))
            (y (arc-center a))
            (arc-radius a)
            (start-angle a)
            (end-angle a))))

(defmethod perimeter ((a arc))
  "Возвращает длину дуги"
  (let* ((angle-diff (abs (- (end-angle a) (start-angle a))))
         ;; Используем наименьший угол между начальным и конечным
         (angle (min angle-diff (- (* 2 pi) angle-diff))))
    (* (arc-radius a) angle)))

;;; Вспомогательные функции

(defgeneric distance (obj1 obj2)
  (:documentation "Вычисляет расстояние между двумя объектами"))

(defmethod distance ((p1 point-2d) (p2 point-2d))
  "Расстояние между двумя точками"
  (let ((dx (- (x p2) (x p1)))
        (dy (- (y p2) (y p1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod distance ((pt point-2d) (seg line-segment))
  "Расстояние от точки до отрезка"
  (let ((x0 (x pt))
        (y0 (y pt))
        (x1 (x (start-point seg)))
        (y1 (y (start-point seg)))
        (x2 (x (end-point seg)))
        (y2 (y (end-point seg))))
    ;; Параметр проекции точки на линию
    (let* ((dx (- x2 x1))
           (dy (- y2 y1))
           (len-sq (+ (* dx dx) (* dy dy))))
      (if (= len-sq 0)
          ;; Отрезок вырождается в точку
          (distance pt (start-point seg))
          (let ((t-param (max 0 (min 1 (/ (+ (* (- x0 x1) dx) (* (- y0 y1) dy)) len-sq)))))
            ;; Точка на отрезке, ближайшая к (x0, y0)
            (let ((closest-x (+ x1 (* t-param dx)))
                  (closest-y (+ y1 (* t-param dy))))
              (sqrt (+ (expt (- x0 closest-x) 2) (expt (- y0 closest-y) 2)))))))))

(defmethod distance ((pt point-2d) (circ circle))
  "Расстояние от точки до окружности"
  (abs (- (distance pt (center circ)) (radius circ))))
