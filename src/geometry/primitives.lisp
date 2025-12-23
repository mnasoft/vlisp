;;;; src/geometry/primitives.lisp
;;;; Классы для 2D геометрических примитивов

(in-package #:vlisp/geometry)

;;; Базовый класс для всех геометрических объектов
(defclass <geometric-object> ()
  ()
  (:documentation "Базовый класс для всех геометрических примитивов"))

;;; Точка в 2D пространстве
(defclass <point-2d> (<geometric-object>)
  ((<point-2d>-x :initarg :x
                  :accessor <point-2d>-x
                  :type number
                  :documentation "X-координата")
   (<point-2d>-y :initarg :y
                  :accessor <point-2d>-y
                  :type number
                  :documentation "Y-координата"))
  (:documentation "Точка в двумерном пространстве"))

(defmethod print-object ((pt <point-2d>) stream)
  (print-unreadable-object (pt stream :type t :identity nil)
    (format stream "~F ~F" (<point-2d>-x pt) (<point-2d>-y pt))))

;;; Отрезок линии
(defclass <line-segment> (<geometric-object>)
  ((<line-segment>-start-point :initarg :start-point
                                :accessor <line-segment>-start-point
                                :type <point-2d>
                                :documentation "Начальная точка отрезка")
   (<line-segment>-end-point :initarg :end-point
                              :accessor <line-segment>-end-point
                              :type <point-2d>
                              :documentation "Конечная точка отрезка"))
  (:documentation "Отрезок прямой в двумерном пространстве"))

(defmethod print-object ((seg <line-segment>) stream)
  (print-unreadable-object (seg stream :type t :identity nil)
    (format stream "(~F ~F) -> (~F ~F)"
            (<point-2d>-x (<line-segment>-start-point seg))
            (<point-2d>-y (<line-segment>-start-point seg))
            (<point-2d>-x (<line-segment>-end-point seg))
            (<point-2d>-y (<line-segment>-end-point seg)))))

(defmethod perimeter ((seg <line-segment>))
  "Возвращает длину отрезка"
  (let ((dx (- (<point-2d>-x (<line-segment>-end-point seg)) (<point-2d>-x (<line-segment>-start-point seg))))
        (dy (- (<point-2d>-y (<line-segment>-end-point seg)) (<point-2d>-y (<line-segment>-start-point seg)))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;; Окружность
(defclass <circle-2d> (<geometric-object>)
  ((<circle-2d>-center :initarg :center
                       :accessor <circle-2d>-center
                       :type <point-2d>
                       :documentation "Центр окружности")
   (<circle-2d>-radius :initarg :radius
                       :accessor <circle-2d>-radius
                       :type number
                       :documentation "Радиус окружности"))
  (:documentation "Окружность в двумерном пространстве"))

(defmethod print-object ((circ <circle-2d>) stream)
  (print-unreadable-object (circ stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F"
            (<point-2d>-x (<circle-2d>-center circ))
            (<point-2d>-y (<circle-2d>-center circ))
            (<circle-2d>-radius circ))))

(defmethod perimeter ((circ <circle-2d>))
  "Возвращает длину окружности (периметр)"
  (* 2 pi (<circle-2d>-radius circ)))

(defmethod area ((circ <circle-2d>))
  "Возвращает площадь окружности"
  (* pi (<circle-2d>-radius circ) (<circle-2d>-radius circ)))

;;; Дуга
(defclass <arc-2d> (<geometric-object>)
  ((<arc-2d>-arc-center :initarg :arc-center
                         :accessor <arc-2d>-arc-center
                         :type <point-2d>
                         :documentation "Центр дуги")
   (<arc-2d>-arc-radius :initarg :arc-radius
                         :accessor <arc-2d>-arc-radius
                         :type number
                         :documentation "Радиус дуги")
   (<arc-2d>-start-angle :initarg :start-angle
                          :accessor <arc-2d>-start-angle
                          :type number
                          :documentation "Начальный угол в радианах")
   (<arc-2d>-end-angle :initarg :end-angle
                        :accessor <arc-2d>-end-angle
                        :type number
                        :documentation "Конечный угол в радианах"))
  (:documentation "Дуга окружности в двумерном пространстве"))

(defmethod print-object ((a <arc-2d>) stream)
  (print-unreadable-object (a stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F angles:[~F ~F]"
            (<point-2d>-x (<arc-2d>-arc-center a))
            (<point-2d>-y (<arc-2d>-arc-center a))
            (<arc-2d>-arc-radius a)
            (<arc-2d>-start-angle a)
            (<arc-2d>-end-angle a))))

(defmethod perimeter ((a <arc-2d>))
  "Возвращает длину дуги"
  (let* ((angle-diff (abs (- (<arc-2d>-end-angle a) (<arc-2d>-start-angle a))))
         ;; Используем наименьший угол между начальным и конечным
         (angle (min angle-diff (- (* 2 pi) angle-diff))))
    (* (<arc-2d>-arc-radius a) angle)))

;;; Вспомогательные функции

(defgeneric distance (obj1 obj2)
  (:documentation "Вычисляет расстояние между двумя объектами"))

(defmethod distance ((p1 <point-2d>) (p2 <point-2d>))
  "Расстояние между двумя точками"
  (let ((dx (- (<point-2d>-x p2) (<point-2d>-x p1)))
        (dy (- (<point-2d>-y p2) (<point-2d>-y p1))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defmethod distance ((pt <point-2d>) (seg <line-segment>))
  "Расстояние от точки до отрезка"
  (let ((x0 (<point-2d>-x pt))
        (y0 (<point-2d>-y pt))
        (x1 (<point-2d>-x (<line-segment>-start-point seg)))
        (y1 (<point-2d>-y (<line-segment>-start-point seg)))
        (x2 (<point-2d>-x (<line-segment>-end-point seg)))
        (y2 (<point-2d>-y (<line-segment>-end-point seg))))
    ;; Параметр проекции точки на линию
    (let* ((dx (- x2 x1))
           (dy (- y2 y1))
           (len-sq (+ (* dx dx) (* dy dy))))
      (if (= len-sq 0)
          ;; Отрезок вырождается в точку
          (distance pt (<line-segment>-start-point seg))
          (let ((t-param (max 0 (min 1 (/ (+ (* (- x0 x1) dx) (* (- y0 y1) dy)) len-sq)))))
            ;; Точка на отрезке, ближайшая к (x0, y0)
            (let ((closest-x (+ x1 (* t-param dx)))
                  (closest-y (+ y1 (* t-param dy))))
              (sqrt (+ (expt (- x0 closest-x) 2) (expt (- y0 closest-y) 2)))))))))

(defmethod distance ((pt <point-2d>) (circ <circle-2d>))
  "Расстояние от точки до окружности"
  (abs (- (distance pt (<circle-2d>-center circ)) (<circle-2d>-radius circ))))
