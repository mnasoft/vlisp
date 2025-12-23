;;;; src/geometry/primitives.lisp
;;;; Классы для 2D геометрических примитивов

(in-package #:vlisp/geometry)

;;; Базовый класс для всех геометрических объектов
(defclass <geometric-object> ()
  ()
  (:documentation "Базовый класс для всех геометрических примитивов"))

;;; Точка в 2D пространстве
(defclass <point-2d> (<geometric-object>)
  ((x :initarg :x
       :accessor x
       :type number
       :documentation "X-координата")
   (y :initarg :y
       :accessor y
       :type number
       :documentation "Y-координата"))
  (:documentation "Точка в двумерном пространстве"))

;;; Отрезок линии
(defclass <line-2d> (<geometric-object>)
  ((start-point :initarg :start-point
                :accessor <line-2d>-start-point
                :type <point-2d>
                :documentation "Начальная точка отрезка")
   (end-point :initarg :end-point
              :accessor <line-2d>-end-point
              :type <point-2d>
              :documentation "Конечная точка отрезка"))
  (:documentation "Отрезок прямой в двумерном пространстве"))

;;; Окружность
(defclass <circle-2d> (<geometric-object>)
  ((center :initarg :center
           :accessor <circle-2d>-center
           :type <point-2d>
           :documentation "Центр окружности")
   (radius :initarg :radius
           :accessor <circle-2d>-radius
           :type number
           :documentation "Радиус окружности"))
  (:documentation "Окружность в двумерном пространстве"))

;;; Дуга
(defclass <arc-2d> (<geometric-object>)
  ((center :initarg :center
           :accessor <arc-2d>-center
           :type <point-2d>
           :documentation "Центр дуги")
   (radius :initarg :radius
           :accessor <arc-2d>-radius
           :type number
           :documentation "Радиус дуги")
   (start-angle :initarg :start-angle
                :accessor <arc-2d>-start-angle
                :type number
                :documentation "Начальный угол в радианах")
   (end-angle :initarg :end-angle
              :accessor <arc-2d>-end-angle
              :type number
              :documentation "Конечный угол в радианах"))
  (:documentation "Дуга окружности в двумерном пространстве"))
