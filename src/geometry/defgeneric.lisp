;;;; src/geometry/defgeneric.lisp
;;;; Общие определения defgeneric для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defgeneric distance (obj1 obj2)
  (:documentation "Вычисляет расстояние между двумя объектами"))

(defgeneric curve-start-param (obj)
  (:documentation "Возвращает начальный параметр кривой."))

(defgeneric curve-end-param (obj)
  (:documentation "Возвращает конечный параметр кривой."))

(defgeneric curve-start-point (obj)
  (:documentation "Возвращает точку кривой для её начального параметра."))

(defgeneric curve-end-point (obj)
  (:documentation "Возвращает точку кривой для её конечного параметра."))

(defgeneric curve-dist-at-param (obj param)
  (:documentation "Длина кривой от её начального параметра до указанного значения PARAM (с ограничением PARAM в допустимый диапазон)."))

(defgeneric curve-point-at-param (obj param)
  (:documentation "Возвращает точку кривой для заданного параметра PARAM."))

(defgeneric curve-first-deriv (obj param)
  (:documentation "Нормализованный касательный вектор кривой в параметре PARAM."))

(defgeneric curve-second-deriv (obj param)
  (:documentation "Вектор левой нормали длиной, равной радиусу кривизны в параметре PARAM; возвращает NIL, если не определено (например, для точки или отрезка)."))

(defgeneric curve-curvature (obj param)
  (:documentation "Кривизна κ в параметре PARAM; для линии 0.0, для точки NIL."))

(defgeneric curve-normal-at-param (obj param &key side)
  (:documentation "Единичная нормаль кривой в параметре PARAM; ключ :side (:left по умолчанию, :right для правой нормали)."))
