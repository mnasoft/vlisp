;;;; src/geometry/package.lisp
;;;; Пакет для работы с 2D геометрическими примитивами

(defpackage #:vlisp/geometry
  (:use #:cl)
  (:export
   ;; Классы примитивов
   #:geometric-object
   #:point-2d
   #:line-segment
   #:circle
   #:arc
   
   ;; Слоты для point-2d
   #:x
   #:y
   
   ;; Слоты для line-segment
   #:start-point
   #:end-point
   
   ;; Слоты для circle
   #:center
   #:radius
   
   ;; Слоты для arc
   #:arc-center
   #:arc-radius
   #:start-angle
   #:end-angle
   
   ;; Методы
   #:distance
   #:perimeter
   #:area))

(in-package #:vlisp/geometry)
