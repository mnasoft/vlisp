;;;; src/geometry/package.lisp
;;;; Пакет для работы с 2D геометрическими примитивами

(defpackage #:vlisp/geometry
  (:use #:cl)
  (:export
   ;; Классы примитивов
   #:<geometric-object>
   #:<point-2d>
   #:<line-2d>
   #:<circle-2d>
   #:<arc-2d>
   
   ;; Аксессоры для <point-2d>
   #:x
   #:y
   
   ;; Аксессоры для <line-2d>
   #:<line-2d>-start-point
   #:<line-2d>-end-point
   
   ;; Аксессоры для <circle-2d>
   #:center
   #:radius
   
   ;; Аксессоры для <arc-2d>
   #:arc-center
   #:arc-radius
   #:start-angle
   #:end-angle
   
   ;; Методы
   #:distance
   #:perimeter
   #:area))

(in-package #:vlisp/geometry)
