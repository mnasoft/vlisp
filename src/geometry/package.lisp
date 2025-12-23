;;;; src/geometry/package.lisp
;;;; Пакет для работы с 2D геометрическими примитивами

(defpackage #:vlisp/geometry
  (:use #:cl)
  (:export
   ;; Классы примитивов
   #:<geometric-object>
   #:<point-2d>
   #:<line-segment>
   #:<circle-2d>
   #:<arc-2d>
   
   ;; Аксессоры для <point-2d>
   #:<point-2d>-x
   #:<point-2d>-y
   
   ;; Аксессоры для <line-segment>
   #:<line-segment>-start-point
   #:<line-segment>-end-point
   
   ;; Аксессоры для <circle-2d>
   #:<circle-2d>-center
   #:<circle-2d>-radius
   
   ;; Аксессоры для <arc-2d>
   #:<arc-2d>-arc-center
   #:<arc-2d>-arc-radius
   #:<arc-2d>-start-angle
   #:<arc-2d>-end-angle
   
   ;; Методы
   #:distance
   #:perimeter
   #:area))

(in-package #:vlisp/geometry)
