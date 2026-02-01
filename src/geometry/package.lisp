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
   #:<circle-2d>-center
   #:<circle-2d>-radius
   
   ;; Аксессоры для <arc-2d>
   #:<arc-2d>-center
   #:<arc-2d>-radius
   #:<arc-2d>-start-angle
   #:<arc-2d>-end-angle
   
   ;; Методы
   #:distance
   #:perimeter
   #:area
   #:curve-start-param
   #:curve-end-param
   #:curve-start-point
   #:curve-end-point
   #:curve-dist-at-param
   #:curve-point-at-param
   #:curve-first-deriv
   #:curve-second-deriv
   #:curve-curvature
   #:curve-normal-at-param))

(in-package #:vlisp/geometry)
