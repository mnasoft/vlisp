;;;; src/geometry/methods/print-object.lisp
;;;; Методы печати для 2D геометрических примитивов

(in-package #:vlisp/geometry)

(defmethod print-object ((pt <point-2d>) stream)
  (print-unreadable-object (pt stream :type t :identity nil)
    (format stream "~F ~F" (x pt) (y pt))))

(defmethod print-object ((seg <line-2d>) stream)
  (print-unreadable-object (seg stream :type t :identity nil)
    (format stream "(~F ~F) -> (~F ~F)"
            (x (<line-2d>-start-point seg))
            (y (<line-2d>-start-point seg))
            (x (<line-2d>-end-point seg))
            (y (<line-2d>-end-point seg)))))

(defmethod print-object ((circ <circle-2d>) stream)
  (print-unreadable-object (circ stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F"
            (x (<circle-2d>-center circ))
            (y (<circle-2d>-center circ))
            (<circle-2d>-radius circ))))

(defmethod print-object ((a <arc-2d>) stream)
  (print-unreadable-object (a stream :type t :identity nil)
    (format stream "center:(~F ~F) radius:~F angles:[~F ~F]"
            (x (<arc-2d>-center a))
            (y (<arc-2d>-center a))
            (<arc-2d>-radius a)
            (<arc-2d>-start-angle a)
            (<arc-2d>-end-angle a))))
