;;;; ./src/axis/axis.lisp

(defpackage #:vlisp/axis
  (:use #:cl)
  (:export axis-point-type-block-name
	   axis-load-reset-point-types
	   axis-point-type-reset
	   axis-point-type-next
	   axis-draw-point-set
	   axis-draw-pline-set
	   axis-draw-spline-set
	   axis-block-scale-set
	   axis-load-point-types
	   axis-alert-mode-set
	   axis-prompt-mode-set 
	   axis-print-list
	   axis-draw-multiple-graphs-by-axis-names))

(in-package #:vlisp/axis)

(defun axis-point-type-block-name (&key (os *standard-output*))
  (format os "(axis:point-type-block-name)"))

(defun axis-load-reset-point-types (&key (os *standard-output*))
  (format os "(axis:load-reset-point-types)~%"))

(defun axis-point-type-reset (&key (os *standard-output*))
  (format os "(axis:point-type-reset)~%"))

(defun axis-point-type-next (&key (os *standard-output*))
  (format os "(axis:point-type-next)~%"))
  
(defun axis-draw-point-set (flag &key (os *standard-output*))
  (format os "(mnas-axis:draw-point-set ~a)~%" flag))

(defun axis-draw-pline-set (flag &key (os *standard-output*))
  (format os "(mnas-axis:draw-pline-set ~a)~%" flag))

(defun axis-draw-spline-set (flag &key (os *standard-output*))
  (format os "(mnas-axis:draw-spline-set ~a)~%" flag))

(defun axis-block-scale-set (xyz-scale &key (os *standard-output*))
  (format os "(mnas-axis:block-scale-set ~f)~%" xyz-scale))

(defun axis-load-point-types (&key (os *standard-output*))
  (format os "(axis:load-point-types)~%"))


