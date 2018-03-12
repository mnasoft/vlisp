;;;; package.lisp

(defpackage #:vlisp
  (:use #:cl)
;;;; (:export dotted-listp)
  (:export *origin*)
  (:export setvar)
  (:export load-vlisp-file)
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
	   axis-draw-multiple-graphs-by-axis-names)
  (:export lines-load-line-types)
  (:export dr-axis
	   dr-ch_prop 
	   dr-point
	   dr-points
	   dr-line
	   dr-pline
	   dr-circle
	   dr-arc
	   dr-xline
	   dr-ray
	   dr-text
	   dr-solid 
	   dr-spline
	   dr-layer-new
	   dr-layer-set
	   dr-insert
	   dr-mtext)
  (:export dr-rect
	   dr-format-a4
	   dr-format-a3)
  (:export vector-length
	   vector+
	   vector-
	   normalize
	   mid-point
	   distance
	   polar
	   angle))




