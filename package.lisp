;;;; package.lisp

(defpackage #:vlisp )

(defpackage #:vlisp
  (:use #:cl)
;;;; (:export dotted-listp)
  (:export vlisp:*origin*)
  (:export vlisp:setvar)
  (:export vlisp:load-vlisp-file)
  
  (:export vlisp:axis-point-type-block-name
	   vlisp:axis-load-reset-point-types
	   vlisp:axis-point-type-reset
	   vlisp:axis-point-type-next
	   vlisp:axis-draw-point-set
	   vlisp:axis-draw-pline-set
	   vlisp:axis-draw-spline-set
	   vlisp:axis-block-scale-set
	   vlisp:axis-load-point-types
	   vlisp:axis-alert-mode-set
	   vlisp:axis-prompt-mode-set 
	   vlisp:axis-print-list
	   vlisp:axis-draw-multiple-graphs-by-axis-names)
  (:export vlisp:lines-load-line-types)
  (:export vlisp:dr-axis
	   vlisp:dr-ch_prop 
	   vlisp:dr-point
	   vlisp:dr-points
	   vlisp:dr-line
	   vlisp:dr-pline
	   vlisp:dr-circle
	   vlisp:dr-arc
	   vlisp:dr-xline
	   vlisp:dr-ray
	   vlisp:dr-text
	   vlisp:dr-solid 
	   vlisp:dr-spline
	   vlisp:dr-layer-new
	   vlisp:dr-layer-set
	   vlisp:dr-insert
	   vlisp:dr-mtext)
  
  (:export vlisp:dr-rect
	   vlisp:dr-format-a4
	   vlisp:dr-format-a3)
  
  (:export vlisp:vector-length
	   vlisp:vector+
	   vlisp:vector-
	   vlisp:normalize
	   vlisp:mid-point)
  (:export vlisp:angle
	   vlisp:distance
	   vlisp:inters
	   vlisp:polar
	   ))




(osnap pt mode)
Returns a 3D point that is the result of applying an Object Snap mode to a specified point
(polar pt ang dist)
Returns the UCS 3D point at a specified angle and distance from a point
(textbox elist)
Measures a specified text object, and returns the diagonal coordinates of a box that encloses the text
