;;;; package.lisp

(defpackage #:vlisp
  (:use #:cl)
  (:export   load-vlisp-file
;;;; axis
    	     dr-axis
	     axis-load-point-types
             axis-draw-multiple-graphs-by-axis-names
;;;; draw	     
	     dr-point       
	     dr-ch_prop     
	     dr-points      
	     dr-pline       
	     dr-axis        
	     dr-line        
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

	     ))
