;;;; package.lisp

(defpackage #:vlisp
  (:use #:cl)
  (:export *origin*
	   )
  (:export dotted-listp
           )
  (:export setvar
	   )
  (:export load-vlisp-file
	   )
  (:export lines-load-line-types)
  (:export vector-length
	   vector+
	   vector-
	   normalize
	   mid-point
           vector-dot-product
           vector-cross-product
	   )
  (:export angle
	   distance
	   inters
	   polar
	   )
  (:export load-vlisp-apps
    
           )
  (:export 
   osnap
   textbox
   ))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
