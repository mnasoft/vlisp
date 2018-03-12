;;;; test.lisp

(in-package #:vlisp)

;;;;  (load-vlisp-file "./src/lsp/utils/draw.lsp")
(progn 
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/dim_style.VLX")
  (load-vlisp-file "./bin/lines.VLX")
  
  (axis-block-scale-set 2.5)
  (axis-load-point-types)
  (dr-point        '(10 20 30) 256 )
  (dr-ch_prop      '((62 . 5)))
  (dr-points       '((0 0 0) (10 10 10 )) 1)
  (dr-pline        '((0 0 0) (10 10 10 )) 1)
  (dr-axis         '(0 0 0) '(0 100 0) 0 100 0 "COOL_axis")
  (dr-line         '( 0  0) '(100 100) 6)
  (dr-circle       '(20 30)   25.3 2)
  (dr-arc          '(30 20)   25.3 pi (* 3/4 pi)  2 )
  (dr-xline        '( 5 15) '(30 20) 3)
  (dr-ray          '( 5 15) '(30 20) 4)
  (dr-text          "New text !!!" '( 5 15) 4.5 0.0 54  :alignment 7 )
  (dr-solid        '( 5 15) '(30 20) '( 5 -15) '(-30 20) 56)
  (dr-spline      '(( 5 15) (30 20) ( 5 -15) (-30 20)) 86)
  (dr-layer-new "New-layer")
  (dr-layer-set "New-layer")
  (dr-insert '(56 89) "123" 1.0 1.0 1.0 0.0)




  (dr-mtext "Sample text sample text sample text sample text sample text sample text sample text sample text " '(10 10 ) 100 3.15 0.0 54 2)
  
  )

(defun test-axis-draw-points ()
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/dim_style.VLX")
  (axis-load-reset-point-types)
  (let ((start-point (vector+ (mid-point (vector+ *origin* '(20 5 0))
					 (vector- '(210 297 0) '(5 5 0)))
			      '(-50 287/4 0)))
	(delta-x 10)
	(delta-y 10)
	(scale 2.0))
    (dr-format-a4 '(0 0 0))
    (dotimes (j 5)
      (dotimes (i 10)
	(axis-point-type-next)
	(dr-insert
	 (mapcar #'+ start-point (list (* i delta-x) (* j delta-y) 0))
	 (axis-point-type-block-name :os nil) scale scale scale 0.0
	 :format-sting "(dr:insert '(~{~f~^ ~}) ~a ~f ~f ~f ~f)~%")))))

(test-axis-draw-points) 

(defun test-line ()
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/lines.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (lines-load-line-types)
  (let* ((lt-lst (list "Continuous"
		       "CENTER" "CENTER2" "CENTER4" "CENTERX2" "CENTERX4"
		       "HIDDEN" "HIDDEN2" "HIDDEN4" "HIDDENX2" "HIDDENX4"
		       "DASHDOT" "DASHDOT2" "DASHDOT4" "DASHDOTX2" "DASHDOTX4"))
	 (length 150)
	 (delta-y 10)
	 (start-point (vector+ (list (/ length -2) -50 0 )
			       (mid-point (vector+ *origin* '(20 5 0))
					  (vector+ *origin* '(205 292 0))))))
    (mapcar #'(lambda (lt)
		(setvar "CELTYPE" lt)
		(dr-line start-point (polar start-point 0 length) 256)
		(setf start-point (vector+ start-point (list 0  delta-y 0))))
	    lt-lst)))

(test-line)

(defun test-point ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((start-point (mid-point (vector+ *origin* '(20 5 0))
					 (vector+ *origin* '(205 292 0))))
	(x-max 150)
	(y-max 200))
  (dotimes (i 100)
    (dr-point (vector+ start-point
			  (vector- (list (random (1+ x-max)) (random (1+ y-max)) 0)
				   (list (/ x-max 2) (/ y-max 2) 0)))
	      (1+ (random 7))))
  (setvar "PDMODE" 35)))

(test-point)

(defun test-points ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((start-point (mid-point (vector+ *origin* '(20 5 0))
				(vector+ *origin* '(205 292 0))))
	(x-max 150)
	(y-max 200)
	(pts nil))
    (dotimes (i 100)
      (push (vector+ start-point
		     (vector- (list (random (1+ x-max)) (random (1+ y-max)) 0)
			      (list (/ x-max 2) (/ y-max 2) 0)))
	    pts))
    (dr-points pts (1+ (random 7)))
    (setvar "PDMODE" 35)))

(test-points)

(defun test-arc-circle ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((left-point '(-20 0 0))
	(right-point '(20 0 0))
	(start-point (mid-point (vector+ *origin* '(20 5 0))
				(vector+ *origin* '(205 292 0))))
	(r-min 20)
	(delta-r 5))
    (dotimes (i 8)
      (dr-circle (vector+ start-point left-point ) (+ r-min (* i delta-r))   (1+ (random 7)))
      (dr-arc (vector+ start-point right-point) (+ r-min (* i delta-r)) (/ (random 10000) 1000) (/ (random 10000) 1000)  (1+ (random 7))))))

(test-circle)

(defun test-axis ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((dy   '(40 135) )
	(dx   '(40 110) )
	)
    (dr-axis (vector+ dx  '(0 0)) (vector+ dx '(150 0))     0 10 0 "x")
    (dr-axis (vector+ dy  '(0 0)) (vector+ dy '( 0 150))    0 10 0 "x1")
    (dr-axis (vector+ dy '(-5 0)) (vector+ dy '(-5 150))    0 10 0 "x2")
    (dr-axis (vector+ dy '(-10 0)) (vector+ dy '(-10 150))  0 8 0 "x3")
    (axis-draw-pline-set nil)
					;    (axis-draw-spline-set nil)
    (axis-draw-point-set t)
    (axis-draw-multiple-graphs-by-axis-names
     "x" '(1 2 3 4 5 6 7 8 9)
     '("x1" "x2" "x3")
     '((0.1 0.4 0.9 1.6 2.5 3.6 4.9 6.4 8.1)
       (0.01 0.08 0.27 0.64 1.25 2.16 3.43 5.12 7.29)
       (0.1 0.4 0.9 1.6 2.5 3.6 4.9 6.4 8.1)))))

(test-axis)
