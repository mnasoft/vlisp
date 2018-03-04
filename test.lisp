;;;; test.lisp

(in-package #:vlisp)

;;;;  (load-vlisp-file "./src/lsp/utils/draw.lsp")
(progn 
  (load-vlisp-file "./bin/Axis.VLX")
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
  (dr-text          "New text !!!" '( 5 15) 4.5 (/ pi 3) 4)
  (dr-solid        '( 5 15) '(30 20) '( 5 -15) '(-30 20) 56)
  (dr-spline      '(( 5 15) (30 20) ( 5 -15) (-30 20)) 86)
  (dr-layer-new "New-layer")
  (dr-layer-set "New-layer")
  (dr-insert '(56 89) "rm-02")

  )


