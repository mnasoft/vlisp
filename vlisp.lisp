
;;;; vlisp.lisp

(in-package #:vlisp)

(defun dotted-listp (lst)
  (not (listp (cdr (last lst)))))

(export 'dotted-listp)

(defun load-vlisp-file (fname &key (os *standard-output*))
  (format os "(load (findfile ~s))~%" fname))

(export 'load-vlisp-file)

(defun load-vlisp-apps (&key (os *standard-output*) (apps '("./bin/Axis.VLX" "./bin/dim_style.VLX" "./bin/lines.VLX")))
  (mapc #'(lambda (app) (vlisp:load-vlisp-file app  :os os)) apps))

(export 'load-vlisp-apps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun dr-axis (start-pt end-pt start-value end-value flag name &key (os *standard-output*) (caption "") (dimension ""))
  (format os "(dr:axis '(~{~f~^ ~}) '(~{~f~^ ~}) ~f ~f ~d ~s ~s ~s)~%" start-pt end-pt start-value end-value flag (mnas-string:translit name) caption dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dr-ch_prop  (property-list &key (os *standard-output*))
  (format os "(dr:ch_prop '(")
  (mapc
   #'(lambda (el)
       (let ((code (car el))
	     (data (cdr el)))
;	 (break "0001:")
	 (cond
	   ((and (<= 1 code 9) (dotted-listp el))
	    (format os "(~d . ~s)" code data))
	   ((and (<= 10 code 18) (not (dotted-listp el)))
	    (format os "(~d ~{~f~^ ~})" code data))
	   ((and (<= 38 code 58) (not (dotted-listp el)))
	    (format os "(~d ~{~f~^ ~})" code data))
	   ((and (<= 60 code 99)  (dotted-listp el))
	    (format os "(~d . ~d)" code data)))))
   property-list)
  (format os "))~%"))

(defun dr-point (point color &key (os *standard-output*))
  (format os "(dr:point '(~{~f~^ ~}) ~d)~%" point color))

(defun dr-points (points color &key (os *standard-output*))
  (format os "(dr:points '(")
  (mapc
   #'(lambda (el)
       (format os "(~{~f~^ ~})" el))
   points)
  (format os ") ~d)~%" color))

(defun dr-line (start-point  end-point color &key (os *standard-output*))
  (format os "(dr:line '(~{~f~^ ~}) '(~{~f~^ ~}) ~d)~%" start-point end-point color))

(defun dr-pline (points color &key (os *standard-output*))
  (format os "(dr:pline '(")
  (mapc
   #'(lambda (el)
       (format os "(~{~f~^ ~})" el))
   points)
  (format os ") ~d)~%" color))

(defun dr-circle (center-point radius color &key (os *standard-output*))
  (format os "(dr:circle '(~{~f~^ ~}) ~f ~d)~%" center-point radius color))

(defun dr-arc (center-point radius start-angle end-angle color &key (os *standard-output*))
  (format os "(dr:arc '(~{~f~^ ~}) ~f ~f ~f ~d)~%" center-point radius start-angle end-angle color))

(defun dr-xline (start-point end-point color &key (os *standard-output*))
    (format os "(dr:xline '(~{~f~^ ~}) '(~{~f~^ ~}) ~d)~%" start-point end-point color))

(defun dr-ray (start-point end-point color &key (os *standard-output*))
  (format os "(dr:ray '(~{~f~^ ~}) '(~{~f~^ ~}) ~d)~%" start-point end-point color))

(defun dr-text (text point height rotation color &key (Alignment 0) (os *standard-output*))
  "Alignment: 
 L= 0   C= 1   R= 2
Al= 3  CT= 4  Wi= 5
TL= 6  TC= 7  TR= 8
ML= 9  MC=10  MR=11
BL=12  BC=13  BR=14
"
  (format os "(dr:text ~s '(~{~f~^ ~}) ~f ~f ~d ~d)~%" text point height rotation color Alignment))

(defun dr-solid  (point1 point2 point3 point4 color &key (os *standard-output*))
  (format os "(dr:solid '(~{~f~^ ~}) '(~{~f~^ ~}) '(~{~f~^ ~}) '(~{~f~^ ~}) ~d)~%" point1 point2 point3 point4 color))

(defun dr-spline (points color &key (os *standard-output*))
  (format os "(dr:spline '(")
  (mapc
   #'(lambda (el)
       (format os "(~{~f~^ ~})" el))
   points)
  (format os ") ~d)~%" color))

(defun dr-layer-new (layer-name &key (os *standard-output*))
  (format os "(dr:layer-new ~s)~%" layer-name))

(defun dr-layer-set (layer-name &key (os *standard-output*))
    (format os "(dr:layer-set ~s)~%" layer-name))

(defun dr-insert (InsertionPoint Name Xscale Yscale ZScale Rotation &key (os *standard-output*) (format-sting "(dr:insert '(~{~f~^ ~}) ~s ~f ~f ~f ~f)~%")  )
  (format os format-sting InsertionPoint Name Xscale Yscale ZScale Rotation))

(defun dr-mtext  (InsertionPoint Width Text height rotation color AttachmentPoint &key (os *standard-output*) )
  "AttachmentPoint - 1 - LT; 2 - CT; 3 - RT"
  (format os "(dr:mtext '(~{~f~^ ~}) ~f ~s ~f ~f ~d ~d)~%" InsertionPoint Width Text height rotation color AttachmentPoint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *origin* '(0 0 0))

(defun vector-length (v)
  (sqrt (apply
	 #'+ (mapcar #'(lambda (el) (* el el))
		     v))))

(defun vector+ (point displasment)
  (mapcar #'+ point displasment))

(defun vector- (point displasment)
  (mapcar #'- point displasment))

(defun normalize (vector)
  (let ((length (vector-length vector)))
    (if (>  length 0)
	(mapcar #'(lambda (el) (/ el length)) vector)
	(error "(defun normalize (vector)...) vector-length <= 0 ")
	)))

(defun mid-point (point1 point2)
  (mapcar #'(lambda (el) (/ el 2))
	  (vector+ point1 point2)))

(defun distance (point1 point2)
  "Returns the 3D distance between two points.
Возвращает 3D расстояние между двумя точками "
  (vector-length  (vector- point1 point2)))

(defun inters (pt1 pt2 pt3 pt4 &optional (onseg t))
  "Finds the intersection of two lines.
Находит пересечение двух линий"
  (let* ((x1    (first  pt1))
	 (y1    (second pt1))
	 (x2    (first  pt1))
	 (y2    (second pt1))
	 (x3    (first  pt1))
	 (y3    (second pt1))
	 (x4    (first  pt1))
	 (y4    (second pt1))
	 (x2-x1 (- x2 x1))
	 (y2-y1 (- y2 y1))
	 (x4-x3 (- x4 x3))
	 (y4-y3 (- y4 y3))))
;  (cond () x2-x1 y2-y1 x4-x3 y4-y3)
  )


(defun polar (point angle distance)
  (vector+ point (list (* distance (cos angle)) (* distance (sin angle)) 0)))

(defun angle (point1 point2)
  "Returns an angle in radians of a line defined by two endpoints.
Возвращает угол в радианах линии, определенной двумя конечными точками
Пример использования:
(angle '(0 0 0) '(1 1 0))
(angle '(0 0 0) '(1 -1 0))
"
  (let ((v (vector- point2 point1)))
    (atan (second v) (first v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dr-rect (point width hight &key (os *standard-output*))
  (dr-pline 
   (list
    point
    (mapcar #'+ point (list width 0     0))
    (mapcar #'+ point (list width hight 0))
    (mapcar #'+ point (list 0     hight 0))
    point)
   256 :os os))

(defun dr-format-a4 (point &key (os *standard-output*))
  (let* ((size '(210 297))
	 (in-size (vector+ size (list -25 -10))))
    (dr-rect point (first size) (second size) :os os)
    (dr-rect (vector+ point (list 20 5))  (first in-size) (second in-size) :os os)))

(defun dr-format-a3 (point &key (os *standard-output*))
  (let* ((size '(420 297))
	 (in-size (vector+ size (list -25 -10))))
    (dr-rect point (first size) (second size) :os os)
    (dr-rect (vector+ point (list 20 5))  (first in-size) (second in-size) :os os)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axis-alert-mode-set (val &key (os *standard-output*))
  (format os "(mnas-axis:alert-mode-set ~a)~%" val))

(defun axis-prompt-mode-set  (val &key (os *standard-output*))
  (format os "(mnas-axis:prompt-mode-set ~a)~%" val))

(defun axis-print-list (axis-name axis-data &key (os *standard-output*))
  (format os "(setq ~A '(~{~f~^ ~}))~%" axis-name axis-data)
  )

(defun axis-draw-multiple-graphs-by-axis-names (x-axis-name x-axis-data y-axis-name-lst y-axis-data-lst &key (os *standard-output*))
  (axis-alert-mode-set nil  :os os)
  (axis-prompt-mode-set nil :os os)
  (axis-print-list x-axis-name x-axis-data :os os)
  (mapcar #'(lambda (nm data ) (axis-print-list nm data :os os)) y-axis-name-lst y-axis-data-lst)
  (format os "(axis:draw-multiple-graphs-by-axis-names ~S '~S)~%" x-axis-name y-axis-name-lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lines-load-line-types (&key (os *standard-output*))
    (format os "(mnas-lines:load-line-types)~%"))
  
  
(defun setvar (variable value &key (os *standard-output*))
  (format os
	  (concatenate
	   'string
	   "(setvar ~s "
	   (cond
	     ((null value) "~a")
	     ((stringp value) "~s")
	     ((numberp value) "~f")
	     ((listp value)   "'(~{~f~^ ~})")
	     (t "~f"))
	   ")~%")
	  variable value))
