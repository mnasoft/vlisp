;;;; ./src/dr/dr.lisp

(defpackage :vlisp/dr
  (:use #:cl)
  (:intern dotted-listp)
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
	   dr-format-a3))

(in-package :vlisp/dr)

(defun dotted-listp (lst)
  (not (listp (cdr (last lst)))))

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
  "@b(Описание:) функция @b(dr-point) возвращает nil.

 Выводит в поток команду на языке AutoLisp, генерирующую точку.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-point '(10.56 50.54) 42) 
 -> (dr:point '(10.56 50.54) 42)
@end(code)"
  (format os "(dr:point '(~{~f~^ ~}) ~d)~%" point color))

(defun dr-points (points color &key (os *standard-output*))
  "@b(Описание:) функция @b(dr-point) возвращает nil.

Выводит в поток команду на языке AutoLisp, генерирующие точки @b(points).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-points '((10.56 50.54) (5.36 -10.34)) 42) 
 -> (dr:points '((10.56 50.54)(5.36 -10.34)) 42)
@end(code)"
  (format os "(dr:points '(")
  (mapc
   #'(lambda (el)
       (format os "(~{~f~^ ~})" el))
   points)
  (format os ") ~d)~%" color))

(defun dr-line (start-point  end-point color &key (os *standard-output*))
  "@b(Описание:) функция @b(dr-line) возвращает nil.

Выводит в поток команду на языке AutoLisp, генерирую отрезок @b(dr-line).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-line '(10.56 50.54) '(5.36 -10.34) 13) 
 ->(dr:line '(10.56 50.54) '(5.36 -10.34) 13)
@end(code)"  
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

(defun dr-axis (start-pt end-pt start-value end-value flag name &key (os *standard-output*) (caption "") (dimension ""))
  (format os "(dr:axis '(~{~f~^ ~}) '(~{~f~^ ~}) ~f ~f ~d ~s ~s ~s)~%"
          start-pt
          end-pt
          start-value
          end-value
          flag
          (mnas-string/translit:translit name)
          caption dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dr-rect (point width hight &key (os *standard-output*))
  "@b(Описание:) функция @b(dr-rect) генерирует вывод в виде команды
  на языке Vlisp, генерирующий прямоугольник.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-rect '(10 10 0.0) 50 20) => NIL
 -> (dr:pline '((10.0 10.0 0.0)(60.0 10.0 0.0)(60.0 30.0 0.0)(10.0 30.0 0.0)(10.0 10.0 0.0)) 256)
@end(code)
"
  (dr-pline 
   (list
    point
    (mapcar #'+ point (list width 0     0))
    (mapcar #'+ point (list width hight 0))
    (mapcar #'+ point (list 0     hight 0))
    point)
   256 :os os))

(defun dr-format-a4 (point &key (os *standard-output*))
  "
@b(Описание:) функция @b(dr-format-a4)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-format-a4 '(50. 50.0)) => NIL
 -> (dr:pline '((50.0 50.0)(260.0 50.0)(260.0 347.0)(50.0 347.0)(50.0 50.0)) 256)
    (dr:pline '((70.0 55.0)(255.0 55.0)(255.0 342.0)(70.0 342.0)(70.0 55.0)) 256)
@end(code)
"
  (let* ((size '(210 297))
	 (in-size (vlisp:vector+ size (list -25 -10))))
    (dr-rect point (first size) (second size) :os os)
    (dr-rect (vlisp:vector+ point (list 20 5))  (first in-size) (second in-size) :os os)))

(defun dr-format-a3 (point &key (os *standard-output*))
  "@b(Описание:) функция @b(dr-format-a3)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dr-format-a3 '(500. 400. 50.0))
 -> (dr:pline '((500.0 400.0)(920.0 400.0)(920.0 697.0)(500.0 697.0)(500.0 400.0)) 256)
    (dr:pline '((520.0 405.0)(915.0 405.0)(915.0 692.0)(520.0 692.0)(520.0 405.0)) 256)
 => NIL
@end(code)
"
  (let* ((size '(420 297))
	 (in-size (vlisp:vector+ size (list -25 -10))))
    (dr-rect point (first size) (second size) :os os)
    (dr-rect (vlisp:vector+ point (list 20 5))  (first in-size) (second in-size) :os os)))
