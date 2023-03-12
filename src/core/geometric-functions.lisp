;;;; geometric-functions.lisp

;;;;selection-set-manipulation-functions

(in-package :vlisp)

" * Geometric functions

| Function                         | Description                                                                                            |
|----------------------------------+--------------------------------------------------------------------------------------------------------|
| (angle pt1 pt2)                  | Returns an angle in radians of a line defined by two endpoints                                         |
| (distance pt1 pt2)               | Returns the 3D distance between two points                                                             |
| (inters pt1 pt2 pt3 pt4 [onseg]) | Finds the intersection of two lines                                                                    |
| (osnap pt mode)                  | Returns a 3D point that is the result of applying an Object Snap mode to a specified point             |
| (polar pt ang dist)              | Returns the UCS 3D point at a specified angle and distance from a point                                |
| (textbox elist)                  | Measures a specified text object, and returns the diagonal coordinates of a box that encloses the text |

"

(defun angle (point1 point2)
  "Returns an angle in radians of a line defined by two endpoints.

 Возвращает угол в радианах линии, определенной двумя конечными точками.

 Пример использования:
@begin[lang=lisp](code)
 (angle '(1.0 1.0) '(1.0 4.0))     => 1.5708
 (angle '(5.0 1.33 ) '(2.4 1.33 )) => 3.14159
 (angle '(0 0 0) '(1 1 0))         => 0.7853982
 (angle '(0 0 0) '(1 -1 0))        => -0.7853982
@end(code)
"
  (let ((v (vector- point2 point1)))
    (atan (second v) (first v))))

(defun distance (point1 point2)
  "Returns the 3D distance between two points.

 @b(Описание:) функция @b(distance) возвращает 3D расстояние между
двумя точками.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance '(1.0 1.0) '(1.0 4.0)) => 3.0
 (distance '(5.0 1.33 ) '(2.4 1.33 )) => 2.6
@end(code)"
  (vector-length  (vector- point1 point2)))


(defun inters (pt1 pt2 pt3 pt4 &optional (onseg t))
  "Finds the intersection of two lines.

 @b(Описание:) функция @b(inters) Возвращает точку пересечения двух
 отрезков, концы которых заданы точками: pt1 pt2 - для первого отрезка;
 pt3 pt4 - для второго отрезка.

@begin[lang=lisp](code)
  (let ((a '(1.0 1.0)) (b '(9.0 9.0))
	(c '(4.0 1.0)) (d '(4.0 2.0)))
    (inters a b c d    ) ;=> NIL
    (inters a b c d T  ) ;=> NIL
    (inters a b c d nil) ;=> (4.0 4.0))
@end(code)
"
  (let* ((x1    (first  pt1))
	 (y1    (second pt1))
	 (x2    (first  pt2))
	 (y2    (second pt2))
	 (x3    (first  pt3))
	 (y3    (second pt3))
	 (x4    (first  pt4))
	 (y4    (second pt4))
	 (denominator (- (* (- y4 y3) (- x1 x2)) (* (- x4 x3) (- y1 y2))))
	 (Ua 0.0)
	 (Ub 0.0)
	 (rez-a nil)
	 (rez-b nil))
    (if (= denominator 0)
	nil
	(progn
	  (setf Ua (/ (- (* (- x4 x2) (- y4 y3)) (* (- x4 x3) (- y4 y2))) denominator)
		Ub (/ (- (* (- x1 x2) (- y4 y2)) (* (- x4 x2) (- y1 y2))) denominator)
		rez-a (list (+ (* Ua x1) (* (- 1 Ua) x2)) (+ (* Ua y1) (* (- 1 Ua) y2)))
		rez-b (list (+ (* Ub x3) (* (- 1 Ub) x4)) (+ (* Ub y3) (* (- 1 Ub) y4))))
	  (cond ((null onseg) (values rez-a rez-b))
		((and onseg (<= 0 Ua 1) (<= 0 Ub 1)) (values rez-a rez-b))
		(t nil))))))

(defun polar (point angle distance)
  "Returns the UCS 3D point at a specified angle and distance from a point.

Возвращает трехмерную точку ПСК под заданным углом и расстоянием от точки."
  (vector+ point (list (* distance (cos angle)) (* distance (sin angle)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun textbox (elist)
  "Measures a specified text object, and returns the diagonal coordinates of a box that encloses the text"
  (error "textbox. Функция пока не определена."
         ))
(defun osnap (pt mode)
  "Returns a 3D point that is the result of applying an Object Snap mode to a specified point.

Возвращает трехмерную точку, которая является результатом применения режима привязки объекта к указанной точке."
    (error "osnap. Функция пока не определена."))
