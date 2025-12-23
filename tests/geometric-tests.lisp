;;;; tests/geometric-tests.lisp
;;;; Тесты для геометрических функций

(in-package #:vlisp/tests)

(def-suite geometric-suite
    :in vlisp-suite
    :description "Тесты геометрических функций")

(in-suite geometric-suite)

;;; angle тесты

(test angle-horizontal
  "Угол горизонтальной линии"
  (is (approx-equal (vlisp:angle '(0 0) '(1 0)) 0)))

(test angle-vertical
  "Угол вертикальной линии"
  (is (approx-equal (vlisp:angle '(0 0) '(0 1)) (/ pi 2) 1d-6)))

(test angle-45-degrees
  "Угол 45 градусов"
  (is (approx-equal (vlisp:angle '(0 0) '(1 1)) (/ pi 4) 1d-6)))

(test angle-negative
  "Углы в отрицательном направлении"
  (is (approx-equal (vlisp:angle '(0 0) '(1 -1)) (/ pi -4) 1d-6)))

(test angle-from-example
  "Тесты из примеров функции"
  (is (approx-equal (vlisp:angle '(1.0 1.0) '(1.0 4.0)) (/ pi 2) 1d-6))
  (is (approx-equal (vlisp:angle '(5.0 1.33) '(2.4 1.33)) pi 1d-6)))

;;; distance тесты

(test distance-zero
  "Расстояние до точки самой себе"
  (is (= 0 (vlisp:distance '(5 5) '(5 5)))))

(test distance-horizontal
  "Расстояние по горизонтали"
  (is (= 5 (vlisp:distance '(0 0) '(5 0)))))

(test distance-vertical
  "Расстояние по вертикали"
  (is (= 10 (vlisp:distance '(3 0) '(3 10)))))

(test distance-3d
  "Расстояние в 3D"
  (is (approx-equal (vlisp:distance '(0 0 0) '(3 4 0)) 5)))

(test distance-from-example
  "Тесты из примеров функции"
  (is (approx-equal (vlisp:distance '(1.0 1.0) '(1.0 4.0)) 3.0))
  (is (approx-equal (vlisp:distance '(5.0 1.33) '(2.4 1.33)) 2.6)))

;;; polar тесты

(test polar-zero-distance
  "Точка на нулевом расстоянии"
  (is (approx-equal-list (vlisp:polar '(0 0 0) 0 0) '(0 0 0))))

(test polar-horizontal
  "Точка на горизонтальной оси"
  (is (approx-equal-list (vlisp:polar '(0 0 0) 0 5) '(5 0 0))))

(test polar-vertical
  "Точка на вертикальной оси"
  (is (approx-equal-list (vlisp:polar '(0 0 0) (/ pi 2) 5) '(0 5 0))))

(test polar-45-degrees
  "Точка под углом 45 градусов"
  (let ((result (vlisp:polar '(0 0 0) (/ pi 4) (sqrt 2))))
    (is (approx-equal-list result '(1 1 0) 1d-6))))

(test polar-from-offset
  "Поиск точки от смещённого начала"
  (is (approx-equal-list (vlisp:polar '(10 10 0) 0 5) '(15 10 0))))

;;; inters тесты

(test inters-parallel
  "Пересечение параллельных линий (NIL)"
  (is (null (vlisp:inters '(0 0) '(10 0) '(0 5) '(10 5)))))

(test inters-perpendicular-not-on-segment
  "Пересечение перпендикулярных линий вне сегмента"
  (is (null (vlisp:inters '(1 1) '(9 9) '(4 1) '(4 2) t))))

(test inters-perpendicular-extend
  "Пересечение перпендикулярных линий с расширением"
  (multiple-value-bind (p1 p2)
      (vlisp:inters '(1 1) '(9 9) '(4 1) '(4 2) nil)
    (is (approx-equal-list p1 '(4 4)))))

(test inters-crossing-on-segment
  "Пересечение на сегментах"
  (multiple-value-bind (p1 p2)
      (vlisp:inters '(0 0) '(10 10) '(0 10) '(10 0) t)
    (is (approx-equal-list p1 '(5 5)))
    (is (approx-equal-list p2 '(5 5)))))

(test inters-t-intersection
  "T-образное пересечение"
  (multiple-value-bind (p1 p2)
      (vlisp:inters '(0 5) '(10 5) '(5 0) '(5 10) t)
    (is (approx-equal-list p1 '(5 5)))
    (is (approx-equal-list p2 '(5 5)))))

(test inters-from-example
  "Тесты из примера функции"
  (let ((a '(1.0 1.0))
        (b '(9.0 9.0))
        (c '(4.0 1.0))
        (d '(4.0 2.0)))
    (is (null (vlisp:inters a b c d)))
    (is (null (vlisp:inters a b c d t)))
    (multiple-value-bind (p1 p2)
        (vlisp:inters a b c d nil)
      (is (approx-equal-list p1 '(4.0 4.0))))))


