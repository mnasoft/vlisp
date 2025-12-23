;;;; tests/vector-tests.lisp
;;;; Тесты для векторных операций

(in-package #:vlisp/tests)

(def-suite vector-suite
    :in vlisp-suite
    :description "Тесты векторных операций")

(in-suite vector-suite)

(test vector-length-zero
  "Тест длины нулевого вектора"
  (is (= 0 (vlisp:vector-length '(0 0 0)))))

(test vector-length-unit-vectors
  "Тест единичных векторов"
  (is (approx-equal (vlisp:vector-length '(1 0 0)) 1))
  (is (approx-equal (vlisp:vector-length '(0 1 0)) 1))
  (is (approx-equal (vlisp:vector-length '(0 0 1)) 1)))

(test vector-length-3-4-5
  "Тест треугольника 3-4-5"
  (is (approx-equal (vlisp:vector-length '(3 4 0)) 5)))

(test vector-length-2d
  "Тест в 2D"
  (is (approx-equal (vlisp:vector-length '(1 1)) (sqrt 2))))

;;; vector+ тесты

(test vector-plus-identity
  "Сложение с нулевым вектором"
  (is (equal (vlisp:vector+ '(1 2 3) '(0 0 0)) '(1 2 3))))

(test vector-plus-basic
  "Базовое сложение векторов"
  (is (equal (vlisp:vector+ '(1 2 3) '(1 2 3)) '(2 4 6)))
  (is (equal (vlisp:vector+ '(5 10 15) '(-2 -3 -1)) '(3 7 14))))

(test vector-plus-2d
  "Сложение в 2D"
  (is (equal (vlisp:vector+ '(1 1) '(2 3)) '(3 4))))

;;; vector- тесты

(test vector-minus-identity
  "Вычитание нулевого вектора"
  (is (equal (vlisp:vector- '(1 2 3) '(0 0 0)) '(1 2 3))))

(test vector-minus-basic
  "Базовое вычитание векторов"
  (is (equal (vlisp:vector- '(5 5 5) '(2 2 2)) '(3 3 3)))
  (is (equal (vlisp:vector- '(1 2 3) '(1 2 3)) '(0 0 0))))

(test vector-minus-negative
  "Вычитание с отрицательным результатом"
  (is (equal (vlisp:vector- '(1 1 1) '(2 3 4)) '(-1 -2 -3))))

;;; normalize тесты

(test normalize-unit-vector
  "Нормализация единичного вектора"
  (is (approx-equal-list (vlisp:normalize '(1 0 0)) '(1 0 0))))

(test normalize-2d
  "Нормализация в 2D"
  (let ((result (vlisp:normalize '(3 4 0))))
    (is (approx-equal-list result '(0.6 0.8 0)))))

(test normalize-preserves-direction
  "Нормализация сохраняет направление"
  (let ((normalized (vlisp:normalize '(5 10 15)))
        (expected (vlisp:normalize '(1 2 3))))
    (is (approx-equal-list normalized expected 1d-6))))

;;; mid-point тесты

(test mid-point-symmetric
  "Середина симметричного отрезка"
  (is (approx-equal-list (vlisp:mid-point '(0 0 0) '(2 2 2)) '(1 1 1))))

(test mid-point-asymmetric
  "Середина асимметричного отрезка"
  (is (approx-equal-list (vlisp:mid-point '(0 0) '(10 10)) '(5 5))))

(test mid-point-negative
  "Середина с отрицательными координатами"
  (is (approx-equal-list (vlisp:mid-point '(-10 -10 0) '(10 10 0)) '(0 0 0))))

;;; vector-dot-product тесты

(test vector-dot-product-zero
  "Скалярное произведение нулевого вектора"
  (is (= 0 (vlisp:vector-dot-product '(0 0 0) '(1 2 3)))))

(test vector-dot-product-orthogonal
  "Скалярное произведение ортогональных векторов"
  (is (= 0 (vlisp:vector-dot-product '(1 0 0) '(0 1 0))))
  (is (= 0 (vlisp:vector-dot-product '(1 0 0) '(0 0 1)))))

(test vector-dot-product-parallel
  "Скалярное произведение параллельных векторов"
  (is (= 4 (vlisp:vector-dot-product '(2 0 0) '(2 0 0))))
  (is (= 14 (vlisp:vector-dot-product '(1 2 3) '(1 2 3)))))

(test vector-dot-product-basic
  "Базовое скалярное произведение"
  (is (= 32 (vlisp:vector-dot-product '(1 2 3) '(4 5 6)))))

;;; vector-cross-product тесты

(test vector-cross-product-zero
  "Векторное произведение с нулевым вектором"
  (is (equal (vlisp:vector-cross-product '(0 0 0) '(1 2 3)) '(0 0 0))))

(test vector-cross-product-orthogonal
  "Векторное произведение ортогональных единичных векторов"
  (is (approx-equal-list (vlisp:vector-cross-product '(1 0 0) '(0 1 0)) '(0 0 1)))
  (is (approx-equal-list (vlisp:vector-cross-product '(0 1 0) '(1 0 0)) '(0 0 -1))))

(test vector-cross-product-anticommutative
  "Векторное произведение антикоммутативно"
  (let ((v1 '(1 2 3))
        (v2 '(4 5 6)))
    (let ((cp1 (vlisp:vector-cross-product v1 v2))
          (cp2 (vlisp:vector-cross-product v2 v1)))
      (is (approx-equal-list cp1 (mapcar #'- cp2))))))


