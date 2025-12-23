;;;; tests/utils.lisp
;;;; Вспомогательные функции для тестирования

(in-package #:vlisp/tests)

;; Экспортируем функции сравнения для использования в других модулях

(defun approx-equal (a b &optional (epsilon 1d-9))
  "Проверяет приблизительное равенство двух чисел в пределах epsilon
   
   Аргументы:
   - a, b: числа для сравнения
   - epsilon: допустимая погрешность (по умолчанию 1d-9)
   
   Возвращает: T если числа приблизительно равны, иначе NIL"
  (< (abs (- a b)) epsilon))

(defun approx-equal-list (list1 list2 &optional (epsilon 1d-9))
  "Проверяет приблизительное равенство двух списков чисел
   
   Аргументы:
   - list1, list2: списки для сравнения
   - epsilon: допустимая погрешность (по умолчанию 1d-9)
   
   Возвращает: T если списки приблизительно равны, иначе NIL"
  (and (= (length list1) (length list2))
       (every #'(lambda (a b) (approx-equal a b epsilon)) list1 list2)))

(defun compare-points (p1 p2 &optional (epsilon 1d-9))
  "Сравнивает две точки (списки координат)
   
   Удобный alias для approx-equal-list"
  (approx-equal-list p1 p2 epsilon))
