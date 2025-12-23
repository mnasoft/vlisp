;;;; src/geometry/svg/tests/tests.lisp

(defpackage #:vlisp/geometry-svg-tests
  (:use #:cl #:fiveam #:vlisp/geometry #:vlisp/geometry-svg)
  (:export #:run-tests))
(in-package #:vlisp/geometry-svg-tests)

(def-suite svg-suite)
(in-suite svg-suite)

(test render-basic-scene
  "Тест базового рендеринга примитивов в SVG-файл"
  (let* ((p1 (make-instance '<point-2d> :x 0 :y 0))
         (p2 (make-instance '<point-2d> :x 10 :y 10))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance '<circle-2d> :center p1 :radius 5.0))
         (arc (make-instance '<arc-2d> :center p1 :radius 7.0 :start-angle 0.0 :end-angle (/ pi 2)))
         (objs (list p1 p2 seg circ arc))
         (svg-string (render-to-svg objs :width 800 :height 600))
         (output-path "/tmp/vlisp-geometry-test-scene.svg"))
    ;; Проверяем, что строка SVG не пустая
    (is (> (length svg-string) 100))
    ;; Проверяем наличие основных SVG-тегов
    (is (search "<svg" svg-string))
    (is (search "</svg>" svg-string))
    (is (search "<circle" svg-string))
    (is (search "<line" svg-string))
    (is (search "<polyline" svg-string))
    ;; Пишем файл
    (is (equal output-path (write-svg-file objs output-path :width 800 :height 600)))
    ;; Проверяем, что файл создан
    (is (probe-file output-path))))

(test render-empty-scene
  "Тест рендеринга пустой сцены"
  (let ((svg (render-to-svg nil)))
    (is (search "<svg" svg))
    (is (search "</svg>" svg))))

(test render-single-point
  "Тест рендеринга одной точки"
  (let* ((p (make-instance '<point-2d> :x 5 :y 5))
         (svg (render-to-svg (list p))))
    (is (search "<circle" svg))
    (is (search "fill='#d22'" svg))))

;; Entry point
(defun run-tests ()
  (run! 'svg-suite))
