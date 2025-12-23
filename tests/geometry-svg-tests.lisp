;;;; tests/geometry-svg-tests.lisp

(in-package #:vlisp/tests)

(def-suite svg-suite
  :in vlisp-suite
  :description "Тесты SVG-рендеринга")

(in-suite svg-suite)

(test render-basic-scene
  "Тест базового рендеринга примитивов в SVG-файл"
  (let* ((p1 (make-instance 'vlisp/geometry:<point-2d> :x 0 :y 0))
         (p2 (make-instance 'vlisp/geometry:<point-2d> :x 10 :y 10))
         (seg (make-instance 'vlisp/geometry:<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance 'vlisp/geometry:<circle-2d> :center p1 :radius 5.0))
         (arc (make-instance 'vlisp/geometry:<arc-2d> :center p1 :radius 7.0 :start-angle 0.0 :end-angle (/ pi 2)))
         (objs (list p1 p2 seg circ arc))
         (svg-string (vlisp/geometry-svg:render-to-svg objs :width 800 :height 600))
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
    (is (equal output-path (vlisp/geometry-svg:write-svg-file objs output-path :width 800 :height 600)))
    ;; Проверяем, что файл создан
    (is (probe-file output-path))))

(test render-empty-scene
  "Тест рендеринга пустой сцены"
  (let ((svg (vlisp/geometry-svg:render-to-svg nil)))
    (is (search "<svg" svg))
    (is (search "</svg>" svg))))

(test render-single-point
  "Тест рендеринга одной точки"
  (let* ((p (make-instance 'vlisp/geometry:<point-2d> :x 5 :y 5))
         (svg (vlisp/geometry-svg:render-to-svg (list p))))
    (is (search "<circle" svg))
    (is (search "fill='#d22'" svg))))
