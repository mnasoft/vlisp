;;;; src/geometry/tests/tests.lisp

(defpackage #:vlisp/geometry-tests
  (:use #:cl #:fiveam #:vlisp/geometry)
  (:export #:run-tests))
(in-package #:vlisp/geometry-tests)

(def-suite geometry-suite)
(in-suite geometry-suite)

(defun pt (x y)
  (make-instance '<point-2d> :x x :y y))

(test start-end-params
  (let* ((p1 (pt 0 0))
         (p2 (pt 10 0))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance '<circle-2d> :center p1 :radius 5.0))
         (arc (make-instance '<arc-2d> :center p1 :radius 5.0 :start-angle 0.5 :end-angle 1.0)))
    (is (= 0.0 (curve-start-param seg)))
    (is (= 1.0 (curve-end-param seg)))
    (is (= 0.0 (curve-start-param circ)))
    (is (= (* 2 pi) (curve-end-param circ)))
    (is (= 0.5 (curve-start-param arc)))
    (is (= 1.0 (curve-end-param arc)))))

(test start-end-points
  (let* ((c (pt 1 2))
         (p1 (pt 0 0))
         (p2 (pt 10 0))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance '<circle-2d> :center c :radius 5.0))
         (arc (make-instance '<arc-2d> :center c :radius 5.0 :start-angle 0.0 :end-angle (/ pi 2))))
    (flet ((=pt (a b)
             (let ((eps 1e-9))
               (and (< (abs (- (x a) (x b))) eps)
                    (< (abs (- (y a) (y b))) eps)))))
      (is (=pt p1 (curve-start-point seg)))
      (is (=pt p2 (curve-end-point seg)))
      (is (=pt (pt (+ (x c) 5.0) (y c)) (curve-start-point circ)))
      (is (=pt (pt (x c) (+ (y c) 5.0)) (curve-point-at-param circ (/ pi 2))))
      (is (=pt (pt (+ (x c) 5.0) (y c)) (curve-start-point arc))))))

(test curve-dist
  (let* ((p1 (pt 0 0))
         (p2 (pt 10 0))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance '<circle-2d> :center p1 :radius 5.0))
         (arc (make-instance '<arc-2d> :center p1 :radius 5.0 :start-angle 0.0 :end-angle pi)))
    (is (= 5.0 (curve-dist-at-param seg 0.5)))
    (is (= (* 5.0 (/ pi 2)) (curve-dist-at-param circ (/ pi 2))))
    (is (= (* 5.0 (/ pi 2)) (curve-dist-at-param arc (/ pi 2))))))

(test point-at-param
  (let* ((p1 (pt 0 0))
         (p2 (pt 10 0))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (mid (curve-point-at-param seg 0.5)))
    (is (= 5.0 (x mid)))
    (is (= 0.0 (y mid)))))

(test first-deriv
  (let* ((p1 (pt 0 0))
         (p2 (pt 10 0))
         (seg (make-instance '<line-2d> :start-point p1 :end-point p2))
         (circ (make-instance '<circle-2d> :center p1 :radius 5.0))
         (t0 (curve-first-deriv seg 0.3))
         (t1 (curve-first-deriv circ 0.0)))
    (is (and (= 1.0 (x t0)) (= 0.0 (y t0))))
    (is (and (= 0.0 (x t1)) (= 1.0 (y t1))))))

(test second-deriv
  (let* ((c (pt 0 0))
         (circ (make-instance '<circle-2d> :center c :radius 5.0))
         (arc (make-instance '<arc-2d> :center c :radius 5.0 :start-angle 0.0 :end-angle pi))
         (v0 (curve-second-deriv circ 0.0))
         (v1 (curve-second-deriv arc 0.0)))
    (is (and (= -5.0 (x v0)) (= 0.0 (y v0))))
    (is (and (= -5.0 (x v1)) (= 0.0 (y v1))))
    (is (null (curve-second-deriv (pt 1 2) 0.1)))
    (is (null (curve-second-deriv (make-instance '<line-2d> :start-point (pt 0 0) :end-point (pt 10 0)) 0.5)))))

(test curvature-and-normal
  (let* ((c (pt 0 0))
         (circ (make-instance '<circle-2d> :center c :radius 5.0))
         (seg (make-instance '<line-2d> :start-point (pt 0 0) :end-point (pt 10 0)))
         (k (curve-curvature circ 0.3))
         (n-left (curve-normal-at-param circ 0.0 :side :left))
         (n-right (curve-normal-at-param circ 0.0 :side :right))
         (n-seg (curve-normal-at-param seg 0.2 :side :left)))
    (is (= (/ 1.0 5.0) k))
    (is (and (= -1.0 (x n-left)) (= 0.0 (y n-left))))
    (is (and (= 1.0 (x n-right)) (= 0.0 (y n-right))))
    (is (and (= 0.0 (x n-seg)) (= 1.0 (y n-seg))))))

;; Entry point to run all tests
(defun run-tests ()
  (run! 'geometry-suite))
