;;;; src/geometry/svg/svg.lisp
;;;; Рендеринг 2D примитивов в SVG

(in-package #:vlisp/geometry-svg)

(defun %clamp (v a b) (max a (min b v)))

(defun make-pt (x y)
  (make-instance 'vlisp/geometry:<point-2d> :x x :y y))

(defun point+ (p dx dy)
  (make-pt (+ (vlisp/geometry:x p) dx)
           (+ (vlisp/geometry:y p) dy)))

(defun %min (a b) (if (< a b) a b))
(defun %max (a b) (if (> a b) a b))

(defun bounds-of-objects (objs &key (samples 64))
  "Вычисляет AABB всех объектов в мировых координатах.
   Для дуг использует дискретизацию по углу. Возвращает четыре значения: minx miny maxx maxy."
  (if (null objs)
      (values 0.0 0.0 1.0 1.0)
      (let ((minx most-positive-double-float)
            (miny most-positive-double-float)
            (maxx most-negative-double-float)
            (maxy most-negative-double-float))
        (flet ((acc (x y)
             (setf minx (%min minx x)
                   miny (%min miny y)
                   maxx (%max maxx x)
                   maxy (%max maxy y))))
      (dolist (o objs)
        (typecase o
          (vlisp/geometry:<point-2d>
           (acc (vlisp/geometry:x o) (vlisp/geometry:y o)))
          (vlisp/geometry:<line-2d>
           (let ((p1 (vlisp/geometry:<line-2d>-start-point o))
                 (p2 (vlisp/geometry:<line-2d>-end-point o)))
             (acc (vlisp/geometry:x p1) (vlisp/geometry:y p1))
             (acc (vlisp/geometry:x p2) (vlisp/geometry:y p2))))
          (vlisp/geometry:<circle-2d>
           (let* ((c (vlisp/geometry:<circle-2d>-center o))
                  (r (vlisp/geometry:<circle-2d>-radius o))
                  (cx (vlisp/geometry:x c)) (cy (vlisp/geometry:y c)))
             (acc (- cx r) (- cy r))
             (acc (+ cx r) (+ cy r))))
          (vlisp/geometry:<arc-2d>
           (let* ((c (vlisp/geometry:<arc-2d>-center o))
                  (r (vlisp/geometry:<arc-2d>-radius o))
                  (a0 (vlisp/geometry:<arc-2d>-start-angle o))
                  (a1 (vlisp/geometry:<arc-2d>-end-angle o))
                  (cx (vlisp/geometry:x c)) (cy (vlisp/geometry:y c))
                  (n (max 2 samples))
                  (amin (min a0 a1)) (amax (max a0 a1)))
             (dotimes (i (1+ n))
               (let* ((tau (+ amin (* (/ (- amax amin) n) i)))
                      (x (+ cx (* r (cos tau))))
                      (y (+ cy (* r (sin tau)))))
                 (acc x y))))
           )
          (t nil)))
        (values minx miny maxx maxy)))))

(defun world->svg-transform (minx miny maxx maxy width height &key (margin 10))
  "Строит линейное преобразование мировых координат в SVG с инверсией оси Y."
  (let* ((w (max 1e-9 (- maxx minx)))
         (h (max 1e-9 (- maxy miny)))
         (aw (- width (* 2 margin)))
         (ah (- height (* 2 margin)))
         (sx (/ aw w))
         (sy (/ ah h))
         (s (min sx sy))
         (tx (- margin (* s minx)))
         (ty (+ margin (* s maxy)))) ; инвертируем Y ниже
    (values s s tx ty)))

(defun world->svg (x y s tx ty height)
  "Преобразование точки (x,y) -> (sx, sy) в системе SVG."
  (let* ((sx (+ (* s x) tx))
         (sy (- height (- ty (* s y)))))
    (values sx sy)))

(defun fmt (control &rest args)
  (apply #'format nil control args))

(defun render-point (p s tx ty height &key (r 2) (stroke "none") (fill "#d22"))
  (multiple-value-bind (sx sy) (world->svg (vlisp/geometry:x p) (vlisp/geometry:y p) s tx ty height)
    (fmt "<circle cx='~f' cy='~f' r='~f' stroke='~a' fill='~a'/>" sx sy r stroke fill)))

(defun render-line (l s tx ty height &key (stroke "#06c") (sw 1))
  (let ((p1 (vlisp/geometry:<line-2d>-start-point l))
        (p2 (vlisp/geometry:<line-2d>-end-point l)))
    (multiple-value-bind (x1 y1) (world->svg (vlisp/geometry:x p1) (vlisp/geometry:y p1) s tx ty height)
      (multiple-value-bind (x2 y2) (world->svg (vlisp/geometry:x p2) (vlisp/geometry:y p2) s tx ty height)
        (fmt "<line x1='~f' y1='~f' x2='~f' y2='~f' stroke='~a' stroke-width='~f'/>" x1 y1 x2 y2 stroke sw)))))

(defun render-circle (c s tx ty height &key (stroke "#060") (sw 1) (fill "none"))
  (let* ((ctr (vlisp/geometry:<circle-2d>-center c))
         (r (vlisp/geometry:<circle-2d>-radius c))
         (cx (vlisp/geometry:x ctr))
         (cy (vlisp/geometry:y ctr)))
    (multiple-value-bind (sx sy) (world->svg cx cy s tx ty height)
      (fmt "<circle cx='~f' cy='~f' r='~f' stroke='~a' stroke-width='~f' fill='~a'/>" sx sy (* s r) stroke sw fill))))

(defun render-arc (a s tx ty height &key (stroke "#a60") (sw 1) (fill "none") (samples 64))
  ;; Рендерим дугу как polyline по выборке углов
  (let* ((c (vlisp/geometry:<arc-2d>-center a))
         (r (vlisp/geometry:<arc-2d>-radius a))
         (a0 (vlisp/geometry:<arc-2d>-start-angle a))
         (a1 (vlisp/geometry:<arc-2d>-end-angle a))
         (amin (min a0 a1))
         (amax (max a0 a1))
         (n (max 2 samples))
         (pts (loop for i from 0 to n collect
                    (let* ((tau (+ amin (* (/ (- amax amin) n) i)))
                           (x (+ (vlisp/geometry:x c) (* r (cos tau))))
                           (y (+ (vlisp/geometry:y c) (* r (sin tau)))))
                      (multiple-value-bind (sx sy) (world->svg x y s tx ty height)
                        (fmt "~f,~f" sx sy))))))
    (fmt "<polyline fill='~a' stroke='~a' stroke-width='~f' points='~{~a~^ ~}'/>" fill stroke sw pts)))

(defun render-object (o s tx ty height)
  (typecase o
    (vlisp/geometry:<point-2d> (render-point o s tx ty height))
    (vlisp/geometry:<line-2d> (render-line o s tx ty height))
    (vlisp/geometry:<circle-2d> (render-circle o s tx ty height))
    (vlisp/geometry:<arc-2d> (render-arc o s tx ty height))
    (t "")))

(defun render-to-svg (objects &key (width 800) (height 600) (margin 10))
  "Рендерит список объектов в текст SVG. Автомасштабирование с полями."
  (multiple-value-bind (minx miny maxx maxy) (bounds-of-objects objects)
    (multiple-value-bind (sx sy tx ty) (world->svg-transform minx miny maxx maxy width height :margin margin)
      (declare (ignore sy))
      (with-output-to-string (out)
        (format out "<svg xmlns='http://www.w3.org/2000/svg' width='~d' height='~d' viewBox='0 0 ~d ~d'>~%" width height width height)
        (format out "<rect x='0' y='0' width='~d' height='~d' fill='white'/>~%" width height)
        (dolist (o objects)
          (format out "~a~%" (render-object o sx tx ty height)))
        (format out "</svg>~%")))))

(defun write-svg-file (objects path &key (width 800) (height 600) (margin 10))
  "Записывает SVG в файл PATH."
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~a" (render-to-svg objects :width width :height height :margin margin)))
  path)
