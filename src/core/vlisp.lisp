;;;; ./src/core/vlisp.lisp

(defpackage #:vlisp
  (:use #:cl)
  (:export *origin*
	   )
  (:export dotted-listp
           )
  (:export setvar
	   )
  (:export load-vlisp-file
	   )
  (:export lines-load-line-types)
  (:export vector-length
	   vector+
	   vector-
	   normalize
	   mid-point
           vector-dot-product
           vector-cross-product
	   )
  (:export angle
	   distance
	   inters
	   polar
	   )
  (:export load-vlisp-apps
           )
  (:export osnap
           textbox
           ))

(in-package #:vlisp)

(defun load-vlisp-file (fname &key (os *standard-output*))
  (format os "(load (findfile ~s))~%" fname))

(defun load-vlisp-apps (&key (os *standard-output*) (apps '("./bin/Axis.VLX" "./bin/dim_style.VLX" "./bin/lines.VLX")))
  (mapc #'(lambda (app) (vlisp:load-vlisp-file app  :os os)) apps))

(defparameter *origin* '(0.0d0 0.0d0 0.0d0)
  "Точка начала координат.")

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
  "@b(Описание:) функция @b(mid-point) возвращает точку, находящуюся
на середине отрезка с конечными точками point1 point2."
  (mapcar #'(lambda (el) (/ el 2))
	  (vector+ point1 point2)))

(defun vector-dot-product (vector-1 vector-2)
  "@b(Описание:) функция @b(vector-dot-product) возвращает результат
скалярного произведения векторов @b(vector-1) и @b(vector-2)."
  (apply #'+
         (loop :for x-1 :in vector-1
               :for x-2 :in vector-2 :collect
                                     (* x-1 x-2))))

(defun vector-cross-product (vector-1 vector-2)
  "@b(Описание:) функция @b(vector-cross-product)
Векторное произведение векторов."
  (multiple-value-bind (x-1 y-1 z-1) (values-list vector-1)
    (multiple-value-bind (x-2 y-2 z-2) (values-list vector-2)
      (list
       (- (* y-1 z-2) (* y-2 z-1))
       (- (* z-1 x-2) (* x-1 z-2))
       (- (* x-1 y-2) (* y-1 x-2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  
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
