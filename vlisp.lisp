;;;; vlisp.lisp

(in-package #:vlisp)

(defun dotted-listp (lst)
  (not (listp (cdr (last lst)))))

(defun load-vlisp-file (fname &key (os *standard-output*))
  (format os "(load (findfile ~s))~%" fname))

(defun axis-load-point-types (&key (os *standard-output*))
  (format os "(axis:load-point-types)~%"))

(defun dr-axis (start-pt end-pt start-value end-value flag name &key (os *standard-output*))
  (format os "(dr:axis '(~{~f~^ ~}) '(~{~f~^ ~}) ~f ~f ~d ~s)~%" start-pt end-pt start-value end-value flag name))

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
  (format os "(dr:point '(~{~f~^ ~}) ~d)~%" point color))

(defun dr-points (points color &key (os *standard-output*))
  (format os "(dr:points '(")
  (mapc
   #'(lambda (el)
       (format os "(~{~f~^ ~})" el))
   points)
  (format os ") ~d)~%" color))

(defun dr-line (start-point  end-point color &key (os *standard-output*))
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

(defun dr-text (text point height rotation color &key (os *standard-output*))
  (format os "(dr:text ~s '(~{~f~^ ~}) ~f ~f ~d)~%" text point height rotation color))

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

(defun dr-insert (point block-name &key (os *standard-output*))
  (format os "(dr:insert '(~{~f~^ ~}) ~s)~%" point block-name))



(defun axis-alert-mode-set (val &key (os *standard-output*))
  (format os "(mnas-axis:alert-mode-set ~a)~%" val))

(defun axis-prompt-mode-set  (val &key (os *standard-output*))
  (format os "(mnas-axis:prompt-mode-set ~a)~%" val))

(defun axis-print-list (axis-name axis-data &key (os *standard-output*))
  (format os "(setq ~A '(~{~f~^ ~}))~%" axis-name axis-data)
  )

(defun axis-draw-multiple-graphs-by-axis-names (x-axis-name x-axis-data y-axis-name-lst y-axis-data-lst &key (os *standard-output*))
  (axis-alert-mode-set nil)
  (axis-prompt-mode-set nil)
  (axis-print-list x-axis-name x-axis-data :os os)
  (mapcar #'(lambda (nm data ) (axis-print-list nm data :os os)) y-axis-name-lst y-axis-data-lst)
  (format os "(axis:draw-multiple-graphs-by-axis-names ~S '~S)~%" x-axis-name y-axis-name-lst))

