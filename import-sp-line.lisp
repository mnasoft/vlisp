(in-package #:vlisp)

(defun import-one (data)
  "Выполняет преобразование строки спецификации, в табличную форму"
  (format t "~{| ~A~}~%"
	  (cdr
	   (reverse
	    (loop :for i :in (reverse data) :by #'cddr
	       :collect i)))))

(defun import-list (data)
  "Выполняет преобразование строк спецификации, в табличную форму"
  (mapc #'import-one data)
  t)


