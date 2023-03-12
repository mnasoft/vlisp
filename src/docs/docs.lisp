;;;; ./src/docs/docs.lisp

(defpackage :vlisp/docs
  (:use #:cl ) 
  (:nicknames "VL/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(vlisp/docs) содержит функции
  генерирования и публикации документации."))

(in-package :vlisp/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:vlisp          :vlisp)
      (:vlisp/axis     nil)
      (:vlisp/dr       nil)
;;;;      (:vlisp/docs     nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:vlisp
      :vlisp/axis
      :vlisp/dr
;;;;  :vlisp/docs 
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :vlisp)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
