;;;; ./src/docs/docs.lisp

(defpackage #:vlisp/docs
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
      (:vlisp/docs     nil)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:vlisp
      :vlisp/axis
      :vlisp/dr
      :vlisp/docs 
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/vlip.
"
  (mnas-package:make-html-path :vlisp)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:vlisp :vlisp/docs)
   "Vlisp"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "vlisp")
   :output-format of)
  (codex:document :vlisp)
  (make-graphs)
  (mnas-package:copy-doc->public-html "vlisp")
  (mnas-package:rsync-doc "vlisp"))

;;;; (make-all)
