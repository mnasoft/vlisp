;;;; tests/package.lisp

(defpackage #:vlisp/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests
           #:vlisp-suite))

(in-package #:vlisp/tests)

;; Определяем основной набор тестов
(def-suite vlisp-suite
    :description "Основной набор тестов для vlisp")

(defun run-tests ()
  "Запускает все тесты vlisp"
  (run! 'vlisp-suite))
