#!/usr/bin/env sbcl --script
;;;; run-tests.lisp
;;;; Скрипт для запуска тестов vlisp из командной строки

(require :asdf)

;; Загружаем систему тестирования
(asdf:test-system "vlisp-tests")

;; Завершаем работу
(uiop:quit)
