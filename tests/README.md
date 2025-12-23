# Tests для vlisp

Этот каталог содержит unit-тесты для библиотеки vlisp, использующие фреймворк **fiveam**.

Тесты оформлены как отдельная ASDF система **vlisp-tests** (см. `vlisp-tests.asd`).

## Структура

- `package.lisp` - определение пакета и основного набора тестов
- `vector-tests.lisp` - тесты векторных операций (vector+, vector-, normalize, и т.д.)
- `geometric-tests.lisp` - тесты геометрических функций (angle, distance, inters, polar)
- `utils.lisp` - вспомогательные функции для сравнения чисел и точек

## Запуск тестов

### Из REPL

```lisp
(asdf:test-system "vlisp-tests")
```

Или отдельно:

```lisp
(asdf:load-system "vlisp-tests")
(vlisp/tests:run-tests)
```

### Из командной строки

```bash
sbcl --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system \"vlisp-tests\")"
```

## Охват тестами

### Векторные функции ✅

- `vector-length` - 4 теста
- `vector+` - 3 теста
- `vector-` - 3 теста
- `normalize` - 3 теста
- `mid-point` - 3 теста
- `vector-dot-product` - 4 теста
- `vector-cross-product` - 3 теста

**Всего: 23 теста**

### Геометрические функции ✅

- `angle` - 5 тестов
- `distance` - 5 тестов
- `polar` - 5 тестов
- `inters` - 6 тестов

**Всего: 21 тест**

## Добавление новых тестов

1. Создайте новый файл `tests/your-tests.lisp` или добавьте в существующий
2. Обновите `vlisp.asd`, добавив компонент:
   ```lisp
   (:file "your-tests")
   ```
3. Используйте `def-suite` и `test` макросы из fiveam

Пример:

```lisp
(in-package #:vlisp/tests)

(def-suite your-suite
    :in vlisp-suite
    :description "Описание ваших тестов")

(in-suite your-suite)

(test your-test-name
  "Описание теста"
  (is (= 2 (+ 1 1))))
```

## Вспомогательные функции

### `approx-equal`

Сравнивает два числа с допустимой погрешностью:

```lisp
(approx-equal 3.14159 pi 1d-4) ; => T
```

### `approx-equal-list`

Сравнивает два списка чисел поэлементно:

```lisp
(approx-equal-list '(1 2 3) '(1.0001 2.0001 3.0001) 1d-3) ; => T
```

### `compare-points`

Удобный alias для сравнения координат точек:

```lisp
(compare-points '(1 2 3) '(1.0001 2.0001 3.0001) 1d-3) ; => T
```
