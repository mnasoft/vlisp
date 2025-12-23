# Геометрические примитивы 2D (vlisp/geometry)

Модуль для работы с двумерными геометрическими примитивами на основе CLOS (Common Lisp Object System).

## Классы

### point-2d
Точка в двумерном пространстве.

**Слоты:**
- `x` - X-координата
- `y` - Y-координата

**Пример:**
```lisp
(make-instance 'vlisp/geometry:point-2d :x 10.0 :y 20.0)
```

### line-segment
Отрезок прямой.

**Слоты:**
- `start-point` - начальная точка (point-2d)
- `end-point` - конечная точка (point-2d)

**Методы:**
- `(perimeter seg)` - длина отрезка

**Пример:**
```lisp
(make-instance 'vlisp/geometry:line-segment
               :start-point (make-instance 'vlisp/geometry:point-2d :x 0 :y 0)
               :end-point (make-instance 'vlisp/geometry:point-2d :x 3 :y 4))
```

### circle
Окружность.

**Слоты:**
- `center` - центр окружности (point-2d)
- `radius` - радиус

**Методы:**
- `(perimeter circ)` - длина окружности (периметр)
- `(area circ)` - площадь окружности

**Пример:**
```lisp
(make-instance 'vlisp/geometry:circle
               :center (make-instance 'vlisp/geometry:point-2d :x 0 :y 0)
               :radius 5.0)
```

### arc
Дуга окружности.

**Слоты:**
- `arc-center` - центр дуги (point-2d)
- `arc-radius` - радиус дуги
- `start-angle` - начальный угол (в радианах)
- `end-angle` - конечный угол (в радианах)

**Методы:**
- `(perimeter a)` - длина дуги

**Пример:**
```lisp
(make-instance 'vlisp/geometry:arc
               :arc-center (make-instance 'vlisp/geometry:point-2d :x 0 :y 0)
               :arc-radius 5.0
               :start-angle 0.0
               :end-angle (/ pi 2))
```

## Функции

### distance
Вычисляет расстояние между объектами.

**Перегруженные версии:**
- `(distance pt1 pt2)` - расстояние между двумя точками
- `(distance pt seg)` - расстояние от точки до отрезка (кратчайшее)
- `(distance pt circ)` - расстояние от точки до окружности

**Примеры:**
```lisp
;; Расстояние между точками
(distance (make-instance 'vlisp/geometry:point-2d :x 0 :y 0)
          (make-instance 'vlisp/geometry:point-2d :x 3 :y 4))
; => 5.0

;; Расстояние от точки до окружности
(distance pt circle)
```

## Использование

```lisp
(asdf:load-system "vlisp/geometry")

(use-package :vlisp/geometry)

;; Создаем точки
(defvar p1 (make-instance 'point-2d :x 0 :y 0))
(defvar p2 (make-instance 'point-2d :x 10 :y 10))

;; Расстояние между точками
(distance p1 p2)
; => 14.142135623730951

;; Создаем отрезок
(defvar seg (make-instance 'line-segment :start-point p1 :end-point p2))
(perimeter seg)
; => 14.142135623730951

;; Создаем окружность
(defvar circ (make-instance 'circle 
                            :center p1 
                            :radius 5.0))
(area circ)
; => 78.53981633974483

;; Создаем дугу
(defvar arc (make-instance 'arc
                           :arc-center p1
                           :arc-radius 5.0
                           :start-angle 0.0
                           :end-angle (/ pi 2)))
(perimeter arc)
; => 7.853981633974483
```

## Структура файлов

- `package.lisp` - определение пакета и экспортируемых символов
- `primitives.lisp` - определение классов и методов для геометрических примитивов
