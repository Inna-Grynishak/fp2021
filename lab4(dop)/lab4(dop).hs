-- Лабораторна робота №4 (додаткове завдання)
-- студентки групи КН-31 підгрупи 1
-- Гринішак Інни

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.


-- Варіант 5
-- Задача 92

-- Завдання:
-- Фiгури на площинi. Використовуються такi фiгури, як 
-- коло (центр та радiус),
-- прямокутник (координати лiвої верхньої та правої нижньої точок), 
-- трикутник (координати вершин) та 
-- мiтка — label (координати лiвої нижньої точки, шрифт та рядок). 
-- Доступнi шрифти — Consolas, Lucida Console та Source Code Pro.
-- 
-- Визначте функцiю для перемiщення фiгури на вказаний вектор.

data Shape = 
    Circle Center Radius
  | Rectangle Point Point
  | Triangle Point Point Point
  | Label Point Font Line
  deriving (Show)

type Radius = Float
type Point = (Float, Float)
type Center = Point
type Font = (String, Width, Height)
type Width = Float
type Height = Float
type Line = NumberOfChars
type NumberOfChars = Integer

type Vector = (Float, Float)

addVectorToPoint :: Point -> Vector -> Point
addVectorToPoint (x1, y1) (v1, v2) = (x1 + v1, y1 + v2)

moveOnVector :: Shape -> Vector -> Shape
moveOnVector(Circle (x1, y1) r) (v1, v2) = Circle (addVectorToPoint(x1, y1) (v1, v2)) r
moveOnVector(Rectangle (x1, y1) (x2, y2)) (v1, v2) = Rectangle (addVectorToPoint(x1, y1) (v1, v2)) (addVectorToPoint(x2, y2) (v1, v2))
moveOnVector(Triangle (x1, y1) (x2, y2) (x3, y3)) (v1, v2) = Triangle (addVectorToPoint(x1, y1) (v1, v2)) (addVectorToPoint(x2, y2) (v1, v2)) (addVectorToPoint(x3, y3) (v1, v2))
moveOnVector(Label (x1, y1) (name, width, height) num_chars) (v1, v2) = Label (addVectorToPoint(x1, y1) (v1, v2)) (name, width, height) num_chars

-- тест
-- moveOnVector(Rectangle (1, 2) (2, 1)) (3, 4)

-- Висновок: в ході лабораторної роботи я ознайомилась з системою типiв та класiв типiв. 
-- Набула досвiду визначення нових типiв та класiв типiв i їх використання.
