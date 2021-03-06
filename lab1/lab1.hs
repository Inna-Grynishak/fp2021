-- Лабораторна робота №1
-- студентки групи КН-31 підгрупа 1
-- Гринішак Інна

-- Мета: Ознайомитись з основними типами мови. Ознайомитись зi структурою та функцiями Glasgow Haskell Compiller. 
-- Набути навичок роботи з iнтерпретатором ghci та визначення найпростiших функцiй.

-- Варіант 5

-- Завдання 1. Наведiть приклади виразiв вказаного типу.

-- 5) ([Char],[Double],[(Bool,Integer)])
firstTask = (['b'], [3.222, 4.32234],[(True, 1)])


-- Завдання 2. Визначте два варiанти вказаних далi функцiй.
-- Перший варiант – з одним аргументом-кортежем, другий – без використання кортежiв чи спискiв. 

-- 5) Функцiя визначає, чи належить кругу дана точка. Точка задається координатами, 
-- круг – координатами центра та радiусом.

func1 :: (Integer, Integer, Integer, Integer, Integer) -> Bool
func1 (x, y, xc, yc, r) = if (xc-x)*(xc-x)+(yc-y)*(yc-y) <= r*r then True
                         else False

func2 :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
func2 x y xc yc r = if (xc-x)*(xc-x)+(yc-y)*(yc-y) <= r*r then True
                         else False

-- Висновок: в ході лабораторної роботи ознайомилися із структурою і функціями GHC. Набули навички 
-- роботи з інтерпретатором ghci та навчилися писати найпростіші функції.