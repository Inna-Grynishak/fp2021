-- Лабораторна робота №4
-- студентки групи КН-31 підгрупи 1
-- Гринішак Інни

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Варіант 5

-- Завдання:
-- Фiгури на площинi. Використовуються такi фiгури, як 
-- коло (центр та радiус),
-- прямокутник (координати лiвої верхньої та правої нижньої точок), 
-- трикутник (координати вершин) та 
-- мiтка — label (координати лiвої нижньої точки, шрифт та рядок). 
-- Доступнi шрифти — Consolas, Lucida Console та Source Code Pro.
-- 
-- Визначте функцiю для отримання прямокутникiв, що охоплюють кожну фiгуру 
-- iз заданого списку.


data Fonts = Consolas
            | Lucida_Console
            | Source_Code_Pro

data Figures = Circle Float Float Float
            | Rectangle Float Float Float Float
            | Triangle Float Float Float Float Float Float
            | Label Float Float Fonts String

instance Show Figures where
    show (Rectangle x1 y1 x2 y2) = " x1=" ++ show x1 ++ ", y1=" ++ show y1 ++ ", " ++ " x2=" ++ show x2 ++ ", y2=" ++ show y2 ++ ";  "

masRect = [(Rectangle 2 5 17 14),
            (Rectangle 9 2 18 20),
            (Rectangle 1 6 12 16)]

masFigures = [(Circle 8 8 2),
            (Rectangle 10 8 14 12),
            (Triangle 3 6 8 10 4 12),
            (Label 6 6 Consolas "String")]

rect :: [Figures] -> [Figures] -> [Figures]
rect [] masFigures = []
rect (rectangle:masRect) masFigures = if (rect1 rectangle masFigures) then rectangle : rect masRect masFigures
                                                                      else rect masRect masFigures

rect1 :: Figures -> [Figures] -> Bool
rect1 (Rectangle rx1 ry1 rx2 ry2) [] = True
rect1 (Rectangle rx1 ry1 rx2 ry2) ((Circle x1 y1 r):masFigures) = if x1-r>rx1 && x1+r<rx2 && y1-r<ry2 && y1+r>ry1 
                                                                    then rect1 (Rectangle rx1 ry1 rx2 ry2) masFigures 
                                                                    else False
rect1 (Rectangle rx1 ry1 rx2 ry2) ((Rectangle x1 y1 x2 y2):masFigures) = if rx1<x1 && ry1<y1 && rx2>x2 && ry2>y2
                                                                        then rect1 (Rectangle rx1 ry1 rx2 ry2) masFigures
                                                                        else False
rect1 (Rectangle rx1 ry1 rx2 ry2) ((Triangle x1 y1 x2 y2 x3 y3):masFigures) = if (x1>rx1 && x1<rx2) && (x2>rx1 && x2<rx2) && (x3>rx1 && x3<rx2) && 
                                                                                 (y1>ry1 && y1<ry2) && (y2>ry1 && y2<ry2) && (y3>ry1 && y3<ry2)
                                                                                then rect1 (Rectangle rx1 ry1 rx2 ry2) masFigures
                                                                                else False
rect1 (Rectangle rx1 ry1 rx2 ry2) ((Label x1 y1 font str):masFigures) = if y1>ry1 && (len' str)+y1<ry2 && (x1+1)>rx1 && x1<rx2
                                                                        then rect1 (Rectangle rx1 ry1 rx2 ry2) masFigures
                                                                        else False

len' :: [Char] -> Float
len' str = my_len str 0.0
my_len [] num = num
my_len (x:xs) num = my_len xs (num+1)


-- Тест
-- rect masRect masFigures


-- Висновок: в ході лабораторної роботи я ознайомилась з системою типiв та класiв типiв. 
-- Набула досвiду визначення нових типiв та класiв типiв i їх використання.
