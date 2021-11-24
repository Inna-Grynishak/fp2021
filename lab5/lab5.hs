-- Лабораторна робота №5
-- студентки групи КН-31 підгрупи 1
-- Гринішак Інни

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення -
--  виведення. Набути досвiду компiляцiї Haskell-програм.


-- Варіант 5

-- 5) Визначити довжину послiдовностi тотожних елементiв списку, напр.:
-- "aaabbcaadddd"⇒ [(’a’,3), (’b’,2), (’c’,1), (’a’,2), (’d’,4)].

-- а) без застосування
len_elem2 :: Eq a => [a] -> [(a, Int)]
len_elem2 xs=zip h (map length l) where
    h = map head l 
    l = (group xs)


 -- 5.2.1. Реалiзувати та скомпiлювати одну з програм, розроблених у ЛР № 3 
 -- для Вашого варiанта з введенням даних: 
 -- а) з клавiатури, б) з файлу та виведенням результатiв: в) на екран, г) у файл.
keyToConsoleF1 :: IO ()
keyToConsoleF1 = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (len_elem2 x)
    putStrLn "Result" 
    print z
-- > keyToConsoleF1
-- Enter massive: 
-- [5,5,5,2,2,1,2,2]
-- Result
-- [(’5’,3), (’2’,2), (’1’,1), (’2’,2)]
-- [1,5,3,1,222]
keyToFileF1 :: IO ()
keyToFileF1 = do
    putStrLn "Enter massive: "
    numb1 <- getLine
    let x = read numb1 :: [Integer]
    z <- return (len_elem2 x)
    writeFile "len_elem_output.txt" ("Result " ++ show z)

-- 5) Знайти найбiльший спiльний дiльник двох чисел. 
-- b) без застосування вбудованих функцiй :

lcd2 0 b = b
lcd2 a b = if a>b then lcd2 (a-b) b
                  else lcd2 (b-a) a

-- > keyToFileF1
-- Enter massive: 
-- [1,1,1,5,5,3, 1,1,222]

keyToConsoleF2 :: IO ()
keyToConsoleF2 = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (lcd2 x y)
    putStrLn "Result" 
    print z
-- > keyToConsoleF2
-- Enter first number: 
-- 28
-- Enter second number: 
-- 42
-- Result
-- 14

keyToFileF2 :: IO ()
keyToFileF2 = do
    putStrLn "Enter first number: "
    numb1 <- getLine
    let x = read numb1 :: Integer
    putStrLn "Enter second number: "
    numb2 <- getLine
    let y = read numb2 :: Integer
    z <- return (lcd2 x y)
    writeFile "lcd_output.txt" ("Result " ++ show z)
-- > keyToFileF2
-- Enter first number: 
-- 42
-- Enter second number: 
-- 28

f :: String -> [Int]
f = read

ff :: String -> Int
ff = read

fff :: Char -> Int
fff = read . pure

fileToFileF1 :: IO ()
fileToFileF1 = do        
        s <- readFile "len_elem_input.txt"
        let r = len_elem2 (f s)
        writeFile "len_elem_output.txt" ("Result " ++ show r)
-- > fileToFileF1
fileToConsoleF1 :: IO ()
fileToConsoleF1 = do        
        s <- readFile "len_elem_input.txt"
        let r = f1len_elem2a s
        putStrLn "Result" 
        print r
-- > fileToConsoleF1
-- Result
-- "[(’5’,3), (’2’,2), (’1’,1), (’2’,2)]"
fileToFileF2 :: IO ()
fileToFileF2 = do        
        s <- readFile "lcd_input.txt"
        let numb1 = fff (head s)
        let numb2 = ff (tail s)
        let r = lcd2 numb1 numb2
        writeFile "lcd_output.txt" ("Result " ++ show r)
-- > fileToFileF2
fileToConsoleF2 :: IO ()
fileToConsoleF2 = do        
        s <- readFile "lcd_input.txt"
        let numb1 = fff (head s)
        let numb2 = ff (tail s)
        let r = lcd2 numb1 numb2
        putStrLn "Result" 
        print r
-- > fileToConsoleF2
-- Result
-- 14

-- Висновок: в ході лабораторної роботи ознайомились з модульною органiзацiєю
-- програм та засобами введення- виведення. Набули досвiду компiляцiї Haskell-програм.