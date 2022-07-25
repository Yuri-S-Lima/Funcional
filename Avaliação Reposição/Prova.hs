{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
primo :: Int -> Bool
auxPrimo :: Int -> Int -> Bool
primo n
    |even n && n /= 2 = False
    |n == 1 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)

auxPrimo x y
    |y == 1 = True
    |mod x y == 0 = False
    |otherwise = auxPrimo x (y - 1)

nPrimo :: Int -> Int
cont :: Int -> Int -> Int
nPrimo n = cont n 2

cont n p
    |n == 0 = p - 1
    |primo p = cont (n - 1) (p + 1)
    |otherwise = cont n (p + 1)

multPrimos :: Int -> Int
multPrimos n = product[(nPrimo x) | x <- [1..n]]

-- 1°
afortunado :: Int -> Int
afortunado n = res (multPrimos n) 2

res :: Int -> Int -> Int
res x m
    |primo (x + m) = m
    |otherwise = res x (m + 1)

-- 2  
getByIndex :: [Int] -> String -> String
getByIndex []_ = []
getByIndex (x:xs) s 
    |not (null xs) && (x < 0 || x > (length s) ) = "*" ++ getByIndex xs s
    |x >= 0 = s!!x : getByIndex xs s
    |otherwise = [s!!x]

-- 3°
pascal :: Int -> [(Int, Int)]
pascal n = [(n, y) |  y <- [0..n]]