potencia :: Int -> Int -> Int
potencia n m
    |m == 0 = 1
    |m > 0 = n * potencia n (m - 1)

divide :: Int -> Int -> Int
divide x y
    |x == y = 1
    |x < y = 0
    |otherwise = 1 + divide (x - y) y

modulo :: Int -> Int -> Int
modulo x y
    |x == y || y == 1 = 0
    |x < y = x
    |x > y = modulo (x - y) y

primo :: Int -> Bool
auxPrimo :: Int -> Int -> Bool
primo n
    |modulo n 2 == 0 && n /= 2 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)
    
auxPrimo x y
    |y == 1 = True
    |modulo x y == 0 = False
    |otherwise = auxPrimo x (y - 1)

perfeito :: Int -> Bool
auxPerfeito :: Int -> Int -> Int -> Bool
perfeito n
    |n < 6 = False
    |otherwise = auxPerfeito n 0 1

auxPerfeito x y z
    |x == y = True
    |x < y || (x > y && z == x) = False
    |modulo x z == 0 = auxPerfeito x (y + z) (z + 1)
    |otherwise = auxPerfeito x y (z + 1)