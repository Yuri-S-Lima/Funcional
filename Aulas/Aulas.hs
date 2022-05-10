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

algarismos :: Double -> Int
algarismos n
    |n < 10 = 1
    |otherwise = 1 + algarismos (n/10)

fatorial :: Int -> Int
fatorial n
    |n == 0 || n == 1 = 1
    |otherwise = n * fatorial (n - 1)

valorFat :: Int -> Int
auxFat :: Int -> Int -> Int -> Int
valorFat m
    |m == 1 = 1
    |m == 2 = 2
    |otherwise = auxFat m 1 1

auxFat m n x
    |x == m = n
    |otherwise = auxFat m (n + 1) (x * (n + 1))

somaFat :: Int -> Int
somaFat n
    |n == 0 = 1
    |otherwise = fatorial(n) + somaFat (n - 1)

fib :: Int -> Int
aFib :: Int -> Int -> Int -> Int
fib n
    |n == 0 = 0
    |n == 1 = 1
    |otherwise = aFib n 1 1

aFib n m p
    |p == n = p
    |otherwise = aFib n p (m + p)
