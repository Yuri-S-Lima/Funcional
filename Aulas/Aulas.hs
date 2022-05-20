potencia :: Int -> Int -> Int {-Função Potência-}
potencia n m
    |m == 0 = 1
    |n == 1 = n
    |m > 0 = n * potencia n (m - 1)

divide :: Int -> Int -> Int {-Função que retorna a quantidade divisões feitas-}
divide x y
    |x == y = 1
    |x < y = 0
    |otherwise = 1 + divide (x - y) y

modulo :: Int -> Int -> Int {-Retorna o resto da divisão-}
modulo x y
    |x == y || y == 1 = 0
    |x < y = x
    |x > y = modulo (x - y) y

primo :: Int -> Bool {-Função Primo, verifica se determinado número é ou não primo-}
auxPrimo :: Int -> Int -> Bool
primo n
    |modulo n 2 == 0 && n /= 2 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)
    
auxPrimo x y
    |y == 1 = True
    |modulo x y == 0 = False
    |otherwise = auxPrimo x (y - 1)

algarismos :: Double -> Int {-Função que conta a quantidade de algarismos que formam determinado número-}
algarismos n
    |n < 10 = 1
    |otherwise = 1 + algarismos (n/10)

fatorial :: Int -> Int {-Função fatorial-}
fatorial n
    |n == 0 || n == 1 = 1
    |otherwise = n * fatorial (n - 1)

valorFat :: Int -> Int {-Função que retorna o valor do resultado de um fatorial-}
auxFat :: Int -> Int -> Int -> Int
valorFat m
    |m == 1 = 1
    |m == 2 = 2
    |otherwise = auxFat m 1 1

auxFat m n x
    |x == m = n
    |otherwise = auxFat m (n + 1) (x * (n + 1))

somaFat :: Int -> Int {-Função que soma os fatoriais no intervalo entre 0 e n-}
somaFat n
    |n == 0 = 1
    |otherwise = fatorial(n) + somaFat (n - 1)

ehPrimo :: Int -> Int -> Bool
ehPrimo m n
    |m == 1 || m == 2 = True
    |((mod m 2) == 0 || (mod m n) == 0) = False
    |((mod m n) /= 0) = ehPrimo m (n - 1)
