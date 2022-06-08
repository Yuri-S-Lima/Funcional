{-Questão 1-}
potencia :: Int -> Int -> Int {-Função Potência-}
potencia n m
    |m == 0 = 1
    |n == 1 = n
    |otherwise = n * potencia n (m - 1)

{-Questão 2-}
primo :: Int -> Bool {-Função Primo, verifica se determinado número é ou não primo-}
auxPrimo :: Int -> Int -> Bool
primo n
    |mod n 2 == 0 && n /= 2 = False
    |n == 1 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)
    
auxPrimo x y
    |y == 1 = True
    |mod x y == 0 = False
    |otherwise = auxPrimo x (y - 1)

nPrimo :: Int -> Int {-Função que retorna o n-ésimo primo-}
cont :: Int -> Int -> Int
nPrimo n = cont n 2

cont n p
    |n == 0 = p - 1
    |primo p = cont (n - 1) (p + 1)
    |otherwise = cont n (p + 1)

{-Questão 3-}
fib :: Int -> Int {-Função que retorna o fibonacci de n-}
fib n
    |n == 1 = 0
    |n == 2 = 1
    |otherwise = fib(n - 1) + fib(n - 2)

fibPrimo :: Int -> Int {-Função que retorna o e-ésimo primo da sequência de Fibonacci-}
auxFP :: Int -> Int -> Int
fibPrimo n = auxFP n 1

auxFP n p
    |n == 0 = fib (p - 1)
    |primo (fib(p)) = auxFP (n - 1)(p + 1)
    |otherwise = auxFP n (p + 1)

{-Questão 4-}
{-Letra A-}
resto :: Int -> Int -> Int {-Função que retorna o resto da divisão entre dois inteiros-}
resto x y
    |x == y || y == 1 || x < y = 0
    |otherwise = mod x y

{-Letra B-}
divide :: Int -> Int -> Int {-Função que retorna a divisão inteira-}
divide x y
    |mod x y /= 0 = 0
    |otherwise = div x y

{-Letra C-}
mdc :: Int -> Int -> Int
auxMDC :: Int -> Int -> Int -> Int -> Int
mdc x y
    |mod x y == 0 = y
    |mod y x == 0 = x
    |otherwise = auxMDC x y 2 1

auxMDC x y z c
    |x == 1 && y == 1 = c
    |mod x z == 0 && mod y z == 0 = auxMDC (div x z) (div y z) (z) (z * c)
    |mod x z == 0 && mod y z /= 0 = auxMDC (div x z) (y) (z) (c)
    |mod x z /= 0 && mod y z == 0 = auxMDC (x) (div y z) (z) (c)
    |z == 2 = auxMDC x y (z + 1) c
    |otherwise = auxMDC x y (z + 2) c

{-Letra D-}
mmc :: Int -> Int -> Int
auxMMC :: Int -> Int -> Int -> Int -> Int
mmc x y = auxMMC x y 2 1

auxMMC x y z c
    |x == 1 && y == 1 = c
    |mod x z == 0 && mod y z == 0 = auxMMC (div x z) (div y z) (z) (z * c)
    |mod x z == 0 && mod y z /= 0 = auxMMC (div x z) (y) (z) (c * z)
    |mod x z /= 0 && mod y z == 0 = auxMMC (x) (div y z) (z) (c * z)
    |z == 2 = auxMMC x y (z + 1) c
    |otherwise = auxMMC x y (z + 2) c

{-Questão 5-}
perfeito :: Int -> Bool {-Função que verifica se determinado número é ou não perfeito-}
auxPerfeito :: Int -> Int -> Int -> Bool
perfeito n
    |n < 6 = False
    |otherwise = auxPerfeito n 0 1

auxPerfeito x y z
    |x == y = True
    |x < y || (x > y && z == x) = False
    |mod x z == 0 = auxPerfeito x (y + z) (z + 1)
    |otherwise = auxPerfeito x y (z + 1)