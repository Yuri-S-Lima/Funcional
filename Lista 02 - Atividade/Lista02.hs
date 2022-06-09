-- 1. Utilizando Expressões-ZF, implemente as listas abaixo.
-- a. Ímpares entre 1 e 100
impares :: [Int]
impares = [x | x <- [1..100], odd x]

-- b. Pares entre 10 e 100
pares :: [Int]
pares = [x | x <- [10..100], x > 10, even x]

-- c. Ímpares entre 1 e N
imparesN :: Int -> [Int]
imparesN n = [x | x <- [1..n], odd x]

-- d. Números entre 1 e N que são múltiplos de 3 e 5 ao mesmo tempo.
multiplos :: Int -> [Int]
multiplos n = [x | x <- [1..n], mod x 3 == 0, mod x 5 == 0]

-- e. Tuplas entre 1 e N, contendo o número e seu respectivo quadrado.
duplas :: Int -> [(Int, Int)]
duplas n = [(x, x * x) | x <- [1..n]]

-- f. Tuplas com os índices de uma matriz 3x4.
matriz :: [(Int, Int)]
matriz = [(x, y) | x <- [1..3], y <- [1..4]]

-- g. Tuplas com os índices de uma matriz NxM.
matrizNM :: Int -> Int -> [(Int, Int)]
matrizNM n m = [(x, y) | x <- [1..n], y <- [1..m]]

-- 2. Escreva uma função com a seguinte assinatura listaFibonacci :: Int->[Int] que retorna uma lista
-- com os n primeiros números da sequência de Fibonacci.
fib :: Int -> Int {-Função que retorna o fibonacci de n primeiros números-}
fib n
    |n == 1 = 0
    |n == 2 = 1
    |otherwise = fib(n - 1) + fib(n - 2)

listaFibonacci :: Int -> [Int]
listaFibonacci 0 = []
listaFibonacci n = [fib y | y <- [1..n]]