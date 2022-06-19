{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

-- 3. Escreva uma função
-- em Haskell que receba uma String em binário e retorne uma String com sua representação hexadecimal.
type Binario = (String, Char)
conv :: [Binario]
conv = [("0000", '0'),("0001", '1'), ("0010", '2'), ("0011", '3'), ("0100", '4'), ("0101", '5'), ("0110", '6'), ("0111", '7'), ("1000", '8'), ("1001", '9'), ("1010", 'A'), ("1011", 'B'), ("1100", 'C'), ("1101", 'D'), ("1110", 'E'), ("1111", 'F')]

verifica :: String -> String
verifica s
    |mod (length s) 4 /= 0 && mod (length s) 4 == 1 = "000" ++ s
    |mod (length s) 4 /= 0 && mod (length s) 4 == 2 = "00" ++ s
    |mod (length s) 4 /= 0 && mod (length s) 4 == 3 = "0" ++ s
    |otherwise = s

voltaHex :: Binario -> Char
voltaHex (bin, hex) = hex

buscaBin :: Binario -> String
buscaBin (bin, hex) = bin

binHexa :: String -> [Binario] -> String
binHexa b (x:xs)
    |null xs = [voltaHex x]
    |b == buscaBin x = [voltaHex x]
    |otherwise = binHexa b xs

converteStr :: [String] -> String
converteStr (x:xs)
    |null xs = x
    |otherwise = x ++ converteStr xs

hexZF :: String -> String
hexZF [] = ""
hexZF str = [t | s <- [verifica str], t <- binHexa (converteStr [take 4 s]) conv ] ++ hexZF (drop 4 (verifica str))

-- Resolução sem ZF
hex :: String -> String
hex str
    |null str = ""
    |otherwise = binHexa (take 4 (verifica str)) conv ++ hex (drop 4 (verifica str))