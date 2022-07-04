-- Retorne a lista dos quadrados dos elementos de L
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
quadrado :: [Int] -> [Int]
quadrado = map(^2)

-- Retorne a soma dos quadrados dos elementos de L
soma :: [Int] -> Int
soma = sum.quadrado

-- Verifique se todos os elementos da lista são positivos
verificaPadrao :: [Bool] -> Bool
verificaPadrao (x:xs)
    |null xs && not x = False
    |null xs = True
    |not x = False
    |otherwise = verificaPadrao xs

pos :: [Int] -> [Bool]
pos = map(<0)

positivos :: [Int] -> Bool
positivos l = verificaPadrao (pos l)

-- O valor mínimo de uma função aplicada a uma lista de inteiros de 0 a n
multiplica :: [Int] -> [Int]
multiplica = map(*2)

listaMult :: [Int]
listaMult = [33,12,35,81,23]

minimo :: Int
minimo = minimum (multiplica listaMult)

-- Teste se o resultado de uma função f aplicada sobre as entradas de 0 a n são todas iguais
compara :: Int -> [Int] -> Bool
compara x (z:zs)
    |null zs && z == x = True
    |z == x = compara x zs
    |otherwise = False

seIgual :: Bool
seIgual = compara (head (quadrado listaMult)) (quadrado listaMult)