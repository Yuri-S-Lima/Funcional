primo :: Int -> Bool {-Função Primo, verifica se determinado número é ou não primo-}
auxPrimo :: Int -> Int -> Bool
primo n
    |mod n 2 == 0 && n /= 2 || n == 1 = False
    |n == 2 = True
    |otherwise = auxPrimo n (n-1)
    
auxPrimo x y
    |y == 1 = True
    |mod x y == 0 = False
    |otherwise = auxPrimo x (y - 1)

-- 1° a. Receba dois números e retorne um valor booleano informando se eles constituem um par de números primos gêmeos.
pGemeos :: Int -> Int -> Bool
pGemeos x y
    |x == y || y - x /= 2 = False
    |primo x && primo y = True
    |otherwise = False

-- b. Receba um determinado número p e retorne um valor booleano informando se ele pertence a algum par de número primo gêmeo.
ehPGemeo :: Int -> Bool
ehPGemeo p
    |p == 2 || primo p == False = False
    |primo (p - 2) || primo (p + 2) = True
    |otherwise = False

-- c. Contabilize quantos pares de primos gêmeos existem abaixo de um número n.
contaPar :: Int -> Int
aux :: Int -> Int -> Int
contaPar c = aux 3 c

aux p c
    |p >= c = 0
    |ehPGemeo p = 1 + aux(p + 3) c
    |otherwise = aux(p + 1) c

-- d. Calcule a soma de todos os primos gêmeos que aparecem entre 0 e n.
soma :: Int -> Int
auxS :: Int -> Int -> Int -> Int -> Int
soma n = auxS n 3 5 0

auxS n p q s -- c = intervalo n buscado | p = primeiro primo | q = segundo primo | s = soma
    |n == 5 = 3
    |q >= n = s
    |pGemeos p q = auxS n (p + 2) (q + 2) (s + (p + q))
    |otherwise = auxS n (p + 2) (q + 2) s