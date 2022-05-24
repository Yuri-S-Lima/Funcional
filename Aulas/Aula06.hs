venda :: Int -> Int
venda 0 = 5
venda 1 = 6
venda 2 = 4
venda 3 = 8

-- Qual o total de vendas desde a semana 0 até a semana n?
somaVenda :: Int -> Int
somaVenda n
    |n == 0 = venda 0
    |n > 0 = soma
    |otherwise = 0
    where
        soma = somaVenda (n - 1) + venda n

-- Qual a maior venda semanal entre as semanas 0 e n?
maiorVenda :: Int -> Int
auxMV :: Int -> Int -> Int
maiorVenda n = auxMV 0 n

auxMV m n
    |n == -1 = m
    |venda n > m = maior
    |otherwise = seguindo
    where
        maior = auxMV (venda n) (n - 1); seguindo = auxMV m (n - 1)

-- Em que semana ocorreu a maior venda?
semanaMaior :: Int
auxSM :: Int -> Int -> Int
semanaMaior  = auxSM 0 3

auxSM m n
    |n == -1 = m
    |venda n > venda m = maior
    |otherwise = seguindo
    where
        maior = auxSM n (n - 1); seguindo = auxSM m (n - 1)

-- Existe alguma semana na qual nada foi vendido?
verificaSemana :: Int
auxVS :: Int -> Int -> Int
verificaSemana = auxVS 0 3

auxVS c n
    |n == -1 = c
    |venda n == 0 = auxVS (c + 1) (n - 1)
    |otherwise = auxVS c (n - 1)

-- Em qual semana não houve vendas? (se houve alguma)
semVenda :: Int
auxSV :: Int -> Int
semVenda = auxSV 3

auxSV n
    |n == -1 = -1
    |venda n == 0 = n
    |otherwise = auxSV (n - 1)