salario :: Int -> Float
salario 1 = 772.25
salario 2 = 2375.0
salario 3 = 1778.5
salario 4 = 6520.0
salario 5 = 3447.35
salario 6 = 5225.75
salario 7 = 8932.0
salario 8 = 648.5
salario 9 = 1982.4
salario 10 = 2557.45
salario _ = 0

-- 1° a. Informar o quanto a empresa gasta com salários mensalmente.
gasto :: Float
soma :: Int -> Float

gasto = soma 1

soma p
    |p == 10 = din
    |otherwise = din + soma (p + 1)
    where
        din = salario p


-- b. Receber um determinado valor n e informar quantas pessoas recebem salário acima de n.
valor :: Float -> Int
contador :: Int -> Float -> Int

valor n = contador 1 n

contador x n 
    |x == 11 = 0
    |salario x > n = 1 + busca
    |otherwise = busca
    where
        busca = contador (x + 1) n

-- c. Receber um determinado valor n e informar qual salário mais se aproxima deste valor.
aproxima :: Float -> Float
verifica :: Int -> Float -> Float -> Float -> Float

aproxima v = verifica 1 v 0 v

verifica c v s d -- c = contador | v = valor buscado | s = salário final | d = diferença
    |c == 11 = s
    |d > diferenca = verifica (c + 1) v (salario c) diferenca
    |otherwise = verifica (c + 1) v s d
    where
        diferenca = abs(salario c - v)

-- d. Considerando que existem contribuições previdenciárias variáveis para cada faixa salarial,
-- informe o total a ser custeado pela empresa sabendo que salários menores que
-- R$1.100,00 são isentos, salários entre R$ 1.100,01 e R$ 2.203,48 pagam 9%, entre R$
-- 2.203,49 e R$ 3.305,22 pagam 12%, entre R$ 3.305,23 e R$ 6.433,57 pagam 14% e
-- salários acima de R$ 6.433,57 contribuem com 22%.
total :: Float
calcula :: Int -> Float

total = calcula 1

calcula n
    |n == 11 = 0
    |din <= 1100.00 = fun
    |din <= 2203.48 = (din * 0.09) + fun
    |din <= 3305.22 = (din * 0.12) + fun
    |din <= 6433.57 = (din * 0.14) + fun
    |otherwise = (din * 0.22) + fun
    where
        din = salario n; fun = calcula (n + 1)