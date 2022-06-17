{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
type Funcionario = (String, Float)

folha :: [Funcionario]
folha = [("Ana", 772.25), ("Jose", 2375.0), ("Santos", 1778.5)]

menor :: Funcionario
menor = minimum folha -- retorna o menor valor

salario :: Funcionario -> Float
salario (nome, sal) = sal

-- 1° a. Informar o quanto a empresa gasta com salários mensalmente.
gastoMensal :: [Funcionario] -> Float
gastoMensal (x:xs)
    |null xs = salario x
    |otherwise = salario x + gastoMensal xs

-- com zf
gastoM :: [Funcionario] -> Float
gastoM folha = sum ([y | (_,y) <- folha])

-- b. Receber um determinado valor n e informar quantas pessoas recebem salário acima de n.
quantidade :: Float -> [Funcionario] -> Int
quantidade n (x:xs)
    |null xs && salario x > n = 1
    |null xs = 0
    |salario x <= n = quantidade n xs
    |otherwise = 1 + quantidade n xs

-- com zf
quantidadeZF :: Float -> [Funcionario] -> Int
quantidadeZF n l = length([x |(_,x) <- l, x > n])

-- c. Receber um determinado valor n e informar qual salário mais se aproxima deste valor.
type Diferenca = (Float, Float)
salarioDiff :: Diferenca -> Float
salarioDiff (_, sal) = sal

auxSal :: [Funcionario] -> Float -> [Diferenca]
auxSal ((_, sal):xs) n
    |null xs = [(abs(sal - n), sal)]
    |otherwise = (abs(sal - n), sal) : auxSal xs n

salarioProx :: Float -> Float
salarioProx n = salarioDiff (minimum (auxSal folha n))

-- com zf
salProxZF :: Float -> [Funcionario] -> Funcionario
salProxZF n lista = lista!!pos
    where
        lista_difs = [(abs(salario(lista!!i) - n), i) | i <- [0..length lista-1]]
        (diff, pos) = minimum lista_difs

-- d. Considerando que existem contribuições previdenciárias variáveis para cada faixa salarial,
-- informe o total a ser custeado pela empresa sabendo que salários menores que R$1.100,00 são isentos, 
-- salários entre R$ 1.100,01 e R$ 2.203,48 pagam 9%, entre R$ 2.203,49 e R$ 3.305,22 pagam 12%, entre R$ 3.305,23 e R$ 6.433,57 pagam 14% e
-- salários acima de R$ 6.433,57 contribuem com 22%.
type Taxa = (Float, Float)
listaValTax :: [Taxa]
listaValTax = [(0, 0), (1000.01, 0.09), (2203.49, 0.12), (3305.23, 0.14), (6433.57, 0.22)]

buscaSal :: Taxa -> Float
buscaSal (sal, tax) = sal

salTax :: Taxa -> Float
salTax (sal, tax) = tax

verificaSal :: Float -> [Taxa] -> Float -> Float
verificaSal s (x:xs) p -- s -> salário, p -> porcentagem
    |null xs = (s * salTax x) + s
    |s >= buscaSal x = verificaSal s xs (salTax x)
    |otherwise = (s * p) + s

totalGasto ::  [Funcionario] -> Float
totalGasto ((_, sal):xs)
    |null xs = verificaSal sal listaValTax 0
    |otherwise = verificaSal sal listaValTax 0 + totalGasto xs

-- com zf
totalGastoZF :: [Funcionario] -> Float
totalGastoZF lista = sum([verificaSal (salario(lista !! x)) listaValTax 0 |x <- [0..length lista - 1]])