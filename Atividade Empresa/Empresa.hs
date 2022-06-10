{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
type Funcionario = (String, Float)

folha :: [Funcionario]
folha = [("Ana", 772.25), ("Jose", 2375.0), ("Santos", 1778.5)]

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
