-- Exemplo 04 - Ler e somar números
main :: IO()
main = do
    putStrLn "Digite um número"
    s1 <- getLine
    putStrLn "Digite outro número"
    s2 <- getLine

    putStrLn "Soma dos números"
    let n1 = read s1 :: Double
    let n2 = read s2 :: Double

    print (n1 + n2)