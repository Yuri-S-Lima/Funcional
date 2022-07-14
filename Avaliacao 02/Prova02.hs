-- Considere um  algoritmo de compressão do tipo Lossless que quantifica o número de repetições consecutivas 
-- de  um  caractere  para  reduzir  o  tamanho  de  uma  string.  Ou  seja,  sempre  que  o  caractere 
-- encontrar uma letra repetida, ele deve substituir esta string pela letra em questão seguida da 
-- quantidade de repetições. Por exemplo, se o algoritmo encontrar 5 caracteres “x” em 
-- sequência  (“xxxxx”),  ele  deve  substituí-los  pela  string  “x5”,  se  forem  4  caracteres  “z”  em 
-- sequência  (“zzzz”)  origina-se  a  string  “z4”,  e  assim  por  diante.  Com  base  no  exposto,  escreva 
-- uma  função  em  Haskell  que  receba  um  texto  e  gere  como  saída  sua  versão  comprimida 
-- utilizando o método descrito acima. (3,0) 
-- Exemplos: 
-- “abcaaaabccaaadeeeeeeeee” -> “abca4bc2a3de9” 
-- “ccassssadddaa” -> “c2as4ad3a2”
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
contador :: Eq a => a -> [a] -> Int
contador c (x:xs)
    |null xs && c == x = 1
    |c == x = 1 + contador c xs
    |otherwise = 0

lossless :: String -> String
lossless [] = []
lossless (x:xs)
    |contador x (x:xs) > 1 = x : show (contador x (x:xs)) ++ lossless (drop (contador x (x:xs)) (x:xs))
    |otherwise = x : lossless xs


-- Considere a necessidade de realizar a análise de dados em outras abordagens de compressão, 
-- implemente  uma  função  polimórfica  que  receba  uma  lista  de  um  tipo  de  dado  qualquer  e 
-- retorne uma série de tuplas indicando o número de repetições consecutivas de cada elemento, 
-- mas organizados pela sua ordem de aparição. (3,0) 
-- “abcaaaabccaaadeeeeeeeee” -> [(a,1), (a,4), (a,3), (b,1), (b,1), (c,1), (c,2), (d,1), (e,9)] 
-- [2,2,2,1,3,3,3,3,2,1,1,3] -> [(2,3), (2,1), (1,1), (1,2), (3,4), (3,1)]

repeticoes :: Eq a => [a] -> [(a, Int)]
repeticoes [] = []
repeticoes (x:xs) = [ (x, contador x (x:xs)) ] ++ repeticoes (drop (contador x (x:xs)) (x:xs))