{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (nub)
-- 1. Você está desenvolvendo um novo algoritmo de compressão de dados em Haskell e deseja
-- analisar quais dados mais se repetem em uma determinada lista. Sendo assim, você resolve
-- criar a função analise que organiza os dados analisados em uma lista de tuplas contendo cada
-- elemento e sua respectiva contagem de repetições. (2,0)
-- Exemplo:
-- analise "IFMA CAXIAS" -> [('I',2),('F',1),('M',1),('A',3),(' ',1),('C',1),('A',3),('X',1),('I',2),('A',3), (’S’,1)]

type Retorno = (Char, Int)

contagem :: Char -> String -> Int
contagem s (x:xs)
    |null xs && (x == s) = 1
    |null xs = 0
    |s /= x = contagem s xs
    |otherwise = 1 + contagem s xs

analise :: [Char] -> [Retorno]
analise str = [ (y, contagem y str ) | y <- str]

-- 2. Suponha que tenhamos uma lista de caracteres e que desejamos ordenar seus elementos de
-- acordo com suas ocorrências. Neste caso, teremos os elementos mais frequentes no início da
-- lista e os mais raros posicionados no fim. (2,0)
-- Exemplo:
-- OrdenaFreq "IFMA CAXIAS" -> “AIFM CXS"
type Frequencia = (Int, Char)

pegChar :: Frequencia -> Char
pegChar (_, c) = c

ordenaStr :: [Frequencia] -> [Frequencia]
ordenaStr [ ] = [ ]
ordenaStr (x : xs) = ordenaStr [y | y <- xs, y >= x]
                    ++ [x] ++
                    ordenaStr [y | y <- xs, y < x]

percorre :: Char -> String -> String
percorre c str = [s | s <- str, s /= c]

ordenaFreq :: [Char] -> String
ordenaFreq str 
    |length str > 1 =  head[pegChar x | x <- ordenaStr[(contagem y str, y) | y <- str]]
                      : ordenaFreq (percorre (head[pegChar x | x <- ordenaStr[(contagem y str, y) | y <- str]]) [ pegChar x | x <- ordenaStr[ (contagem y str, y) | y <- str]])
    |otherwise = str 

--ordenaFreq str = nub ([pegChar x | x <- ordenaStr[(contagem y str, y) | y <- str]])