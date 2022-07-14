{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

listaPositivos :: [Int]
listaPositivos = [0,5..10000]

listaInt :: [Int]
listaInt = [0..100]
-- 1. Escreva uma função que retorne se um número pertence ou não a uma lista
type Retorno = (Int, Bool)

verifica :: Int -> [Int] -> Bool
verifica n (x:xs)
    |null xs && (x /= n) = False
    |null xs = True
    |x /= n = verifica n xs
    |otherwise = True

pertence :: Int -> Retorno
pertence n = (n, verifica n [1,3..500])

pertenceV2 :: Int -> Bool
pertenceV2 n = or [n == l | l <- [1,3..500]]

-- com alta ordem e polimorfismo
pertenceV3 :: Eq a => a -> [a] -> Bool
pertenceV3 n l = or (map (== n) l)

-- 2. Escreva uma função que retorne o maior elemento de uma lista de inteiros.
buscaMaior :: [Int] -> Int -> Int
buscaMaior (x:xs) n
    |null xs && (n > x) = n
    |null xs = x
    |n > x = buscaMaior xs n
    |otherwise = buscaMaior xs x

maiorElemento :: [Int] -> Int
maiorElemento l = buscaMaior l 0

maiorInt :: [Int] -> Int
maiorInt [] = 0
maiorInt (x : xs)
  | x >= maiorInt xs = x
  | otherwise = maiorInt xs

-- 3. Escreva uma função que receba um número inteiro positivo n,
-- uma lista e retorne o n-ésimo elemento da lista.
nElemento :: Int -> [Int] -> Int
nElemento n lista = lista!!n

-- com polimorfismo
nElemento2 :: Int -> [a] -> a
nElemento2 n lista = lista!!n

-- 4. Escreva uma função que retire o n-ésimo elemento de uma lista.
-- Exemplo: "abcdefghi" 3 -> "abdefghi"
retira :: [Int] -> Int -> [Int]
retira lista n = [x | x <- lista, x /= lista !! n]

-- com polimorfismo e alta ordem
retiraV2 :: Eq a => [a] -> Int -> [a]
retiraV2 lista n = filter(\x -> x /= lista !! n) lista

-- 5. Escreva uma função que receba uma frase e diga se esta é um palíndromo. 
-- Para verificar se uma frase é palíndromo basta verificar se ela é igual à sua reversa 
-- (implemente a função reverse).
type RetPalindromo = (String, Bool)

verificaRev :: String -> Bool
verificaRev str
    |str == reverse str = True
    |otherwise = False

palindromo :: String -> RetPalindromo
palindromo str = (str, verificaRev str)

-- 6. Escreva uma função que elimina caracteres repetidos. Eles devem
-- ser substituídos por uma única cópia do elemento e a ordem dos
-- elementos não deve ser alterada. Ex.: "aaaabccaadeeee" -> "abcade"
compara :: String -> String -> String
compara sc sk
    |sc /= sk = sc
    |otherwise = ""

elimina :: String -> String
elimina str
    |length str > 1 = compara [head str] [head (tail str)] ++ elimina (tail str)
    |otherwise = str

eliminaV2 :: String -> String
eliminaV2 [] = []
eliminaV2 (x:xs) =  x : eliminaV2 [a | a <- x : xs, a /= x]

-- com polimorfismo
eliminaV3 :: Eq a => [a] -> [a]
eliminaV3 [] = []
eliminaV3 (x:xs) = x : eliminaV3 [a | a <- x : xs, a /= x]

-- 7. Escreva uma função que duplique cada elemento de uma lista.
-- Exemplo: [1, 2, 3] -> [1,1,2,2,3,3]
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = x : x : duplica xs

-- com polimorfismo
duplicaV2 :: [a] -> [a]
duplicaV2 [] = []
duplicaV2 (x:xs) = x : x : duplicaV2 xs

-- 8. Escreva uma função que recebe uma String e retorna a primeira
-- palavra dessa String sem contar pontuação.
-- Exemplo: “Olá, mundo” -> “Olá”
palavra :: String -> String
palavra (x:xs)
    |null xs && verificacao = ""
    |null xs = [x]
    |verificacao = ""
    |otherwise = x : palavra xs
    where
        verificacao = x == ',' || x == '.' || x == ';' || x == ' '

palavraV2 :: String -> String
palavraV2 [] = []
palavraV2 (x:xs)
    |verificacao = []
    |otherwise = x : palavraV2 xs 
    where
        verificacao = x == ',' || x == '.' || x == ';' || x == ' '

-- 9. Implemente uma função que move todos os elementos de uma
-- lista para a direita.Exemplo:
-- moverDireita [ 'a', 'b', 'c' ] 0 -> [ 'a', 'b', 'c' ]
-- moverDireita [ 'a', 'b', 'c' ] 1 -> [ 'c', 'a', 'b' ]
-- moverDireita [ 'a', 'b', 'c' ] 2 -> ['b', 'c', 'a']
troca :: Int -> [Int] -> [Int]
troca t l
    |t > 1 = troca (t - 1) (head (reverse l) : reverse(tail (reverse l)))
    |otherwise = l

moverDireita :: [Int] -> [Int]
moverDireita lista = troca (length lista) lista

-- com polimorfismo

trocaV2 :: Int -> [a] -> [a]
trocaV2 t l
    |t > 1 = trocaV2 (t - 1) (head (reverse l) : reverse(tail (reverse l)))
    |otherwise = l

moverDireitaV2 :: [a] -> [a]
moverDireitaV2 lista = trocaV2 (length lista) lista

-- 10. Implemente uma função que recebe duas listas sem elementos
-- repetidos e retorna uma lista com elementos comuns entre elas.
-- Exemplo: intercede [1,2,3,4] [2,3,4,5] -> [2,3,4]
lista1 :: [Int]
lista1 = [1,2..100]

lista2 :: [Int]
lista2 = [0,3..100]

intercede :: [Int] -> [Int] -> [Int]
intercede l1 l2 = [x |x <- l1, y <- l2, x == y]

-- com polimorfismo
intercedeV2 :: Eq a => [a] -> [a] -> [a]
intercedeV2 l1 l2 = [x | x <- l1, y <- l2, x == y]

-- 11. Implemente a função 'split', que recebe um número inteiro n e uma
-- lista de números inteiros e retorna uma tupla onde o primeiro
-- elemento é uma lista dos itens maiores que n e o segundo
-- elemento é uma lista dos itens menores.
-- Exemplo: split 5 -> [1,2,3,4,5,6,7,8] -> ([6,7,8],[1,2,3,4,5])
type Itens = ([Int],[Int])

listaVinte :: [Int]
listaVinte = [1..20]

split :: Int -> [Int] -> Itens
split n lista = ([x | x <- lista, x > n], [y | y <- lista, y <= n])

-- com polimorfismo
splitV2 :: Ord a => a -> [a] -> ([a],[a])
splitV2 n lista = ([x | x <- lista, x > n], [y | y <- lista, y <= n])

-- 12. Escreva uma função que dados dois índices, m e n, extraia da
-- lista os elementos compreendidos entre entre esses valores, onde
-- ambos os limites estão incluídos. Comece a contar os elementos
-- do 1. Exemplo:['a','b','c','d','e','f','g','h','i','k'] 3 7 -> "cdefg"
elementosComp :: String -> Int -> Int -> String
elementosComp lista m n = [lista !! t |t <- [0..length lista - 1], t >= m && t <= n]

-- 13. Escreva uma função que empacote duplicatas consecutivas de elementos de lista em sublistas.
-- Se uma lista contém elementos repetidos, eles devem ser colocados em sublistas separadas.
-- Exemplo:['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] -> ["aaaa","b","cc","aa","d","eeee"]
empacotando :: String -> [String] -> String
empacotando p (x:xs)
    |null xs && p == x = x
    |null xs = []
    |p /= x && not (null xs) = []
    |otherwise = x ++ empacotando p xs

quebraListaE :: String -> [String] -> [String]
quebraListaE p (x:xs)
    |null xs && p == x = []
    |null xs = [x]
    |p /= x && not (null xs) = x : xs
    |otherwise = quebraListaE p xs

empacotaDuplicata :: [String] -> [String]
empacotaDuplicata s
    |null s = []
    |null (tail s) = s
    |otherwise = empacotando (head s) s : empacotaDuplicata (quebraListaE (head s) s)

-- 14. Considerando:
-- Reg = [(15,”Ana”),(22,”Pedro”),(2,”Maria”),(12,”João”),(14,”Pablo”),(23,”Poliana”)]
-- Implemente uma função para ordenar o registro considerando as idades.
type Dados = (Int, String)
reg :: [Dados]
reg = [(15,"Ana"),(22,"Pedro"),(2,"Maria"),(12,"Joao"),(14,"Pablo"),(23,"Poliana")]

ordena :: [Dados]
ordena = ordenaDados reg

ordenaDados :: [Dados] -> [Dados]
ordenaDados [ ] = [ ]
ordenaDados (x : xs) = ordenaDados [y | y <- xs, y <= x]
                    ++ [x] ++
                    ordenaDados [y | y <- xs, y > x]

-- 15. Implemente uma função que recebe duas listas e retorna outra lista com os elementos intercalados.
-- Exemplo:
-- intercala [1,2,3] [4,6] -> [1,4,2,6,3]
-- intercala [] [4,6] -> [4,6]
intercala :: [Int] -> [Int] -> [Int]
intercala (x:xs) (y:ys)
    |null xs && null ys = x : [y]
    |null xs && not (null ys) = x : y : ys
    |null ys && not (null xs) = x : y : xs
    |otherwise = x : y : intercala xs ys

-- 17. Defina, em Haskell, uma função f que, dadas uma lista i de inteiros
-- e uma lista l qualquer, retorne uma nova lista constituı́da pela
-- lista l seguida de seus elementos que têm posição indicada na
-- lista i, conforme o exemplo a seguir:
-- f [2,1,4] [’a’, ’b’, ’c’, ’d’] -> [’a’, ’b’, ’c’, ’d’, ’b’, ’a’, ’d’].
f :: [Int] -> String -> String
f lista str = str ++ [str !! (y - 1) |y <- lista]

-- 18. Defina a função metade :: [a] -> ([a],[a]) que divide uma lista em duas metades.
-- Exemplo:
-- metade [1,2,3,4,5,6] -> ([1,2,3],[4,5,6])
-- metade [1,2,3,4,5] -> ([1,2],[3,4,5])
type DuplaListas = ([Int],[Int])

quebraLista :: Int -> [Int] -> DuplaListas
quebraLista n l = (take (div n 2) l, drop (div n 2) l)

metade :: [Int] -> DuplaListas
metade lista = quebraLista (length lista) lista

-- 19. Crie uma função que adiciona um elemento no final da lista dada como parâmetro.
-- Exemplo:
-- add_fim [1, 2, 3] 10 -> [1, 2, 3, 10]
addFim :: [Int] -> Int -> [Int]
addFim l n = l ++ [n]

-- 20. Considere que o preço de uma passagem de avião em um trecho pode variar dependendo da idade do passageiro. 
-- Pessoas com 60 anos ou mais pagam apenas 60% do preço total. Crianças até 10 anos pagam 50% e 
-- bebês (abaixo de 2 anos) pagam apenas 10%.
-- Elabore uma função que tenha como entrada uma lista de tuplas composta pelo valor da passagem e a idade do passageiro,
-- respectivamente, e produza o valor total a ser pago.
type Passageiro = (Double, Int)

preco :: Passageiro -> Double
preco (p, _) = p

idade :: Passageiro -> Int
idade (_, i) = i

desconto :: Int -> Double
desconto i
    |i <= 2 = 0.9
    |i <= 10 = 0.5
    |i >= 60 = 0.4
    |otherwise = 0.0

valores :: [Passageiro] -> [Double]
valores lista = [p - (p * desconto i) |v <- lista, p <- [preco v], i <- [idade v]]

-- 21. O índice de massa corporal (IMC) é uma medida simples para classificar o peso de adultos. O IMC de um indivíduo é calculado
-- como o valor do peso (em quilogramas) a dividir pelo quadrado da altura (em metros): IMC = 𝑝𝑒𝑠𝑜/𝑎𝑙𝑡𝑢𝑟𝑎².
-- Escreva uma definição da função classifica :: [(Int, Float, Float)] -> [(Int, String)]
-- que determina a classificação acima para uma lista composta por um ID, o peso em quilogramas e a altura em metros e produz
-- como saída a uma lista contendo o ID e sua respectiva classificação.
type Informacoes = (Int, Double, Double)

pegaId :: Informacoes -> Int
pegaId (i,_,_) = i

pesoKG :: Informacoes -> Double
pesoKG (_,p,_) = p

altura :: Informacoes -> Double
altura (_,_,h) = h

retorno :: Double -> String
retorno imc
    |imc < 18.5 = "Abaixo Peso"
    |imc < 25 = "Peso Normal"
    |imc < 30 = "Excesso de Peso"
    |otherwise = "Obesidade"

classifica :: [Informacoes] -> [(Int, String)]
classifica lista = [ (i, retorno (p / (h * h)) ) |l <- lista, i <- [pegaId l], p <- [pesoKG l], h <- [altura l]]

-- 26. Defina uma função forte ∶∶ String → Bool
-- para verificar se uma palavra-passe dada numa cadeia de
-- caracteres é forte segundo os seguintes critérios: deve ter 8
-- caracteres ou mais e pelo menos uma letra maiúscula, uma letra
-- minúscula e um algarismo. Sugestão: use a função or ∶∶ [Bool] → Bool e listas em
-- compreensão.
verificaMaiusculo :: String -> Int
verificaMaiusculo str = length[x | x <- str, x >= 'A' && x <= 'Z']

verificaMinusculo :: String -> Int
verificaMinusculo str = length[x | x <- str, x >= 'a' && x <= 'z']

verificaAlgarismo :: String -> Int
verificaAlgarismo str = length[x | x <- str, x >= '0' && x <= '9']

forte :: String -> Bool
forte [] = False
forte str
    |length str < 8 = False
    |verificaMaiusculo str > 0 && verificaMinusculo str > 0 && verificaAlgarismo str > 0 = True
    |otherwise = False