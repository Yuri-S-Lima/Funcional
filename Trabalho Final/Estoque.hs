{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char (toUpper)


data Arvore a = Nulo | No (Arvore a) a (Arvore a)
    deriving (Eq, Ord, Show)

type Produtos = (String, Int, Double)

retonaNome :: Produtos -> String
retonaNome (s, i, d) = s

vende :: String -> Int -> [Produtos] -> [Produtos]
vende s q ((n, t, p):xs)
    |null xs && n == s && t - q < 1 = xs
    |null xs = (n, t - q, p) : xs
    |n == s && t - q < 1 = xs
    |n == s = (n, t - q, p) : xs
    |otherwise = (n, t, p) : vende s q xs

soma :: Arvore Produtos -> Double
soma Nulo =  0
soma (No arv1 (a, b, c) arv2) = fromIntegral b * c + soma arv1 + soma arv2

criarArvore :: (Ord a) => [a] -> Arvore a
criarArvore [] = Nulo
criarArvore (x:xs) = criarArvoreAux (No Nulo x Nulo) xs
    where
        criarArvoreAux  arvore [] = arvore
        criarArvoreAux  arvore (x:xs) = criarArvoreAux (inserir arvore x) xs

inserir :: (Ord a) => Arvore a -> a -> Arvore a
inserir Nulo x = No Nulo x Nulo
inserir (No arv1 v arv2) x
    |v == x = No arv1 v arv2
    |v < x = No arv1 v (inserir arv2 x)
    |otherwise = No (inserir arv1 x) v arv2

arvore :: [Produtos] -> Arvore Produtos
arvore = criarArvore

atualiza :: Produtos -> [Produtos] -> [Produtos]
atualiza l [] = [l]
atualiza p (x:xs)
    |null xs && retonaNome p == retonaNome x = p : xs
    |null xs = x : [p]
    |retonaNome p == retonaNome x = p : xs
    |otherwise = x : atualiza p xs

imprimirProdutos :: Arvore Produtos -> IO ()
imprimirProdutos Nulo = return()
imprimirProdutos (No arv1 v arv2) = do
   imprimirProdutos arv1
   print v
   imprimirProdutos arv2

pesquisa :: Arvore Produtos -> String -> IO ()
pesquisa Nulo _ = return ()
pesquisa (No arv1 v arv2) p
    |retonaNome v == p = print v
    |retonaNome v > p = pesquisa arv1 p
    |otherwise = pesquisa arv2 p

verificaNulo :: String -> Arvore Produtos -> IO()
verificaNulo _ Nulo = putStrLn "\nImpossivel Buscar, nao existem produtos cadastrados!"
verificaNulo [] _ = putStrLn "\nImpossivel Buscar, campo nome vazio!"
verificaNulo nome arv = pesquisa arv nome 

verificaDados :: [Produtos] -> IO()
verificaDados [] = putStrLn "\nNao existem produtos cadastrados!"
verificaDados dados = return ()

verificaVenda ::  String -> Int -> [Produtos] -> [Produtos]
verificaVenda _ _ [] = []
verificaVenda nome qtd produtos = vende nome qtd produtos

retornoSoma :: Arvore Produtos -> IO()
retornoSoma Nulo = putStrLn "\nNao existem produtos cadastrados, impossivel somar!"
retornoSoma arv = putStrLn ("\nTOTAL: R$ " ++ show (soma arv))  

menu :: [Produtos] -> Arvore Produtos ->  IO ()
menu dados arv = do

    putStrLn "\n------------------MENU-------------------"
    putStrLn "Digite 1 para buscar produto"
    putStrLn "Digite 2 para inserir / atualizar produto"
    putStrLn "Digite 3 somar total dos produtos"
    putStrLn "Digite 4 para vender produto"
    putStrLn "Digite 5 imprimir produtos"
    putStrLn "Digite 0 para sair"
    putStr "Opção: "

    opt <- getChar
    getChar

    case opt of
        '1' -> do
            putStrLn "\n BUSCAR PRODUTO"
            putStrLn "\nDigite um nome para busca"
            nome <- getLine
            verificaNulo (map toUpper nome) arv

            menu dados arv

        '2' -> do           
            putStrLn "\n INSERIR / ATUALIZAR PRODUTO"
            putStrLn "Digite o nome"
            nome <- getLine
            putStrLn "Digite a quantidade"
            qtd <- getLine
            putStrLn "Digite o preco"
            preco <- getLine

            let atualizado = atualiza (map toUpper nome, read qtd :: Int, read preco :: Double) dados

            menu atualizado (arvore atualizado)

        '3' -> do
            retornoSoma arv

            menu dados arv

        '4' -> do
            putStrLn "\n VENDER PRODUTO"
            putStrLn "Digite o nome do produto vendido"
            nome <- getLine
            putStrLn "Digite a quantidade"
            qtd <- getLine
            verificaDados dados
            let lis = verificaVenda (map toUpper nome) (read qtd :: Int) dados
            
            menu lis (arvore lis)

        '5' -> do
            putStrLn "\n LISTA DE PRODUTOS"
            verificaDados dados
            imprimirProdutos arv
            
            menu dados arv

        '0' -> do
            putStrLn "\n--------FIM--------"

        _ -> do
        putStrLn "\nOpção inválida!"
        menu dados arv

main :: IO ()
main = do
    let inicial = arvore[]
    menu [] inicial
    return()