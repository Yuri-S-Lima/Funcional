-- árvore binária de busca a partir de uma lista

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- estrutura
data Arvore a = Nulo | No (Arvore a) a (Arvore a)
    deriving (Show)


-- cria uma árvore binária de busca
-- recebe uma lista e retorna uma árvore
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