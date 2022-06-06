-- [3,1,4]
ordena :: [Int] -> [Int]
ordena[] = []
ordena(a:x) = insere a (ordena x)

insere :: Int -> [Int] -> [Int]
insere b [] = b : []
insere b(z:zs)
    |b <= z = b:z:zs
    |otherwise = z:(insere b zs)

    