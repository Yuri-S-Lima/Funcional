somaLista :: [Int] -> Int
somaLista[] = 0
somaLista(a:x) = a + somaLista x

-- somaLista :: [Int] -> Int
-- somaLista = sum