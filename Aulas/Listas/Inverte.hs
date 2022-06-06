inverteLista :: [Int] -> [Int]
inverteLista[] = []
inverteLista(a:x) = inverteLista x ++ [a]

-- [4,3,2,1] -> 321 ++ [4]
-- 321 -> 21 ++ [3] ++ [4]
-- 21 -> 1 ++ [2] ++ [3] ++ [4]
-- 1 -> [] ++ [1] ++ [2] ++ [3] ++ [4]
-- [] -> [1,2,3,4]