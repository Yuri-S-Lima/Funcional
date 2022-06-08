divisores :: Int -> [Int]
divisores n = [x | x <- [1..n - 1], mod n x == 0]

perfeitos :: Int -> [Int]
perfeitos p = [x | x <- [1..p], sum(divisores x) == x]

concatena :: [[Int]] -> [Int]
concatena l = [c | x <- l, c <- x]