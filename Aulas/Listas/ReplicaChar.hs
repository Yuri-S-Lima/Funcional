replicaChar :: Char -> Int -> [Char]
replicaChar c n
    |n == 0 = []
    |otherwise =  c : replicaChar c (n - 1)