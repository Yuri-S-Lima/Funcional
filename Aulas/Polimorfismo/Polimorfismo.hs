concat2 :: [[t]] -> [t]
concat2 [] = []
concat2 (a:x) = a ++ (concat2 x)

unZip :: [(t, u)] -> ([t],[u])
unZip lista = ([fst l | l <- lista], [snd l| l <- lista])

init2 :: [t] -> [t]
init2 [] = []
init2 (_:[]) = []
init2 (a:x) = a : init2 x

tome :: Int -> [t] -> [t]
tome n lista = take n lista

tire :: Int -> [t] -> [t]
tire n lista = drop n lista