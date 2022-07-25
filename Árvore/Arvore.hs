data ArvBin = Nulo | No Int ArvBin ArvBin

arv :: ArvBin
arv = No 7 (No 4 (No 1 Nulo Nulo) (No 5 Nulo Nulo)) 
           (No 18 (No 13 Nulo Nulo) Nulo)

em_ordem :: ArvBin -> [Int]
em_ordem Nulo = []
em_ordem (No num esq dir) = (em_ordem esq) ++ [num] ++ (em_ordem dir)