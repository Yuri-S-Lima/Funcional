type Hora = (Int, Int, Int)

verifica :: Hora -> Bool
verifica (h, m, s)
    |h < 0 || h > 23 = False
    |m < 0 || m > 59 = False
    |s < 0 || s > 59 = False
    |otherwise = True

segundos :: Hora -> Int
segundos (h, m, s)
    |not (verifica (h, m, s)) = 0
    |otherwise = (h * (60 * 60)) + (m * 60) + s

-- aux :: Int -> Int -> Int -> Int -> Hora
converte :: Int -> Hora
converte s
    |s <= 0 = (0,0,0)
    |otherwise = ((div s 3600), (div (s - 3600) (60)), (mod (s - 3600) 60))

-- aux t h m ss 
--     |t == 0 = (h, m, ss)
--     |otherwise = aux 0 (div t 3600) (div (t - 3600) 60) (mod (t - 3600) 60)

-- -- 1h - 3600s
-- -- xh - 4500s