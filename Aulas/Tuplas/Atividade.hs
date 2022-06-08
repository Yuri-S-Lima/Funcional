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

converte :: Int -> Hora
converte s
    |s <= 0 = (0,0,0)
    |otherwise = (hora, min, seg)
    where
        hora = div s 3600; min = (div (s - (3600 * hora)) (60)); seg = (mod (s - 3600) 60);