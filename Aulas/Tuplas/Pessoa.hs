type Pessoa = (String, String, Int)
nome :: Pessoa -> String
fone :: Pessoa -> String
anoNasc :: Pessoa -> Int

nome(n, p, a) = n
fone(n, p, a) = p
anoNasc(n, p, a) = 2022 - a
