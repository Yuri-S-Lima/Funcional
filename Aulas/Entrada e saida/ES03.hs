-- Exemplo 03 - Ler e imprimir caractere
main :: IO()
main = do
    caractere <- getChar
    putChar caractere