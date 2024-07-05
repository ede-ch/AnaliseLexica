-- Exemplo em Haskell com declarações e comandos básicos

-- Declaração de variáveis
-- x é uma variável inteira (Int)
x :: Int
x = 10

-- y é uma variável real (Double)
y :: Double
y = 3.14

-- z é uma variável lógica (Bool)
z :: Bool
z = True

-- Comando de leitura (getLine) e escrita (print)
main :: IO ()
main = do
    putStrLn "Digite um número:"
    input <- getLine
    let numero = read input :: Int  -- Converte a entrada para um número inteiro

    -- Comando de atribuição
    let soma = x + numero

    -- Comando condicional (if-else)
    if soma > 15 then
        putStrLn "A soma é maior que 15."
    else
        putStrLn "A soma é 15 ou menor."

    -- Expressões aritméticas e operadores
    let divisao = y / fromIntegral x  -- Realiza a divisão de y por x

