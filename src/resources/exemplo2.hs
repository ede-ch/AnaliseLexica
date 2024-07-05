-- exemplo2.hs

-- Declaração das funções principais
main :: IO ()
main = do
    -- Declaração das variáveis 'a', 'b', e 'c' com valores iniciais
    let a = 42                      -- Inteiro (Int)
    let b = 3.14                    -- Real (Double em Haskell)
    let c = "Haskell é divertido!" -- String (String em Haskell)

    -- Imprime o resultado da expressão aritmética: 'a * 2'
    print (a * 2)

    -- Imprime o resultado da expressão aritmética: 'b / 2'
    print (b / 2)

    -- Concatena a string 'c' com outra frase e imprime o resultado
    print (c ++ " Você não acha?")

    -- Estrutura condicional aninhada
    if a == 42 then                  -- Se 'a' for igual a 42
        if b > 3 then                -- E se 'b' for maior que 3
            print "Tudo está certo!" -- Imprime "Tudo está certo!"
        else                        -- Senão
            print "Algo está errado com b" -- Imprime "Algo está errado com b"
    else                            -- Se 'a' não for igual a 42
        print "Algo está errado com a" -- Imprime "Algo está errado com a"
