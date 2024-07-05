-- exemplo2.hs
main :: IO ()
main = do
    let a = 42
    let b = 3.14
    let c = "Haskell é divertido!"
    print (a * 2)
    print (b / 2)
    print (c ++ " Você não acha?")
    if a == 42 then
        if b > 3 then
            print "Tudo está certo!"
        else
            print "Algo está errado com b"
    else
        print "Algo está errado com a"
