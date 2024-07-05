-- exemplo1.hs
main :: IO ()
main = do
    let x = 10
    let y = 20.5
    let z = "Hello, Haskell!"
    print (x + y)
    print z
    if x > 5 then
        print "x é maior que 5"
    else
        print "x não é maior que 5"
