-- file: ch07/basicio.hs
main = do
       putStrLn "Greetings! What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcolme to Haskell, " ++ inpStr ++ "!"