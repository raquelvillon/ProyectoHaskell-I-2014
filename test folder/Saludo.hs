main = do
       putStrLn "Hola, cómo te llamas?"
       inpStr <- getLine
       putStrLn $ "Bienvenido a Haskell, " ++ inpStr ++ "!"