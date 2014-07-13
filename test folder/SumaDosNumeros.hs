import System.IO

main :: IO ()
main = do 
	a <- get
	b <- getLine
	mapM_ putStrLn (lines sumaDosNumeros a b)

sumaDosNumeros :: Integer -> Integer -> Integer
sumaDosNumeros x y = x+y