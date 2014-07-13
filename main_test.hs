import System.IO
import System.Exit
import Data.List
import Data.Char
import Data.List.Split

--Aqui se definen los tipos (Si los usamos)
data Vuelo = 	EOF |
				COrigen String |
				CDestino String |
				Distancia Int |
				Costo Float |
				Error String
				deriving (Show, Eq)

--Aqui se definen las funciones que hacemos
leerCSV :: FilePath -> IO [String]
leerCSV file = do
					csv <- readFile file 
					putStrLn "Archivo leido"
					--let listaSplitArchivo = split (oneOf ";") 
					writeFile "output.txt" (csv) -- con esto verifico que si lo lee
					return (lines csv)



--Aqui esta el main
main = do
  archivo <- leerCSV "DatasetM.csv"
  putStrLn "Principal"
  --show archivo
  --lineas : splitOn ";" file
  --mapM_ putStrLn (lines archivo)