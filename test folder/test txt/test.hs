import System.IO  
--import Control.Monad
import Data.List.Split
--data BookInfo = Book Int String [String]
--                deriving (Show)
--data MagazineInfo = Magazine Int String [String]
--                    deriving (Show)
--myInfo = Book 9780135072455 "Algebra of Programming"
--         ["Richard Bird", "Oege de Moor"]

main= do  
        handle <- openFile "DatasetM.txt" ReadMode
        contents <- hGetContents handle
        let lineas = splitOneOf ";\n" contents
        --let singlewords = words contents
        --let res= busSec lineas "503"
        --let lista = ["bs","sbs","dvsb","kbj"]
        let listaTuplas=armarListCom lineas
        putStrLn "Ingrese Ciudad Origen: "
        cOrig <- getLine
        putStrLn "Ingrese Ciudad Destino: "
        cDest <- getLine
        let coincidencas1=buscarOrg cOrig listaTuplas
        let coincidencas2=buscarDest cDest coincidencas1
        
        --let ruta0=armarRuta cOrig listaTuplas

        --let coincidencas1=buscarOrg "Dallas, TX" listaTuplas
        --let coincidencas2=buscarDest "San Antonio, TX" coincidencas1
        --print (length listaTuplas)
        let impriVal = "Distancia: " ++ coincidencas2 !!0 !!2 ++ "Km  Costo: " ++ coincidencas2 !!0 !!3
        print  impriVal
        hClose handle


tomarTupla :: [String] -> [String]
tomarTupla l = take 4 l

armarListCom :: [String]->[[String]]
armarListCom []=[[]]
armarListCom l =  [tomarTupla l] ++ armarListCom (drop 4 l) 

buscarOrg :: String -> [[String]] -> [[String]]
buscarOrg o [[]] = [[]]
buscarOrg o itinierarios =  
    if o == itinierarios !!0 !!0
        then [itinierarios!!0] ++ buscarOrg o (drop 1 itinierarios)
        else buscarOrg o (drop 1 itinierarios)

buscarDest :: String -> [[String]] -> [[String]]
buscarDest d [[]] = [[]]
buscarDest d itinierarios =  
    if d == itinierarios !!0 !!1
        then [itinierarios!!0] ++ buscarDest d (drop 1 itinierarios)
        else buscarDest d (drop 1 itinierarios)

--mostrarValores :: [[String]] -> return()
--mostrarValores l = do
--                    show l !!0 !!0
--                    show l !!0 !!1


armarRuta :: String -> [[String]] -> [[String]]
armarRuta o [[]] = [[]]
armarRuta  o itinierarios =  
    if o `elem` itinierarios!!0 
        then [itinierarios!!0] ++ buscarDest o (drop 2 itinierarios)
        else buscarDest o (drop 2 itinierarios)


--busSec::Ord a=>[a]->a->Bool
--busSec [] _ = False
--busSec (x:xs) ele
--                    | x==ele = True
--                    | True = busSec xs ele