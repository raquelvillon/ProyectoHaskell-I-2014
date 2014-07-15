import System.IO  
import Control.Monad
import Data.List.Split
data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]


main= do  
       	--
        handle <- openFile "DatasetM.txt" ReadMode
        contents <- hGetContents handle
        let lineas = splitOneOf ";\n" contents
        --let singlewords = words contents
        --let res= busSec lineas "503"
        --let lista = ["bs","sbs","dvsb","kbj"]
        let listaTuplas=armarListCom lineas
        let coincidencas1=buscarOrg "Dallas, TX" listaTuplas
        let coincidencas2=buscarDest "San Antonio, TX" coincidencas1
        --print (length listaTuplas)
        print  coincidencas2
        hClose handle


tomarDos :: [String] -> [String]
tomarDos l = take 4 l


armarListCom :: [String]->[[String]]
armarListCom []=[[]]
armarListCom l =  [tomarDos l] ++ armarListCom (drop 4 l) 

buscarOrg :: String -> [[String]] -> [[String]]
buscarOrg o [[]] = [[]]
buscarOrg o itinierarios =  
    if o `elem` itinierarios!!0
        then [itinierarios!!0] ++ buscarOrg o (drop 1 itinierarios)
        else buscarOrg o (drop 1 itinierarios)

buscarDest :: String -> [[String]] -> [[String]]
buscarDest o [[]] = [[]]
buscarDest o itinierarios =  
    if o `elem` itinierarios!!0
        then [itinierarios!!0] ++ buscarDest o (drop 1 itinierarios)
        else buscarDest o (drop 1 itinierarios)




busSec::Ord a=>[a]->a->Bool
busSec [] _ = False
busSec (x:xs) ele
                    | x==ele = True
                    | True = busSec xs ele