import System.IO  
import Control.Monad
import Data.List.Split
data BookInfo = Book Int String [String]
                deriving (Show)
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

--lineas = []
main= do  
       	--
        handle <- openFile "DatasetM.txt" ReadMode
        contents <- hGetContents handle
        let lineas = splitOneOf ";\n" contents
        --let singlewords = words contents
        --let res= busSec lineas "503"
        let lista=tomarDos ["bs","sbs","dvsb"]
        print lista
        
        hClose handle


tomarDos :: [String] -> [String]
tomarDos l = take 2 l

busSec::Ord a=>[a]->a->Bool
busSec [] _ = False
busSec (x:xs) ele
                    | x==ele = True
                    | True = busSec xs ele