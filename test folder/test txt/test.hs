import System.IO  
import Control.Monad
import Data.List.Split

main = do  
        let list = []
        handle <- openFile "DatasetM.txt" ReadMode
        contents <- hGetContents handle
        let lineas = splitOneOf ";\n" contents
        let singlewords = words contents
            --list = singlewords
        --print list
        print lineas
        hClose handle   
