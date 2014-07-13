import System.IO  
import Control.Monad

main = do  
        let list = []
        handle <- openFile "DatasetM.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = singlewords
        print list
        hClose handle   

f :: [String] -> [String]
f = map read