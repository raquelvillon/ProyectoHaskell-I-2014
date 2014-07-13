import System.IO
import Data.List.Split.splitOn

main :: IO ()
main = do 
       inh <- openFile "dataset.csv" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh


mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map inpStr)
                   mainloop inh outh

