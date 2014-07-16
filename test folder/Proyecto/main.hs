import System.IO  
--import Control.Monad
import Data.List.Split
--data BookInfo = Book Int String [String]
--                deriving (Show)
--data MagazineInfo = Magazine Int String [String]
--                    deriving (Show)
--myInfo = Book 9780135072455 "Algebra of Programming"
--         ["Richard Bird", "Oege de Moor"]

velocidadVuelo = 420

main= do  
        handle <- openFile "DatasetSimple.txt" ReadMode
        contents <- hGetContents handle
        let lineas = splitOneOf ";\n" contents
        --let listaTuplas=armarListCom lineas
        --putStrLn "Ingrese Ciudad Origen: "
        --cOrig <- getLine
        --putStrLn "Ingrese Ciudad Destino: "
        --cDest <- getLine
        --let coincidencas1=buscarOrg cOrig listaTuplas
        --let coincidencas2=buscarDest cDest coincidencas1
        --let impriVal = "Distancia: " ++ coincidencas2 !!0 !!2 ++ "Km  Costo: " ++ coincidencas2 !!0 !!3
        --let distancia = coincidencas2 !!0 !!2
        --let distNum = read distancia :: Float
        --let tiempoVuelo =  velocidadVuelo/distNum
        --print impriVal
        --print "Tiempo aprox (horas): " 
        --print tiempoVuelo 
        let numerosTuplas = pasarANumeros lineas
        print numerosTuplas
        hClose handle


ciudadToNum :: String -> Int
ciudadToNum "Albany, NY"                    = 1
ciudadToNum "Albuquerque, NM"               = 2
ciudadToNum "Atlanta, GA (Metro Area)"      = 3
ciudadToNum "Atlantic City, NJ"             = 4
ciudadToNum "Austin, TX"                    = 5
ciudadToNum "Bellingham, WA"                = 6
ciudadToNum "Birmingham, AL"                = 7
ciudadToNum "Boise, ID"                     = 8
ciudadToNum "Boston, MA (Metro Area)"       = 9
ciudadToNum "Buffalo, NY"                   = 10
ciudadToNum "Burlington, VT"                = 11
ciudadToNum "Charleston, SC"                = 12
ciudadToNum "Charlotte, NC"                 = 13
ciudadToNum "Chicago, IL"                   = 14
ciudadToNum "Cincinnati, KY"                = 15
ciudadToNum "Cleveland, OH (Metro Area)"    = 16
ciudadToNum "Colorado Springs, CO"          = 17
ciudadToNum "Columbia, SC"                  = 18
ciudadToNum "Columbus, OH"                  = 19
ciudadToNum "Corpus Christi, TX"            = 20
ciudadToNum "Dallas, TX"                    = 21
ciudadToNum "Dayton, OH"                    = 22
ciudadToNum "Denver, CO"                    = 23
ciudadToNum "Des Moines, IA"                = 24
ciudadToNum "Detroit, MI"                   = 25
ciudadToNum "El Paso, TX"                   = 26
ciudadToNum "Eugene, OR"                    = 27
ciudadToNum "Flint, MI"                     = 28
ciudadToNum "Fort Myers, FL"                = 29
ciudadToNum "Fresno, CA"                    = 30
ciudadToNum "Grand Rapids, MI"              = 31
ciudadToNum "Greensboro/High Point, NC"     = 32
ciudadToNum "Hartford, CT"                  = 33
ciudadToNum "Houston, TX"                   = 34
ciudadToNum "Huntsville, AL"                = 35
ciudadToNum "Indianapolis, IN"              = 36
ciudadToNum "Jacksonville, FL"              = 37
ciudadToNum "Kansas City, MO"               = 38
ciudadToNum "Key West, FL"                  = 39
ciudadToNum "Las Vegas, NV"                 = 40
ciudadToNum "Los Angeles, CA (Metro Area)"  = 41
ciudadToNum "Louisville, KY"                = 42
ciudadToNum "Medford, OR"                   = 43
ciudadToNum "Memphis, TN"                   = 44
ciudadToNum "Miami, FL (Metro Area)"        = 45
ciudadToNum "Milwaukee, WI"                 = 46
ciudadToNum "Minneapolis, MN"               = 47
ciudadToNum "Myrtle Beach, SC"              = 48
ciudadToNum "Nashville, TN"                 = 49
ciudadToNum "New Orleans, LA"               = 50
ciudadToNum "New York City, NY (Metro Area)"= 51
ciudadToNum "Oklahoma City, OK"             = 52
ciudadToNum "Omaha, NE"                     = 53
ciudadToNum "Orlando, FL"                   = 54
ciudadToNum "Philadelphia, PA"              = 55
ciudadToNum "Phoenix, AZ"                   = 56
ciudadToNum "Pittsburgh, PA"                = 57
ciudadToNum "Portland, OR"                  = 58
ciudadToNum "Sacramento, CA"                = 59
ciudadToNum "San Antonio, TX"               = 60
ciudadToNum "San Diego, CA"                 = 61
ciudadToNum "San Francisco, CA (Metro Area)"= 62
ciudadToNum "Seattle, WA"                   = 63
ciudadToNum "Sioux Falls, SD"               = 64
ciudadToNum "Spokane, WA"                   = 65
ciudadToNum "St. Louis, MO"                 = 66
ciudadToNum "Tampa, FL (Metro Area)"        = 67
ciudadToNum "Tucson, AZ"                    = 68
ciudadToNum "Washington, DC (Metro Area)"   = 69
ciudadToNum "West Palm Beach/Palm Beach, FL"= 70
ciudadToNum x= read x :: Int

ciudadToNum2 :: String -> Int
ciudadToNum2 "Quito"     = 1
ciudadToNum2 "Guayaquil" = 2
ciudadToNum2 "Cuenca"    = 3
ciudadToNum2 "Machala"   = 4
ciudadToNum2 "Loja"      = 5
ciudadToNum2 "Puyo"      = 6
ciudadToNum2 x= read x :: Int

sacarDolar :: String -> String
sacarDolar s = drop 2 s

stringToFloat :: String -> Float
stringToFloat n = read (sacarDolar n) :: Float

pasarANumeros:: [String] -> [(Int,Int,Int,Float)]
pasarANumeros [] =[(1,1,0,0)] 
pasarANumeros l = [(ciudadToNum (l!!0), ciudadToNum (l!!1),ciudadToNum (l!!2), stringToFloat(l!!3))] ++pasarANumeros(drop 4 l)

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