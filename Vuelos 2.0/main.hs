import System.IO  
import Data.List.Split
import Data.List
import Data.Graph
import Data.Array
import Conversiones
import GHC.Float

{- referencia http://www.glc.us.es/~jalonso/vestigium/el-tipo-abstracto-de-datos-de-los-grafos-en-haskell/
 creo un tipo Grafo que es un Array 
-}
type Grafo v p = Array v [(v,p)] 

--función que parsea 
creaGrafo :: (Ix v, Num p) => Bool -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo d cs vs =
    accumArray 
     (\xs x -> xs++[x]) [] cs 
     ((if d then []
       else [(x2,(x1,p))|(x1,x2,p) <- vs, x1 /= x2]) ++
      [(x1,(x2,p)) | (x1,x2,p) <- vs])

--nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
--nodos g = indices g

--peso :: (Ix v,Num p) => v -> v -> (Grafo v p) -> p
--peso x y g = head [c | (a,c) <- g!x , a == y]

--aristasND :: (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
--aristasND g = 
--    [(v1,v2,w) | v1 <- nodos g , (v2,w) <- g!v1 , v1 < v2]

{-Referencia http://stackoverflow.com/questions/11168238/haskell-generating-all-paths-between-nodes
devuelve una lista de listas de tuplas que representan todos los caminos posibles entre x y 
utiiza funciones de la libreria Graph propia de Haskell 
connect recibe  un num de Origen, un num de Deestino y un grafo-}
connect x y g = helper x y g [x]
  where
    helper a b g visited
        | a == b    = [[]]
        | otherwise = [(a,c,d):path | (c,d) <- g!a, c `notElem` visited, path <- helper c b g (c:visited)]

-- como los datos que se parsearán sólo contine distanicas y precios, sen necesitará calcular el tiempo por medio de un calculo
-- vellocidadVuelo contiene la velocidad promedio a la que viajan los aviones (Km/h)
-- para que pueda realizarse bien la conversion (velocidad=distancia/tiempo  => tiempo = distancia/velocidad) se lo transforma en Float
velocidadVuelo = fromIntegral 420 :: Float

main= do  
        handle <- openFile "DatasetSimple.txt" ReadMode --lee el dataset
        contents <- hGetContents handle -- devuelve un dato manejable 
        let lineas = splitOneOf ";\n" contents -- separa el contendido cada vez que encuentra un : o un salto de linea
        let gT = tuplasDistanciasG lineas --generacion de tuplas de distandias considerando sólo la distancia del dataset
        let gC = tuplasCostoG lineas --generacion de tuplas de distandias considerando sólo el costo del dataset
        -- interacción con el usuario 
        putStr "Ingrese Ciudad Origen: "
        cOrigS <- getLine
        let cOrig = ciudadToNum cOrigS
        putStr "Ingrese Ciudad Destino: "
        cDestS <- getLine
        let cDest = ciudadToNum cDestS
        putStrLn "FILTRO \n\t 1.  Tiempo\n\t 2.  Costo\n"
        putStr "Ingrese el numero de la opcion: "
        f<-getLine
        if f=="1"
            then do 
                --putStrLn"Espere..."
                let g = creaGrafo True (1,70) gT
                let c = connect cOrig cDest g
                if null c 
                    then do
                        let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                        print msj
                    else do
                        let tp = distantcias c
                        let d = minimum tp
                        printPantalla d cOrig cDest
                        let tVuelo = tiempoDeVuelo d
                        putStr "En (aprox horas): "
                        print (tVuelo)

            else do 
                --putStrLn"Espere..."
                let g = creaGrafo True (1,70) gC
                let c = connect cOrig cDest g
                if null c 
                    then do
                        let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                        print msj
                    else do
                        let tp = distantcias c
                        let d = minimum tp
                        printPantalla d cOrig cDest
                        let costo= costoDeVuelo d 
                        putStr "Costo de : $"
                        print costo
       
        --let listaTuplas=armarListCom lineas
        --let coincidencas1=buscarOrg cOrig listaTuplas
        --let coincidencas2=buscarDest cDest coincidencas1
        --let impriVal = "Distancia: " ++ coincidencas2 !!0 !!2 ++ "Km  Costo: " ++ coincidencas2 !!0 !!3
        --let distancia = coincidencas2 !!0 !!2
        --let distNum = read distancia :: Float
        --let tiempoVuelo =  velocidadVuelo/distNum
        --print impriVal
        --print "Tiempo aprox (horas): " 
        --print tiempoVuelo 
        
        hClose handle


sumaDistantcias :: [(Int,Int,Int)] -> (Int,Int,Int)
sumaDistantcias [] = (0,0,0)
sumaDistantcias (x:xs) = (0,0, obtenerDistncia x + obtenerDistncia (sumaDistantcias xs)) 

obtenerDistncia:: (Int,Int,Int) -> Int
obtenerDistncia (x,y,z)= z

obtenerCiudad:: (Int,Int,Int) -> Int
obtenerCiudad (x,y,z)= y 

addTupla:: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
addTupla x y = head x : y 

distantcias:: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
distantcias [] = []
distantcias (x:xs)= addTupla [sumaDistantcias x] x : distantcias xs

obtenerDistanciasTuplas :: [[(Int,Int,Int)]] -> [Int]
obtenerDistanciasTuplas []=[]
obtenerDistanciasTuplas (x:xs) = [obtenerDistncia (head x)] ++ obtenerDistanciasTuplas xs

obtenerCiudadTuplas :: [(Int,Int,Int)] -> [Int]
obtenerCiudadTuplas []=[]
obtenerCiudadTuplas (x:xs) = [obtenerCiudad x] ++ obtenerCiudadTuplas xs
                            
obtenerCiudadesIntermedias :: [(Int,Int,Int)] -> [(Int,Int,Int)]
obtenerCiudadesIntermedias l = do 
                                let t = drop 1 l
                                let f = init t
                                f

imprimirTrasbordos :: [Int] -> String
imprimirTrasbordos []=""
imprimirTrasbordos (x:xs) = do 
                                numToCiudad x ++ " -> " ++ imprimirTrasbordos xs

                       
--ciudadToNum :: String -> Int
--ciudadToNum "Quito"     = 1
--ciudadToNum "Guayaquil" = 2
--ciudadToNum "Cuenca"    = 3
--ciudadToNum "Machala"   = 4
--ciudadToNum "Loja"      = 5
--ciudadToNum "Puyo"      = 6
--ciudadToNum x= read x :: Int

sacarDolar :: String -> String
sacarDolar s = drop 2 s

stringToFloat :: String -> Float
stringToFloat n = read (sacarDolar n) :: Float

tuplasDistanciasG:: [String] -> [(Int,Int,Int)]
tuplasDistanciasG [] =[] 
tuplasDistanciasG l = [(ciudadToNum (l!!0), ciudadToNum (l!!1),ciudadToNum (l!!2))] ++tuplasDistanciasG(drop 4 l)

tuplasCostoG :: [String] -> [(Int,Int,Int)]
tuplasCostoG [] = []
tuplasCostoG l = [(ciudadToNum (l!!0), ciudadToNum (l!!1),ciudadToNum (drop 2 (l!!3)))] ++tuplasCostoG(drop 4 l)

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


armarRuta :: String -> [[String]] -> [[String]]
armarRuta o [[]] = [[]]
armarRuta  o itinierarios =  
    if o `elem` itinierarios!!0 
        then [itinierarios!!0] ++ buscarDest o (drop 2 itinierarios)
        else buscarDest o (drop 2 itinierarios)

rutaImprimir :: [(Int,Int,Int)] -> String
rutaImprimir l = do 
                    let cidInt = obtenerCiudadesIntermedias l
                    let oct = obtenerCiudadTuplas cidInt
                    let transbordos = imprimirTrasbordos oct
                    let transbordosFix = take ((length transbordos) - 4) transbordos
                    transbordosFix

printPantalla :: [(Int,Int,Int)] -> Int -> Int -> IO ()
printPantalla l orig dest = do  
                            let transbordos = rutaImprimir l
                            --let tVuelo = tiempoDeVuelo l
                            let text = "Ruta Optima: desde " ++ numToCiudad orig ++ " a " ++ numToCiudad dest
                            print (text)   
                            putStr "Pasando por: " 
                            print (transbordos)  


tiempoDeVuelo :: [(Int,Int,Int)] -> Float
tiempoDeVuelo l = do  
                    let distNum = obtenerDistncia (head l)
                    let tempD = fromIntegral distNum :: Float
                    let impr = tempD/velocidadVuelo
                    impr

costoDeVuelo :: [(Int,Int,Int)] -> Int
costoDeVuelo l = obtenerDistncia (head l)