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

{-nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
nodos g = indices g

peso :: (Ix v,Num p) => v -> v -> (Grafo v p) -> p
peso x y g = head [c | (a,c) <- g!x , a == y]

aristasND :: (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
aristasND g = 
    [(v1,v2,w) | v1 <- nodos g , (v2,w) <- g!v1 , v1 < v2]-}

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
        let gT = tuplasGrafoDistancias lineas --generacion de tuplas de distandias considerando sólo la distancia del dataset
        let gC = tuplasGrafoCostos lineas --generacion de tuplas de distandias considerando sólo el costo del dataset
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
        --según la opción ingresada, ejecuta el código con grafos diferentes y con resultados diferentes
        if f=="1"
            then do 
                let g = creaGrafo True (1,70) gT
                let c = connect cOrig cDest g
                if null c 
                    then do
                        let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                        print msj
                    else do
                        let todos_caminos = valorTodosCaminos c
                        let camino_mas_corto = minimum todos_caminos --saca el camino mas corto
                        printPantalla camino_mas_corto cOrig cDest
                        let tVuelo = tiempoDeVuelo camino_mas_corto
                        putStr "En (aprox horas): "
                        print (tVuelo)

            else do 
                let g = creaGrafo True (1,70) gC
                let c = connect cOrig cDest g
                if null c 
                    then do
                        let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                        print msj
                    else do
                        let todos_caminos = valorTodosCaminos c
                        let camino_mas_corto = minimum todos_caminos --saca el camino mas corto
                        printPantalla camino_mas_corto cOrig cDest
                        let costo= costoDeVuelo camino_mas_corto 
                        putStr "Costo de : $"
                        print costo
        
        hClose handle

{-suma los terceros elementos de la lista de tuplas.
se obtiene un total de todas las distancias o costos (segun el grafo que se analice) de un camino que une 2 lugares -}
sumaValorZ :: [(Int,Int,Int)] -> (Int,Int,Int)
sumaValorZ [] = (0,0,0)
sumaValorZ (x:xs) = (0,0, obtenerZ x + obtenerZ (sumaValorZ xs)) 

--Sacas el tercer valor de las tupla y lo devuelve
obtenerZ:: (Int,Int,Int) -> Int
obtenerZ (x,y,z)= z

--Saca el segundo valor de una tupla y lo devuelve 
obtenerY:: (Int,Int,Int) -> Int
obtenerY (x,y,z)= y 

--agrega la suma de las distancias/costos (tuplas (0,0,valor)) a un camino determinado
agregarSumaValorZ:: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
agregarSumaValorZ x y = head x : y 

--devuelve un lista de caminos con los totales de distancia/costo en cada uno
valorTodosCaminos:: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
valorTodosCaminos [] = []
valorTodosCaminos (x:xs)= agregarSumaValorZ [sumaValorZ x] x : valorTodosCaminos xs

--devuelve los valores de las ciudades intermedias de un cammino 
obtenerValoresYDeUnCamino :: [(Int,Int,Int)] -> [Int]
obtenerValoresYDeUnCamino []=[]
obtenerValoresYDeUnCamino (x:xs) = [obtenerY x] ++ obtenerValoresYDeUnCamino xs
                            
--Saca la primera y ultima tuppla de cada camino para luego poder sacar los valores intermedios
depurarCaminos :: [(Int,Int,Int)] -> [(Int,Int,Int)]
depurarCaminos l = do 
                    let t = drop 1 l
                    let f = init t
                    f
--Psas de numeros a Strings las ciudades intermedias de un determinado camino
imprimirTrasbordos :: [Int] -> String
imprimirTrasbordos []=""
imprimirTrasbordos (x:xs) = do 
                            numToCiudad x ++ " -> " ++ imprimirTrasbordos xs

--obtiene Las tuplas que describen los valores de Distancias entre ciudades para crear el grafo
tuplasGrafoDistancias:: [String] -> [(Int,Int,Int)]
tuplasGrafoDistancias [] =[] 
tuplasGrafoDistancias l = [(ciudadToNum (l!!0), ciudadToNum (l!!1),ciudadToNum (l!!2))] ++tuplasGrafoDistancias(drop 4 l)

--obtiene Las tuplas que describen los valores de Costos entre ciudades para crear el grafo
tuplasGrafoCostos :: [String] -> [(Int,Int,Int)]
tuplasGrafoCostos [] = []
tuplasGrafoCostos l = [(ciudadToNum (l!!0), ciudadToNum (l!!1), ciudadToNum (drop 2 (l!!3)))] ++tuplasGrafoCostos(drop 4 l)

{---------Procesamiento de listas de listas anterioro---------
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
        else buscarDest o (drop 2 itinierarios)-}

--devuelve un string con la descripción de un camino
rutaImprimir :: [(Int,Int,Int)] -> String
rutaImprimir l = do 
                    let cidInt = depurarCaminos l
                    let oct = obtenerValoresYDeUnCamino cidInt
                    let transbordos = imprimirTrasbordos oct
                    let transbordosFix = take ((length transbordos) - 4) transbordos
                    transbordosFix

--presenta en pantalla los resultados de la descripción del camino
printPantalla :: [(Int,Int,Int)] -> Int -> Int -> IO ()
printPantalla l orig dest = do  
                            let transbordos = rutaImprimir l
                            let text = "Ruta Optima: desde " ++ numToCiudad orig ++ " a " ++ numToCiudad dest
                            print (text)   
                            putStr "Pasando por: " 
                            print (transbordos)  

--Calcula el tiempo de vuelo con el total de la distancia de un camino
--recibe un camino y devulve el total del tiempo 
tiempoDeVuelo :: [(Int,Int,Int)] -> Float
tiempoDeVuelo l = do  
                    let distNum = obtenerZ (head l)
                    let tempD = fromIntegral distNum :: Float
                    let impr = tempD/velocidadVuelo
                    impr
--obtiene el Costo total de un camino determinado
--recibe un camino y devuelve un valor
costoDeVuelo :: [(Int,Int,Int)] -> Int
costoDeVuelo l = obtenerZ (head l)