{-

Copyright (c) 2014, Marcelo Sánchez & Raquel Villón
Todos los derechos reservados.

PROYECTO EN HASKELL PARA LENGUAJES DE PROGRAMACION.

   *******************************************
   *                                         *
   *         Buscador de Rutas Optimas       *
   *            Aeropuertos de EEUU          *
   *                                         *  
   * funcion principal: main                 *
   *                                         *
   *******************************************

El programa recibe un DataSet que contiene: los aeropuertos(nombre de la ciudad a la que pertenecen), distancias entre cada par de ciudades y el costo del viaje

ejemplo de uso: 
> main 
-}

import System.IO  
import Data.List.Split
import Data.List
import Data.Graph
import Data.Array
import Conversiones
import GHC.Float


{-crea un tipo Grafo que es un Array
referencia: http://www.glc.us.es/~jalonso/vestigium/el-tipo-abstracto-de-datos-de-los-grafos-en-haskell/
-}
type Grafo v p = Array v [(v,p)] 


-- VARIABLES GLOBALES ---------------------

-- velocidadVuelo: contiene la velocidad promedio a la que viajan los aviones (Km/h)
-- ya que los datos que se parsearán sólo contienen distanicas y precios, se necesita obtener el tiempo por medio de un calculo
-- para poder realizar correctamente la conversion (velocidad=distancia/tiempo  => tiempo = distancia/velocidad) se se hace un casting a Float del valor
velocidadVuelo = fromIntegral 420 :: Float
-- totalNodosGrafo: contiene el numero total de nodos del grafo ingresado
totalNodosGrafo = 70


-- MENUS ----------------------------------

{-Imprime en pantalla el menú principal del programa
entrada: recibe las tuplas para crear los grafos de Tiempo (gT) y Costo (gC)
permite acceder al menu de filtros o salir del programa-}
menuPrincipal :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> IO()
menuPrincipal gT gC = do 
                        putStrLn "\n====================\n.- Menú de Vuelos -.\n====================\n"
                        putStrLn "Bienvenido!"
                        putStrLn "\n\t 1. Nueva Busqueda\n\t 2. Salir\n\nQué desea hacer? (Ingrese el número) : "
                        opInicio <- getLine
                        let s = read (opInicio)::Int
                        case () of _
                                    | s==1      -> menuOptimizacion gT gC
                                    | s==2      -> putStr "\nExit..\nBuen viaje!\n"
                                    | otherwise -> menuPrincipal gT gC

{-Imprime en pantalla el menú de los métodos de optimización
entrada: recibe las tuplas para crear los grafos de Tiempo (gT) y Costo (gC)
permite crear un grafo para buscar caminos optimos considerando el tiempo o el costo de vuelo-}
menuOptimizacion :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> IO()
menuOptimizacion gT gC = do 
                        putStrLn "\n ++ Método de Optimización ++\n\t 1.  Tiempo\n\t 2.  Costo\n\t 3.  Menu Principal\n"
                        putStrLn "Seleccione una opción: "
                        opFiltro <- getLine
                        let s = read (opFiltro)::Int
                        case () of _
                                    | s==1      -> optimizacionTiempo gT gC
                                    | s==2      -> optimizacionCosto gC gT
                                    | s==3      -> menuPrincipal gT gC
                                    | otherwise -> menuOptimizacion gT gC


-- MANEJADORES ----------------------------

{-Funcion que maneja la optimización por TIEMPO
entrada: recibe las tuplas para crear los grafos de Tiempo (gT) y Costo (gC)
unicamente crea el grafo de distancia(gT) para buscar la ruta optima considerando el tiempo-}
optimizacionTiempo :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> IO()
optimizacionTiempo gT gC = do 
                        putStr "\n* Optimización - TIEMPO *\n"
                        putStr "Ingrese Ciudad de Origen: "
                        cOrigS <- getLine
                        let cOrig = ciudadToNum cOrigS
                        putStr "Ingrese Ciudad de Destino: "
                        cDestS <- getLine
                        let cDest = ciudadToNum cDestS
                        
                        let g = creaGrafo True (1,totalNodosGrafo) gT
                        let c = connect cOrig cDest g --obtiene todos los caminos entre dos nodos de un grafo
                        if null c 
                            then do
                                let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                                print msj
                            else do
                                let todos_caminos = valorTodosCaminos c -- devuelve una lista con todos los caminos y la suma de la ruta al principio de cada uno
                                let camino_mas_corto = minimum todos_caminos --saca el camino mas corto
                                printPantalla camino_mas_corto cOrig cDest
                                let tVuelo = tiempoDeVuelo camino_mas_corto
                                putStr "En (aprox horas): "
                                print (tVuelo)
                                putStr "___________________________________________________\n\n"
                                menuOptimizacion gT gC

{-Funcion que maneja la optimización por COSTO
entrada: recibe las tuplas para crear los grafos de Tiempo (gT) y Costo (gC)
unicamente crea el grafo de costo(gC) para buscar la ruta optima considerando el tiempo-}
optimizacionCosto :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> IO()
optimizacionCosto gC gT = do
                        putStr "\n* Optimización - COSTO *\n"
                        putStr "Ingrese Ciudad de Origen: "
                        cOrigS <- getLine
                        let cOrig = ciudadToNum cOrigS
                        putStr "Ingrese Ciudad de Destino: "
                        cDestS <- getLine
                        let cDest = ciudadToNum cDestS

                        let g = creaGrafo True (1,totalNodosGrafo) gC
                        let c = connect cOrig cDest g --obtiene todos los caminos entre dos nodos de un grafo
                        if null c 
                            then do
                                let msj = "No hay ruta entre "++cOrigS++" y " ++ cDestS
                                print msj
                            else do
                                let todos_caminos = valorTodosCaminos c -- devuelve una lista con todos los caminos y la suma de la ruta al principio de cada uno
                                let camino_mas_corto = minimum todos_caminos --saca el camino mas corto
                                printPantalla camino_mas_corto cOrig cDest
                                let costo= costoDeVuelo camino_mas_corto 
                                putStr "Costo de : $"
                                print costo 
                                putStr "___________________________________________________\n\n"
                                menuOptimizacion gT gC


-- PROCESAMIENTO DE DATOS------------------ 

{-entrada:recibe una serie de tuplas con los valores (nodo-origen, nodo-destino, pesoDelArco) necesarios para crear un grafo
salida: retorna un grafo dirigido-}
creaGrafo :: (Ix v, Num p) => Bool -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo d cs vs =
    accumArray 
     (\xs x -> xs++[x]) [] cs 
     ((if d then []
       else [(x2,(x1,p))|(x1,x2,p) <- vs, x1 /= x2]) ++
      [(x1,(x2,p)) | (x1,x2,p) <- vs])

{-funcion que busca todos los caminos posibles
entrada: connect recibe un numero de origen, un numero de destino y un grafo
devuelve una lista de listas de tuplas que representan todos los caminos posibles entre x y 
utiiza funciones de la libreria Graph propia de Haskell
referencia: http://stackoverflow.com/questions/11168238/haskell-generating-all-paths-between-nodes
-}
connect x y g = helper x y g [x]
  where
    helper a b g visited
        | a == b    = [[]]
        | otherwise = [(a,c,d):path | (c,d) <- g!a, c `notElem` visited, path <- helper c b g (c:visited)]

--obtiene el segundo valor de una tupla y lo devuelve 
obtenerY:: (Int,Int,Int) -> Int
obtenerY (x,y,z)= y 

--obtiene el tercer valor de las tupla y lo devuelve
obtenerZ:: (Int,Int,Int) -> Int
obtenerZ (x,y,z)= z

{-suma los terceros elementos de la lista de tuplas.
se obtiene un total de todas las distancias o costos (segun el grafo que se analice) de un camino que une 2 lugares (nodos)-}
sumaValorZ :: [(Int,Int,Int)] -> (Int,Int,Int)
sumaValorZ [] = (0,0,0)
sumaValorZ (x:xs) = (0,0, obtenerZ x + obtenerZ (sumaValorZ xs)) 

--agrega una tupla con la suma de las distancias/costos (tuplas (0,0,valor)) a un camino determinado
agregarSumaValorZ:: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
agregarSumaValorZ x y = head x : y 

--devuelve un lista de caminos con los totales de distancia/costo en cada uno al inicio en una tupla (0,0,suma)
valorTodosCaminos:: [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
valorTodosCaminos [] = []
valorTodosCaminos (x:xs)= agregarSumaValorZ [sumaValorZ x] x : valorTodosCaminos xs

--devuelve los valores de las ciudades intermedias de un cammino 
obtenerValoresYDeUnCamino :: [(Int,Int,Int)] -> [Int]
obtenerValoresYDeUnCamino []=[]
obtenerValoresYDeUnCamino (x:xs) = [obtenerY x] ++ obtenerValoresYDeUnCamino xs
                            
--remueve la primera y ultima tupla de cada camino para luego poder sacar los valores intermedios
depurarCaminos :: [(Int,Int,Int)] -> [(Int,Int,Int)]
depurarCaminos l = do 
                      let t = drop 1 l
                      let f = init t
                      f

--pasa de numeros a Strings las ciudades intermedias de una lista dada (camino de ciudades)
imprimirTrasbordos :: [Int] -> String
imprimirTrasbordos []=""
imprimirTrasbordos (x:xs) = do 
                            numToCiudad x ++ " -> " ++ imprimirTrasbordos xs

--obtiene las tuplas que describen los valores de Distancias entre ciudades para crear el grafo
tuplasGrafoDistancias:: [String] -> [(Int,Int,Int)]
tuplasGrafoDistancias [] =[] 
tuplasGrafoDistancias l = [(ciudadToNum (l!!0), ciudadToNum (l!!1),ciudadToNum (l!!2))] ++tuplasGrafoDistancias(drop 4 l)

--obtiene las tuplas que describen los valores de Costos entre ciudades para crear el grafo
tuplasGrafoCostos :: [String] -> [(Int,Int,Int)]
tuplasGrafoCostos [] = []
tuplasGrafoCostos l = [(ciudadToNum (l!!0), ciudadToNum (l!!1), ciudadToNum (drop 2 (l!!3)))] ++tuplasGrafoCostos(drop 4 l)

--devuelve un string con la descripción de un camino
obtenerSrtringRuta :: [(Int,Int,Int)] -> String
obtenerSrtringRuta l = do 
                        let cidInt = depurarCaminos l --obtengo las tuplas de nodos entre dos puntos
                        let oct = obtenerValoresYDeUnCamino cidInt --obtengo los identificadores de las ciudades (lista)
                        let transbordosTmp = imprimirTrasbordos oct -- almaceno el String con la ruta entre los nodos
                        let transbordos = take ((length transbordosTmp) - 4) transbordosTmp -- elimina el caracter " -> " del final de trasbordosTmp
                        transbordos

--imprime en pantalla los resultados de la descripción del camino
printPantalla :: [(Int,Int,Int)] -> Int -> Int -> IO ()
printPantalla l orig dest = do  
                            let transbordoStr = obtenerSrtringRuta l
                            let text = "Ruta Optima: desde " ++ numToCiudad orig ++ " a " ++ numToCiudad dest
                            putStr "___________________________________________________\n\n"
                            print (text)   
                            putStr "Pasando por: " 
                            print (transbordoStr)  

--calcula el tiempo de vuelo con el total de la distancia de un camino
--recibe un camino (arco-distancia) y devulve el total del tiempo 
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


main = do  
        handle <- openFile "DatasetSimple.txt" ReadMode --lee el dataset
        contents <- hGetContents handle -- devuelve un dato manejable 
        let lineas = splitOneOf ";\n" contents -- separa el contendido cada vez que encuentra un : o un salto de linea
        let gT = tuplasGrafoDistancias lineas --generacion de tuplas de distandias considerando sólo la distancia del dataset
        let gC = tuplasGrafoCostos lineas --generacion de tuplas de distandias considerando sólo el costo del dataset
        menuPrincipal gT gC -- menu de interaccion con el usuario 
        hClose handle