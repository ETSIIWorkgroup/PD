-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para formatear la impresión del tablero en un formato más legible.

module Source.Formateado (
    formateaTableroAvanzado
) where

-- Librerias importadas
import System.IO
import Data.Array
import Source.Tipos
import Source.Seguridad
import Data.List.Split
import Data.List (intercalate)

{-
    Función que cargue el tablero del juego, elimine los caracteres especiales y luego transforme
    dicho tablero en un formato más nuevo que por último se lee por pantalla. 
    
    Usa lectura de ficheros
-}
formateaTableroAvanzado :: IO()
formateaTableroAvanzado = do
    -- Cargar el tablero
    tablero <- leerSeguro "Files/tablero.txt"
    let tableroLeido = parseaTablero tablero

    -- Conversión sin caracteres especiales
    matrizALimpio tableroLeido

    -- Conversión a nuevo formato
    escribeTableroAvanzado "Files/tablero-limpio.txt"

    -- Mostrar por pantalla el nuevo tablero
    leeTableroFormateado

{-
    Función auxiliar para mostrar el tablero, separada de la anterior para evitar bloqueos en
    ficheros.

    Usa lectura de ficheros
-}
leeTableroFormateado :: IO()
leeTableroFormateado = do
    tableroFinal <- leerSeguro "Files/tablero-formateado-avanzado.txt"
    putStrLn tableroFinal

{-
    Función que dado el contenido de un tablero en fichero, usa 'words' para
    leer línea a línea (y map) para crear un array bidimensional
    de 9 x 9 (un sudoku) con ese contenido.

    Usa funciones de orden superior
-}
parseaTablero :: String -> Sudoku
parseaTablero lineas = listArray dimensiones (map read (words lineas))
    where dimensiones = ((1,1),(9,9))

-- A partir de aquí todo son funciones auxiliares para el tratamiento del tablero:

{-
    Función que dado el array (el sudoku) limpia línea a línea resultando en un tablero
    sin caracteres especiales, solo los números del sudoku. Escribe el resultado a un 
    nuevo fichero para trabajar más fácil.

    Hemos tenido que recurrir a withFile que maneja el cierre de ficheros correctamente para 
    evitar problemas del tipo: Files/tablero.txt: withFile: resource busy (file is locked)
    
    Usa escritura de ficheros
-}
matrizALimpio :: Sudoku -> IO()
matrizALimpio matriz = do 

    -- Obtener los elementos del array bidimensional
    let elementos = elems matriz
    let filas = chunksOf 9 elementos

    -- Por cada línea, la límpia y escribe el resultado en un fichero intermedio
    let lineas = concat [lines x | x <- filas]
    let alinear = unwords lineas
    let nuevaLinea = convertirEspaciosANuevaLinea alinear
    withFile "Files/tablero-limpio.txt" WriteMode (\handle -> hPutStr handle nuevaLinea)

{-
    Función que partiendo del fichero limpio, inserte los caracteres necesarios por filas y por 
    columnas, escribiendo el resultado en un fichero intermedio que luego será mostrado por pantalla 
    de forma independiente al fichero "tablero.txt".

    Hemos tenido que recurrir a withFile que maneja el cierre de ficheros correctamente para 
    evitar problemas del tipo: Files/tablero.txt: withFile: resource busy (file is locked)

    Usa escritura de ficheros, listas por compresión y funciones de orden superior
-}
escribeTableroAvanzado :: FilePath -> IO()
escribeTableroAvanzado fichero = do
    contenido <- leerSeguro fichero
    let linea = "------+-------+------"
    let filas =  words contenido
    let agrupar3 = chunksOf 3 filas
    let bloques3 =  [ chunksOf 3 x | xs <- agrupar3, x <- xs]
    let barrita = concat [ x ++ "|"| xs <- bloques3 , x <- xs]
    let agrupar = words [ x | x <- barrita]
    let agruparFilas =  [ chunksOf 12 xs | xs <- agrupar]
    let inicio = [ init x | xs <- agruparFilas, x <- xs]
    let agrupar3otravez = chunksOf 3 [ x | x <- inicio]
    let concatena = [ concat xs | xs <- agrupar3otravez]
    let lineas = intercalate linea concatena
    let separar = init (agruparContenido lineas)
    let unir = concat [ dividir9 xs | xs <- separar]
    let alinear = unwords unir
    let nuevaLinea = convertirEspaciosANuevaLinea alinear
    let espaciar = agregarEspacios [ x | x <- nuevaLinea]
    withFile "Files/tablero-formateado-avanzado.txt" WriteMode (\handle -> hPutStr handle espaciar)

{-
    Función auxiliar que dado una lista de caracteres inserte un salto de línea cuando encuentre
    un espacio. 

    Usa funciones de orden superior con una lambda
-}
convertirEspaciosANuevaLinea :: String -> String
convertirEspaciosANuevaLinea = map (\caracter -> if caracter == ' ' then '\n' else caracter)

{-
    Función axuliar que dado una lista de caracteres agregue espacios entre los caracteres propios
    del sudoku (0123456789X|) dado que son chars. 

    Usa patrones, recursión y guardas
-}
agregarEspacios :: String -> String
agregarEspacios [] = []
agregarEspacios (x:xs)
  | esNumero x = x : ' ' : agregarEspacios xs
  | otherwise = x : agregarEspacios xs
  where esNumero c = elem c "0123456789X|"

{-
    Función auxiliar que dado una lista agrupe los elementos en trozos de longitud específica que 
    coindicen con las dimensiones del fichero del tablero. 

    Usa patrones y recursión
-}
agruparContenido :: [a] -> [[a]]
agruparContenido [] = []
agruparContenido lista = take 33 lista : take 21 (drop 33 lista) : agruparContenido (drop 54 lista)

{-
    Función auxiliar que dado una lista de elementos toma trozos de un tamaño prefijado por las
    dimensiones del fichero del tablero. 

    Usa patrones y guardas
-}
dividir9 [] = []
dividir9 (x:xs) 
    | notElem x lista = chunksOf 22 (x:xs)
    | otherwise = chunksOf 11 (x:xs) 
    where lista = ['X','1','2','3','4','5','6','7','8','9']

--