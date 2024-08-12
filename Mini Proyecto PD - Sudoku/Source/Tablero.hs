-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para representar el juego por consola, leer
-- valores, comprobar los valores del tablero, etc.

module Source.Tablero (
    leeFila,
    leeColumna,
    leeValor,
    leerSudoku,
    valoresBloque,
    localizaBloque,
    actualizaSudoku,
    filaIesima, 
    columnaIesima,
    escrituraTablero,
    formateaTableroParaSalida
) where

-- Librerias importadas
import System.IO
import Data.Array
import Source.Tipos
import Source.Seguridad
import Data.List.Split
import Data.Maybe (fromJust)
import qualified Data.Map as Map

-- Lectura de valores

{-
    Función que capture la entrada por teclado para la lectura de la fila.
    Comprueba si el valor leido es válido mediante 'esValido'.

    Usa if/elses
-}
leeFila :: IO Int
leeFila = do
    fila <- getLine
    if esValido fila then do
        let filaLeida = read fila :: Int 
        return filaLeida
    else do
        putStrLn "FILA INVÁLIDA"
        leeFila

{-
    Función que capture la entrada por teclado para la lectura de la columna.
    Comprueba si el valor leido es válido mediante 'esValido'.

    Usa if/elses
-}
leeColumna :: IO Int
leeColumna = do
    columna <- getLine
    if esValido columna then do
        let columnaLeida = read columna :: Int 
        return columnaLeida
    else do
        putStrLn "COLUMNA INVÁLIDA"
        leeColumna

{-
    Función que capture la entrada por teclado para la lectura del valor a insertar.
    Comprueba si el valor leido es válido mediante 'esValorValido'.

    Dado que el tablero es de tipo Char hacemos una conversión tras la lectura del 
    valor entero a un Char.

    Usa if/elses
-}
leeValor :: IO Char
leeValor = do
    valor <- getLine
    if esValorValido valor then do
        -- Aquí convertimos el entero en un Char
        let valorLeido = read valor :: Int
        let valorConvertido = transformaAChar valorLeido 
        return valorConvertido
    else do
        putStrLn "VALOR INVÁLIDO"
        leeValor

{-
    Función auxiliar que dado un entero (que ya hemos comprobado que es válido), lo
    transforme a su valor como caracter (Char) para ser introducido en el tablero.

    Para simplificar la conversión, sabiendo de antemano todos los valores posibles,
    vamos a usar un Map<Int,Char> en el que ya declaramos la conversión 1:1.

    Dado que los mapas devuelven siempre: Maybe Tipo, nosotros forzamos a que el tipo
    sea solo Char usando la librería Maybe.

    Usa librerias adicionales y guardas
-}
conversion :: Map.Map Int Char
conversion = Map.fromList [(1, '1'),(2, '2'),(3, '3'),(4, '4'),(5, '5'),(6, '6'),(7, '7'),(8, '8'),(9, '9')]

transformaAChar :: Int -> Char
transformaAChar numero
    | Map.member numero conversion == False = error "NO EXISTE"
    | otherwise                             = fromJust (Map.lookup numero conversion)

-- Comprobaciones valores

{-
    Función que compruebe si el valor leido es un valor válido.

    Para este caso, se leen filas o columnas como String por teclado.
    El tablero 9x9 es de tipo Array asi que los valores posibles se
    mueven entre el 1 y el 9.

    Usa funciones de orden superior
-}
esValido :: String -> Bool
esValido xs = elem xs ["1","2","3","4","5","6","7","8","9"]

{-
    Función que compruebe si el valor leido es un valor válido.

    Para este caso, se leen valores a insertar como Integers por teclado.

    Dado que nuestro tablero es de tipo Char, los elementos leidos por
    teclado serán enteros (para facilitar su introducción al usuario) pero
    luego serán convertidos a Char.

    Usa funciones de orden superior
-}
esValorValido :: String -> Bool
esValorValido xs = elem xs ["1","2","3","4","5","6","7","8","9"]

-- Lectura del tablero

{-
    Función que dado un fichero (nombre de un fichero) usa leerSeguro para
    cargar el contenido y convertirlo en un sudoku. 

    Usa parseaTablero para convertir las filas del fichero en un sudoku.
-}
leerSudoku :: String -> IO Sudoku
leerSudoku fichero = do
    tablero <- leerSeguro fichero
    let sudoku = parseaTablero tablero
    return sudoku 

{-
    Función que dado el contenido de un tablero en fichero, usa 'words' para
    leer línea a línea (y map) para crear un array bidimensional
    de 9 x 9 (un sudoku) con ese contenido.

    Usa funciones de orden superior
-}
parseaTablero :: String -> Sudoku
parseaTablero lineas = listArray dimensiones (map read (words lineas))
    where dimensiones = ((1,1),(9,9))

{-
    Función que dado un tablero y el bloque a ubicar, devuelve una lista 
    de los valores de ese bloque para validar que son únicos (en un Sudoku no pueden repetirse
    números dentro de un mismo bloque de nueve valores).

    Como inicialmente valoresBloque ha sido llamado junto a localizaBloque, ya conocemos el 
    bloque en el que estamos (del 1 al 9), ahora recorremos todas las posiciones del tablero
    comprobando si el bloque en el que estamos corresponde y recogemos los valores para los 
    que si corresponda.

    Usa listas por compresión
-}
valoresBloque :: Sudoku -> Int -> [Char]
valoresBloque tablero bloque = [tablero ! (i,j) | i <- [1..filas], j <- [1..columnas], localizaBloque (i,j) == bloque]
  where ((_,_),(filas,columnas)) = bounds tablero

{-
    Función que dado una posición (fila / columna), nos devuelve el bloque (conjunto de
    nueve valores) del tablero para esa posición. Dado que un tablero tiene esta pinta:

    *---*---*---*---*---*---*---*---*---*
    |           |           |           |
    |     1     |     2     |     3     |
    |           |           |           |
    *---*---*---*---*---*---*---*---*---*
    |           |           |           |
    |     4     |      5    |     6     |
    |           |           |           |
    *---*---*---*---*---*---*---*---*---*
    |           |           |           |
    |     7     |     8     |     9     |
    |           |           |           |
    *---*---*---*---*---*---*---*---*---*

    , como cada 3 elementos hacía arriba/abajo y izquierda/derecha representa un bloque,
    calculamos el bloque como: 

    * Obtener la primera fila del bloque, para lo que siempre restamos uno (por si fuera 9),
    y hacemos filaAjustada div 3.

    * Obtener la primera columna del bloque, ara lo que siempre restamos uno (por si fuera 9),
    y hacemos columnaAjustada div 3.

    La suma de ambas dimensiones ajustadas por 3 es el bloque seleccionado del 1 al 9.
-}
localizaBloque :: Posicion -> Int
localizaBloque (fila,columna) = 1 + 3 * (filaAjustada `div` 3) + (columnaAjustada `div` 3)
    where filaAjustada = (fila - 1)
          columnaAjustada = (columna - 1)

{-
    Función que dado una fila devuelve los elementos del sudoku para esa fila.

    Usa listas por compresión
-}
filaIesima :: Sudoku -> Int -> [Char]
filaIesima tablero filaSeleccionada = [tablero ! (i,j) | i <- [1..filas], j <- [1..columnas], i == filaSeleccionada]
  where ((_,_),(filas,columnas)) = bounds tablero

{-
    Función que dado una columna devuelve los elementos del sudoku para esa columna.

    Usa listas por compresión
-}
columnaIesima :: Sudoku -> Int -> [Char]
columnaIesima tablero columnaSeleccionada = [tablero ! (i,j) | i <- [1..filas], j <- [1..columnas], j == columnaSeleccionada]
  where ((_,_),(filas,columnas)) = bounds tablero

-- Actualizaciones del tablero

{-
    Función que dado un tablero, una posición y un caracter, 
    escribe dicho caracter sobre la posición seleccionada y 
    devuelve dicho tablero actualizado.

    Usamos el operador // que es la inversa de ! (usado para retornar 
    el valor dada una posición); en este caso array // [(x,y),valor] coloca
    el valor en el array dada una posición.
-}
actualizaSudoku :: Sudoku -> Posicion -> Char -> Sudoku
actualizaSudoku tablero posicion valor = tablero // [(posicion, valor)]

{-
    Función que dado un sudoku, persista dicho tablero sobre el fichero
    modificable que usamos en cada iteración.

    Convertimos el sudoku en lista para formatear la salida de forma que
    cada elemento esté espaciado; luego guardamos la salida en el fichero
    para mostrarla por consola.

    Hemos tenido que recurrir a withFile que maneja el cierre de ficheros correctamente para 
    evitar problemas del tipo: Files/tablero.txt: withFile: resource busy (file is locked)

    Usa escritura de ficheros
-}
escrituraTablero :: Sudoku -> IO()
escrituraTablero tablero = do 
  
  -- Obtenemos las columnas del sudoku
  let ((_,_),(_,columnas)) = bounds tablero
  
  -- Convierte el sudoku a una lista
  let lista = elems tablero

  -- Formatea los elementos para que tengan un espacio, esto es,
  -- cada elemento de la lista se concatena con un espacio.
  let listaFormateada = [(show x) ++ " " | x <- lista]

  -- Formatea la lista anterior para devolver una lista de listas, 
  -- cada lista representa una fila del tablero.
  let listaListas = chunksOf columnas listaFormateada

  -- Usamos unlines que añadirá un salto de linea tras cada lista, 
  -- escribiendo asi el tablero de nuevo. Aplica concat a cada linea
  -- porque ya es una lista de listas.
  let salida = unlines (map concat listaListas)

  -- Escribe la salida en el archivo tablero.txt
  withFile "Files/tablero.txt" WriteMode (\handle -> hPutStr handle salida)


-- Formateo del tablero leido por motivos de legibilidad

{-
    Función que lea el tablero y aplique un formateo al fichero
    , escribiendo el resultado a un fichero temporal que mostraremos
    por pantalla. 

    Hemos tenido que recurrir a withFile que maneja el cierre de ficheros correctamente para 
    evitar problemas del tipo: Files/tablero.txt: withFile: resource busy (file is locked)

    Usa lectura/escritura de ficheros
-}
formateaTableroParaSalida :: IO()
formateaTableroParaSalida = do
    tableroLeido <- leerSeguro "Files/tablero.txt"

    -- Filtramos las comillas
    let lineasFiltradas = map filtro (lines tableroLeido)

    -- Colocamos un separador cada 3 filas
    let lineasSeparadas = concatMap (\(i, linea) -> linea : if i `mod` 3 == 0 then ["------------------"] else []) (zip [1..] lineasFiltradas)

    -- A cada cadena, cada 3 caracteres le añadimos un '|'
    let lineasConSeparadores = map agregarSeparadores lineasSeparadas
    
    -- Escribimos el resultado en el archivo
    withFile "Files/tablero-formateado.txt" WriteMode (\handle -> hPutStr handle (unlines lineasConSeparadores))

    -- Leemos y mostramos el archivo formateado con espacios
    tableroFormateado <- leerSeguro "Files/tablero-formateado.txt"
    putStrLn "\n"
    putStrLn tableroFormateado
    putStrLn "\n"

-- Método auxiliar para ser aplicado como filtro mediante un map
filtro :: String -> String
filtro = filter (\caracter -> caracter /= '\'')

{-
    Función que dado un String, concatena un separador (caracter "|")
    cada 6 elementos, quedando así la línea como: 1 2 3 | 4 5 6 | 7 8 9

    Usa recursividad y patrones
-}
agregarSeparadores :: String -> String
agregarSeparadores [] = []
agregarSeparadores lista = take 6 lista ++ "|" ++ agregarSeparadores (drop 6 lista)

--