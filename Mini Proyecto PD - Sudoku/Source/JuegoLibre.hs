-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para implementar un modo de juego libre.

module Source.JuegoLibre (
    iniciaJuegoLibre
) where

-- Librerias importadas
import Data.Array
import Source.Tipos
import Source.Tablero
import Source.Utilidades

jugador :: ContadorSimple
jugador = ContS "Jugador" 0 0 0

{-
    Función que muestre por pantalla la selección de dificultad y lea por
    teclado la selección del jugador; si la selección no es correcta se vuelve
    a pedir al usuario. 

    Una vez seleccionado, se invoca a carga, que inicia el tablero elegido.

    Usa case of y recursión
-}
iniciaJuegoLibre :: IO()
iniciaJuegoLibre = do 
    putStrLn "\n SELECCIÓN DE DIFICULTAD "
    putStrLn "\t 1 FÁCIL "
    putStrLn "\t 2 MEDIO "
    putStrLn "\t 3 DIFÍCIL "
    seleccion <- getLine
    case seleccion of "1" -> carga "Files/facil.txt"   "Files/facil-sol.txt" jugador
                      "2" -> carga "Files/medio.txt"   "Files/medio-sol.txt" jugador 
                      "3" -> carga "Files/dificil.txt" "Files/dificil-sol.txt" jugador
                      "d" -> carga "Files/debug.txt"   "Files/debug-sol.txt" jugador   -- Opción oculta para debugear
                      _   -> iniciaJuegoLibre

{-
    Función que cargue dos ficheros (tablero original y resuelto), los 'almacena' en 
    variables y escribe el valor del suduko leido un fichero blanco para no alterar el 
    tablero original.

    Sobre este fichero modificable se irá iterando durante la resolución del puzzle.
    Para ello, se llama a la función comienzo con los valores del sudoku original y su solución.
-}
carga :: String -> String -> ContadorSimple -> IO()
carga original resultado contador = do
    sudokuOriginal  <- leerSudoku original
    sudokuResultado <- leerSudoku resultado
    
    -- Carga el sudoku original sobre un fichero modificable
    escrituraTablero sudokuOriginal 

    -- Arranca la primera iteración del juego con el tablero
    comienzoLibre sudokuOriginal sudokuResultado contador

{-
    Función que partiendo del tablero inicial llama a iteracion, cada vez que 
    iteramos sobre el tablero volvemos a esta función para comprobar si está 
    completo o no. 

    Si el puzzle está completo, llamamos a la función que valida el resultado.

    Usa if/elses
-}
comienzoLibre :: Sudoku -> Sudoku -> ContadorSimple -> IO()
comienzoLibre tablero resultado contador = do
    if esCompleto tablero then do 
        finalSimple tablero resultado contador
    else do 
        iteracion tablero resultado contador

{-
    Función recursiva que permite al usuario seleccionar la posición que quiere
    modificar; se llama de forma recursiva para ir rellenando valores.

    Usa case of y recursión
-}
iteracion :: Sudoku -> Sudoku -> ContadorSimple -> IO()
iteracion tablero resultado contador = do 
    
    -- Persiste el estado del Sudoku en el fichero modificable
    escrituraTablero tablero

    -- Muestra el tablero
    formateaTableroParaSalida

    -- Obtiene por teclado la fila y columna del tablero a actualizar
    putStrLn "FILA: "
    fila <- leeFila
    putStrLn "COLUMNA: "
    columna <- leeColumna

    let posicion = (fila, columna)

    putStrLn "\t 1 ESCRIBIR NÚMERO "
    putStrLn "\t 2 VER POSIBILIDADES "
    putStrLn "\t 3 OBTENER PISTA "
    seleccion <- getLine
    case seleccion of "1" -> escrituraValor tablero resultado posicion contador
                      "2" -> Source.JuegoLibre.opciones tablero resultado posicion contador
                      "3" -> pista tablero resultado posicion contador
                      _ -> iteracion tablero resultado contador

{-
    Función que dado un sudoku y una posición de dicho sudoku, escriba 
    en dicha posición un valor pasado por teclado.
-}
escrituraValor :: Sudoku -> Sudoku -> Posicion -> ContadorSimple -> IO()
escrituraValor tablero resultado posicion contador = do
    
    -- Obtiene por teclado el valor a escribir
    putStrLn "VALOR: "
    valor <- leeValor

    -- Incrementa el contador
    let contadorActualizado = incrementaSimple contador 1 0 0

    -- Genera el nuevo sudoku resultante de escribir ese valor
    let tableroActualizado = actualizaSudoku tablero posicion valor
   
    -- Persiste el nuevo sudoku sobre el fichero modificable
    escrituraTablero tableroActualizado

    -- Vuelve a iterar sobre el tablero
    comienzoLibre tableroActualizado resultado contadorActualizado

{-
    Función que dado una posición y un sudoku no completo, devuelva una lista
    de los posibles valores que pueden insertarse en ese cuadrante (nueve valores
    por cuadrantes).

    Comprobamos los valores del 1 al 9 que no están en el bloque.
-}
opciones :: Sudoku -> Sudoku -> Posicion -> ContadorSimple -> IO()
opciones tablero resultado posicion contador = do

    -- Extraemos los posibles valores y los mostramos
    let valores = [opcion | opcion <- ['1','2','3','4','5','6','7','8','9'], esOpcionValida tablero opcion posicion]
    putStrLn $ "POSIBILIDADES: " ++ concat (map show valores)

    -- Incrementa el contador
    let contadorActualizado = incrementaSimple contador 0 1 0

    -- Volvemos a llamar a comienzo para seguir jugando
    comienzoLibre tablero resultado contadorActualizado

{-
    Función que dado el tablero y su solución rellene de forma automática una 
    casilla indicada a modo de ayuda.
-}
pista :: Sudoku -> Sudoku -> Posicion -> ContadorSimple -> IO()
pista tablero resultado (fila,columna) contador = do
    
    -- Obtener el valor del sudoku resuelto para dicha posicion
    let valor = resultado ! (fila,columna)

    -- Actualizar el tablero del juego con ese valor
    let tableroActualizado = actualizaSudoku tablero (fila,columna) valor

    -- Persiste el nuevo sudoku sobre el fichero modificable
    escrituraTablero tableroActualizado

    -- Incrementa el contador
    let contadorActualizado = incrementaSimple contador 0 0 1

    -- Vuelve a iterar sobre el tablero
    comienzoLibre tableroActualizado resultado contadorActualizado

--