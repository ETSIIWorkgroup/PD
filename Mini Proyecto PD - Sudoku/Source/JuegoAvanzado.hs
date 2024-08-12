-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para implementar un modo de juego autoguiado.

module Source.JuegoAvanzado (
    iniciaJuegoAvanzado
) where

-- Librerias importadas
import Data.Array
import Source.Tipos
import Source.Tablero
import Source.Utilidades
import Source.Formateado

jugador :: ContadorAvanzado
jugador = ContA "Jugador" 0 0

{-
    Función que muestre por pantalla la selección de dificultad y lea por
    teclado la selección del jugador; si la selección no es correcta se vuelve
    a pedir al usuario. 

    Una vez seleccionado, se invoca a carga, que inicia el tablero elegido.

    Usa case of y recursión
-}
iniciaJuegoAvanzado :: IO()
iniciaJuegoAvanzado = do 
    putStrLn "\n SELECCIÓN DE DIFICULTAD "
    putStrLn "\t 1 FÁCIL "
    putStrLn "\t 2 MEDIO "
    putStrLn "\t 3 DIFÍCIL "
    seleccion <- getLine
    case seleccion of "1" -> carga "Files/facil.txt"   "Files/facil-sol.txt" jugador
                      "2" -> carga "Files/medio.txt"   "Files/medio-sol.txt" jugador
                      "3" -> carga "Files/dificil.txt" "Files/dificil-sol.txt" jugador
                      "d" -> carga "Files/debug.txt"   "Files/debug-sol.txt" jugador   -- Opción oculta para debugear
                      _   -> iniciaJuegoAvanzado

{-
    Función que cargue dos ficheros (tablero original y resuelto), los 'almacena' en 
    variables y escribe el valor del suduko leido un fichero blanco para no alterar el 
    tablero original.

    Sobre este fichero modificable se irá iterando durante la resolución del puzzle.
    Para ello, se llama a la función comienzo con los valores del sudoku original y su solución.

    Usa lectura de ficheros
-}
carga :: String -> String -> ContadorAvanzado -> IO()
carga original resultado contador = do
    sudokuOriginal  <- leerSudoku original
    sudokuResultado <- leerSudoku resultado
    
    -- Carga el sudoku original sobre un fichero modificable
    escrituraTablero sudokuOriginal 

    -- Arranca la primera iteración del juego con el tablero
    comienzoAvanzado sudokuOriginal sudokuResultado contador

{-
    Función que partiendo del tablero inicial llama a iteracion, cada vez que 
    iteramos sobre el tablero volvemos a esta función para comprobar si está 
    completo o no. 

    Si el puzzle está completo, llamamos a la función que valida el resultado.

    Usa guardas
-}
comienzoAvanzado :: Sudoku -> Sudoku -> ContadorAvanzado -> IO()
comienzoAvanzado tablero resultado contador
    | esCompleto tablero == True = do
        finalAvanzado contador
    | otherwise = do 
        iteracion tablero resultado contador

{-
    Función recursiva que permite al usuario seleccionar la posición que quiere
    modificar; se llama de forma recursiva para ir rellenando valores.

    Usa case of y recursión
-}
iteracion :: Sudoku -> Sudoku -> ContadorAvanzado -> IO()
iteracion tablero resultado contador = do 
    
    -- Persiste el estado del Sudoku en el fichero modificable
    escrituraTablero tablero

    -- Muestra el tablero en un nuevo formato y el contador de fallos
    formateaTableroAvanzado
    putStrLn $ "FALLOS: " ++ show (fallos contador) ++ "\n"

    -- Obtiene por teclado la fila y columna del tablero a actualizar
    putStrLn "FILA: "
    fila <- leeFila
    putStrLn "COLUMNA: "
    columna <- leeColumna

    let posicion = (fila, columna)

    putStrLn "\t 1 ESCRIBIR NÚMERO "
    putStrLn "\t 2 VER POSIBILIDADES "
    seleccion <- getLine
    case seleccion of "1" -> escrituraValor tablero resultado posicion contador
                      "2" -> Source.JuegoAvanzado.opciones tablero resultado posicion contador
                      _ -> iteracion tablero resultado contador

{-
    Función que dado un sudoku y una posición de dicho sudoku, escriba 
    en dicha posición un valor pasado por teclado.

    Usa if/elses
-}
escrituraValor :: Sudoku -> Sudoku -> Posicion -> ContadorAvanzado -> IO()
escrituraValor tablero resultado posicion contador = do
    
    -- Obtiene por teclado el valor a escribir
    putStrLn "VALOR: "
    valor <- leeValor

    -- Comprobamos el valor introducido
    let valido = compruebaPosicion resultado posicion valor
    if valido then do
        -- Genera el nuevo sudoku resultante de escribir ese valor
        let tableroActualizado = actualizaSudoku tablero posicion valor
   
        -- Persiste el nuevo sudoku sobre el fichero modificable
        escrituraTablero tableroActualizado
        
        -- Incrementa la puntuación
        let contadorActualizado = incrementaAvanzado contador 100

        -- Vuelve a iterar sobre el tablero
        comienzoAvanzado tableroActualizado resultado contadorActualizado
    else do
        putStrLn "¡VALOR EQUIVOCADO!"

        -- Decrementa la puntuación
        let contadorActualizado = decrementaAvanzado contador 50

        -- Suma un fallo
        let contadorActualizadoConFallo = incrementaFallos contadorActualizado 1

        -- Vuelve a iterar sobre el tablero
        comienzoAvanzado tablero resultado contadorActualizadoConFallo

{-
    Función que dado una posición y un sudoku no completo, devuelva una lista
    de los posibles valores que pueden insertarse en ese cuadrante (nueve valores
    por cuadrantes).

    Comprobamos los valores del 1 al 9 que no están en el bloque.
-}
opciones :: Sudoku -> Sudoku -> Posicion -> ContadorAvanzado -> IO()
opciones tablero resultado posicion contador = do

    -- Extraemos los posibles valores y los mostramos
    let valores = [opcion | opcion <- ['1','2','3','4','5','6','7','8','9'], esOpcionValida tablero opcion posicion]
    putStrLn $ "POSIBILIDADES: " ++ concat (map show valores)

    -- Decrementa la puntuación
    let contadorActualizado = decrementaAvanzado contador 25

    -- Volvemos a llamar a comienzo para seguir jugando
    comienzoAvanzado tablero resultado contadorActualizado

--