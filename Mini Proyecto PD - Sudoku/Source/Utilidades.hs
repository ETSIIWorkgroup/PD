-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para ayudar a la ejecución del juego.

module Source.Utilidades (
    esOpcionValida,
    finalSimple,
    finalAvanzado,
    incrementaAvanzado,
    decrementaAvanzado,
    incrementaFallos,
    incrementaSimple,
    esCompleto,
    compruebaPosicion
) where

-- Librerias importadas
import Data.Array
import Source.Tipos
import Source.Tablero
import Source.Ranking

{-
    Función que actualice el contador existente, se suma n a la columna correspondiente
    (jugadas, pistas, opciones).

    Usa tipos de datos de registro
-}
incrementaSimple :: ContadorSimple -> Int -> Int -> Int -> ContadorSimple
incrementaSimple (ContS nombreS jugadasS pistas opciones) x z y = ContS nombreS (jugadasS + x) (pistas + z) (opciones + y)

{-
    Función que valida el resultado; dado un sudoku completo, comprobamos su
    resultado frente a la solución. Muestra el nº de jugadas, veces que se han pedido
    pistas y veces que se han consultado opciones.

    Usa guardas
-}
finalSimple :: Sudoku -> Sudoku -> ContadorSimple -> IO()
finalSimple tablero resultado contador
    | validaSudoku tablero resultado == True = do
        putStrLn "¡SUDOKU COMPLETADO!"
        putStrLn $ "JUGADAS: " ++ show (jugadasS contador) ++ " PISTAS: " ++ show (pistas contador) ++ " OPCIONES CONSULTADAS: " ++ show (opciones contador)
    | otherwise = do
        putStrLn "¡VUELVE A INTENTARLO!"

{-
    Función que actualice el contador existente, se suma x a puntuacion.

    Usa tipos de datos de registro
-}
incrementaAvanzado :: ContadorAvanzado -> Int -> ContadorAvanzado
incrementaAvanzado (ContA nombreA fallos puntuacion) x = ContA nombreA (fallos + 0) (puntuacion + x)

{-
    Función que actualice el contador existente, se resta x a puntuacion.
    
    Usa tipos de datos de registro
-}
decrementaAvanzado :: ContadorAvanzado -> Int -> ContadorAvanzado
decrementaAvanzado (ContA nombreA fallos puntuacion) x = ContA nombreA (fallos + 0) (puntuacion - x)

{-
    Función que actualice el contador existente, se suma x a fallos.

    Usa tipos de datos de registro
-}
incrementaFallos :: ContadorAvanzado -> Int -> ContadorAvanzado
incrementaFallos (ContA nombreA fallos puntuacion) x = ContA nombreA (fallos + x) (puntuacion + 0)

{-
    Función que cierra el juego. Dado el contador, muestra el nº de jugadas que ha
    tomado al jugador resolver el puzzle.

    Usa case of
-}
finalAvanzado :: ContadorAvanzado -> IO()
finalAvanzado contador = do
    putStrLn $ "FALLOS: " ++ show (fallos contador)
    putStrLn $ "PUNTUACIÓN: " ++ show (puntuacion contador)
    putStrLn "¿Guardar puntuación? (S/N)"
    seleccion <- getLine
    case seleccion of
        "S" -> escribeResultado contador
        "s" -> escribeResultado contador
        "N" -> return ()
        "n" -> return ()
        _ -> finalAvanzado contador

{-
    Función que persiste en un fichero la puntuación del jugador.

    Recibe el contador del cual se extrae la puntuación y pide al jugador
    su nombre por teclado.

    Usa escritura de ficheros
-}
escribeResultado :: ContadorAvanzado -> IO()
escribeResultado contador = do
    putStrLn "NOMBRE: "
    nombre <- getLine
    appendFile "Files/puntuacion.txt" (formateaPuntuacion contador nombre)
    putStrLn "¡GUARDADO!"
    putStrLn "\n"
    ranking "Files/puntuacion.txt"

{-
    Función auxiliar para formatear la puntuación junto al nombre del jugador
    en el fichero seleccionado. Se extrae la puntuación del registro ContadorAvanzado.

    Usa tipos de datos de registro
-}
formateaPuntuacion :: ContadorAvanzado -> String -> String
formateaPuntuacion contador nombre = "Jugador: " ++ nombre ++ " Puntuación: " ++ show (puntuacion contador) ++ " Fallos: " ++ show (fallos contador) ++ "\n"

{-
    Función que dado un sudoku completo (no quedan casillas por rellenar), 
    compruebe dicho tablero frente a la solución. 
-}
validaSudoku :: Sudoku -> Sudoku -> Bool
validaSudoku tableroResuelto resultado = tableroResuelto == resultado   

{-
    Función que dado un sudoku compruebe si está completo o no; esto es, 
    si hay casillas aún por rellenar o no.

    Ya que el tablero es de tipo Char, y las casillas por rellenar serán
    'X', comprobamos que no queden 'X's en los elementos del tablero.
-}
esCompleto :: Sudoku -> Bool
esCompleto tablero = not ('X' `elem` elems tablero)
    
{-
    Función auxiliar para validar posibles valores que pueden ir dentro de un 
    bloque de Sudoku por resolver. Se usa en la función 'opciones'.

    Usamos tres funciones auxiliares para comprobar que valores están en las 
    filas, columnas y en el bloque seleccionado.

    Para ubicar en cual de los nueve bloques del tablero está el elemento
    seleccionado, usamos una cuarta función auxiliar, 'localizaBloque'.

    'localizaBloque' es usado internamente por valoresBloque cuando recorre una
    lista de compresión, pero es llamado inicialmente con la posición pasada (fila,
    columna) para ubicar el bloque seleccionado.
-}
esOpcionValida :: Sudoku -> Char -> Posicion -> Bool
esOpcionValida tablero valor (fila,columna) = 
    notElem valor (filaIesima tablero fila) && 
    notElem valor (columnaIesima tablero columna) &&
    notElem valor (valoresBloque tablero (localizaBloque (fila,columna)))

{-
    Función que valida la posición introducida por teclado, es decir, dado el resultado y
    el valor introducido devolverá si es true/false en cada caso.

    Usa guardas
-}
compruebaPosicion :: Sudoku -> Posicion -> Char -> Bool
compruebaPosicion resultado posicion valor
    | valor == resultado ! posicion = True
    | otherwise                     = False

--