-- PD: entrada/salida
-- El juego del nim y las funciones de entrada/salida. 
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

------------------------------------------------------------------------
-- § Introducción                                                     --
------------------------------------------------------------------------

-- En el juego del nim el tablero tiene 5 filas numeradas de estrellas,
-- cuyo contenido inicial es el siguiente 
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
-- Dos jugadores retiran por turno una o más estrellas de una fila. El
-- ganador es el jugador que retire la última estrella. En este
-- ejercicio se va implementar el juego del Nim para practicar con las
-- funciones de entrada y salida.
-- Nota: El juego debe de ejecutarse en una consola, no en la shell de
-- emacs. 

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char
import Text.Printf

-- ---------------------------------------------------------------------
-- § Representación                                                   --
-- ---------------------------------------------------------------------
 
-- El tablero se representará como una lista de números indicando el
-- número de estrellas de cada fila. Con esta representación, el tablero
-- inicial es [5,4,3,2,1]. 

-- Representación del tablero.
type Tablero = [Int]

-- inicial es el tablero al principio del juego.
inicial :: Tablero
inicial = [5,4,3,2,1]

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    finalizado :: Tablero -> Bool
-- tal que (finalizado t) se verifica si t es el tablero de un juego
-- finalizado; es decir, sin estrellas. Por ejemplo,
--    finalizado [0,0,0,0,0]  ==  True
--    finalizado [1,3,0,0,1]  ==  False
-- ---------------------------------------------------------------------

-- Esto no es correcto, porque solo vale para tableros concretos:
finalizado :: Tablero -> Bool
finalizado t = t == [0,0,0,0,0]

-- Esto si es correcto:
finalizadoE :: Tablero -> Bool
finalizadoE t = sum t == 0

-- ---------------------------------------------------------------------
-- Ejecicio 2.2. Definir la función
--    valida :: Tablero -> Int -> Int -> Bool
-- tal que (valida t f n) se verifica si se puede coger n estrellas en
-- la fila f del tablero t y n es mayor o igual que 1. Por ejemplo,
--    valida [4,3,2,1,0] 2 3  ==  True
--    valida [4,3,2,1,0] 2 4  ==  False
--    valida [4,3,2,1,0] 2 2  ==  True
--    valida [4,3,2,1,0] 2 0  ==  False
-- ---------------------------------------------------------------------

-- t !! f es el elemento en la posición f de la lista t
-- Como es un índice y empieza en 0, hay que restar uno
valida :: Tablero -> Int -> Int -> Bool
valida t f n = (t !! (f - 1) >= n) && (n >= 1)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    jugada :: Tablero -> Int -> Int -> Tablero
-- tal que (jugada t f n) es el tablero obtenido a partir de t
-- eliminando n estrellas de la fila f. Por ejemplo,
--    jugada [4,3,2,1,0] 2 1  ==  [4,2,2,1,0]
-- ---------------------------------------------------------------------

jugada :: Tablero -> Int -> Int -> Tablero
jugada tablero fila n = [if i == fila then (x - n) else x | (x,i) <- zip tablero [1,2..], valida tablero fila n]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la acción
--    nuevaLinea :: IO ()
-- que consiste en escribir una nueva línea. Por ejemplo,
--    ghci> nuevaLinea
--    
--    ghci> 
-- ---------------------------------------------------------------------

-- Está mal explicado; consiste en añadir un salto de línea:
nuevaLinea :: IO ()
nuevaLinea = putChar '\n'

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    estrellas :: Int -> String
-- tal que (estrellas n) es la cadena formada con n estrellas. Por
-- ejemplo, 
--    ghci> estrellas 3
--    "* * * "
-- ---------------------------------------------------------------------

-- Esto funciona, pero no espacia los caracteres:
estrellas :: Int -> String
estrellas n = replicate n '*'

-- En su lugar hay que usar unwords:
-- Input: unwords ["aa","bb","cc","dd","ee"]
-- Output: "aa bb cc dd ee"
estrellasA :: Int -> String
estrellasA n = unwords ["*" | _ <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la acción
--    escribeFila :: Int -> Int -> IO ()
-- tal que (escribeFila f n) escribe en la fila f n estrellas. Por
-- ejemplo, 
--    ghci> escribeFila 2 3
--    2: * * *
-- ---------------------------------------------------------------------
 
-- putStrLn no sirve aquí, hace falta printf para formatear la salida usando
-- %d para enteros y %s para strings.
escribeFila :: Int -> Int -> IO ()
escribeFila f n = printf "%d: %s " f (estrellasA n)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la acción
--    escribeTablero :: Tablero -> IO ()
-- tal que (escribeTablero t) escribe el tablero t. Por
-- ejemplo,
--    ghci> escribeTablero [3,4,1,0,1]
--    1: * * * 
--    2: * * * * 
--    3: * 
--    4: 
--    5: * 
-- ---------------------------------------------------------------------

-- sequence_ es la forma de representar la lista como filas, de lo contrario entra
-- en conflicto con el tipo IO 
escribeTablero :: Tablero -> IO ()
escribeTablero tablero = sequence_ [escribeFila i n | (i,n) <- zip [1,2..] tablero]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la acción
--    leeDigito :: String -> IO Int
-- tal que (leeDigito c) escribe una nueva línea con l cadena "prueba",
-- lee un carácter y comprueba que es un dígito. Además, si el carácter
-- leido es un dígito entonces devuelve el entero correspondiente y si
-- no lo es entonces escribe el mensaje "Entrada incorrecta" y vuelve a
-- leer otro carácter. Por ejemplo,  
--    ghci> leeDigito "prueba "
--    prueba 3
--    3
--    ghci> leeDigito "prueba "
--    prueba c
--    ERROR: Entrada incorrecta
--    prueba 3
--    3
-- ---------------------------------------------------------------------

-- El enunciado está completamente mal explicado.
leeDigito :: String -> IO Int
leeDigito c = do
    putStrLn "prueba "
    numero <- getChar
    if isDigit numero then do
        return (read [numero])
    else do
        putStrLn "Entrada incorrecta"
        leeDigito c

leeDigit :: String -> IO Int
leeDigit n = do
    putStrLn n
    numero <- getChar
    if isDigit numero then
        return (read [numero])
    else do
        putStrLn "Entrada incorrecta"
        s <- leeDigit n
        return s

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los jugadores se representan por los números 1 y 2.
-- Definir la función 
--    siguiente :: Int -> Int
-- tal que (siguiente j) es el jugador siguiente de j. 
-- ---------------------------------------------------------------------

siguiente :: Int -> Int
siguiente n = case n of 1 -> 2
                        2 -> 1
                        _ -> error "Jugador no encontrado"

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la acción
--    juego :: Tablero -> Int -> IO ()
-- tal que (juego t j) es el juego a partir del tablero t y el turno del
-- jugador j. Por ejemplo,
--    ghci> juego [0,1,0,1,0] 2
--    
--    1: 
--    2: * 
--    3: 
--    4: * 
--    5: 
--    
--    J 2
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: 
--    
--    J 1
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

juego :: Tablero -> Int -> IO ()
juego tablero jugador = do
    if finalizadoE tablero then do
        putStrLn "VICTORIA"
    else do
        escribeTablero tablero
        putStrLn "ELIGE UNA FILA: "
        fila <- getNumero
        putStrLn "ELIGE CUANTAS ESTRELLAS RETIRAS: "
        n <- getNumero
        if valida tablero fila n then do
            let nuevoTablero = jugada tablero fila n
            let nuevoJugador = siguiente jugador
            juego nuevoTablero nuevoJugador
        else do
            putStrLn "JUGADA INCORRECTA"
            juego tablero jugador

-- Función auxiliar para obtener el nº leido por teclado
-- porque las funciones de leeDigito son una mierda.
getNumero :: IO Int
getNumero = do
    numero <- getLine
    let n = read numero :: Int
    return n

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la acción
--    nim :: IO ()
-- consistente en una partida del nim. Por ejemplo, se puede desarrollar
-- en una consola (no en la shell de emacs) como sigue
--    ghci> nim
--    
--    1: * * * * * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: * * * * 
--    3: * * * 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 3
--    Elige cuantas estrellas retiras: 3
--    
--    1: * 
--    2: * * * * 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 1
--    Elige una fila: 2
--    Elige cuantas estrellas retiras: 4
--    
--    1: * 
--    2: 
--    3: 
--    4: * * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: * 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 1
--    Elige una fila: 1
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: * 
--    5: * 
--    
--    J 2
--    Elige una fila: 4
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: * 
--    
--    J 1
--    Elige una fila: 5
--    Elige cuantas estrellas retiras: 1
--    
--    1: 
--    2: 
--    3: 
--    4: 
--    5: 
--    
--    J 1 He ganado
-- ---------------------------------------------------------------------

-- Solo consiste en llamar a juego con los parámetros ya especificados
nim :: IO ()
nim = juego inicial 1

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función principal para poder compilar el
-- el fichero. Compila el fichero y genera un ejecutable.
-- ---------------------------------------------------------------------

main :: IO()
main = nim

-- ---------------------------------------------------------------------
-- Ejercicio 13. Implementa un menú inicial que permita o bien comenzar
-- un juego nuevo, o bien cargar una partida desde un fichero (nombre indicado 
-- por el usuario). El formato del fichero debe ser:
--   jugador
--   n1 n2 n3 n4 n5
-- Es decir, la primera línea indicará el jugador que tenía el turno,
-- especificado por su número (1 o 2). La siguiente línea indica, con
-- separación de comas, el número de estrellas en cada fila.
-- ---------------------------------------------------------------------

-- Fin