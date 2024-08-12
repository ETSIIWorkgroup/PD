-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para controlar la ejecución del juego.

module Source.Control (
    iniciaSeleccion
) where

-- Librerias importadas
import Data.Array
import Source.Tipos
import Source.JuegoLibre
import Source.JuegoAvanzado

{-
    Función que muestre por pantalla la selección del tipo de juego 
    y lea por teclado la selección del jugador; si la selección no es 
    correcta se vuelve a pedir al usuario.
    
    Usa case of
-}
iniciaSeleccion :: IO()
iniciaSeleccion = do
    putStrLn "\n MODO DE JUEGO "
    putStrLn "\t 1 LIBRE "
    putStrLn "\t 2 GUIADO "
    seleccion <- getLine
    case seleccion of "1" -> iniciaJuegoLibre
                      "2" -> iniciaJuegoAvanzado
                      _ -> iniciaSeleccion                      

--