-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de los tipos usados para el juego.

module Source.Tipos (
    Posicion(..),
    Sudoku(..),
    ContadorAvanzado(..),
    ContadorSimple(..)
) where

-- Librerias importadas
import Data.Array

-- Tipo 'Posicion' que representa una posición del tablero de un Sudoku 
-- especificando su fila y columna.
type Posicion = (Int, Int)

-- Tipo 'Sudoku' que representa un array bidimensional de caracteres.
type Sudoku = Array (Int, Int) Char

{-
    Tipo de registro específico del modo de juego avanzado que representa un contador de
    jugadas; usando 'data' y la sintaxis de registro de datos de la asignatura.
-}
data ContadorAvanzado = 
    ContA {
        nombreA :: String, fallos :: Int, puntuacion :: Int
    }
    deriving (Show,Eq)

{-
    Tipo de registro específico del modo de juego simple que representa un contador de
    veces que se ha usado una pista o se ha consultado las opciones posibles; 
    usando 'data' y la sintaxis de registro de datos de la asignatura.
-}
data ContadorSimple = 
    ContS {
        nombreS :: String, 
        jugadasS :: Int,
        pistas :: Int,
        opciones :: Int
    }
    deriving (Show,Eq)

--