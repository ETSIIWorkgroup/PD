-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

{-
    * Para jugar => Ctrl + Shift + I (load ghci) => main + Enter
    
    * Si ghci no carga la ruta por defecto:

        - Load GHCi
        - Escribir: :l App.Main + Enter
        - Escribir: main + Enter
-}

module App.Main where

-- Librerias importadas
import Source.Control

-- Función de inicio
main :: IO ()
main = iniciaSeleccion

--