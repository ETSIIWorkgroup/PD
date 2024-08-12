-- PD-Practica 1.1
-- Definiciones de funciones, tipos y clases.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

import Data.Char
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Define una función que determina si un elemento está
-- presente en una lista.
--
-- *Main> contiene "Hola mundo" 'b'
-- False
-- *Main> contiene "Hola mundo" 'a'
-- True
-- *Main> contiene "Hola mundo" 'm'
-- True
-- *Main> contiene [] 'm'
-- False
-- ---------------------------------------------------------------------

contiene :: (Eq a) => [a] -> a -> Bool
contiene xs x
    | elem x xs = True
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 2. Define una función que devuelva el índice de un elemento
-- dentro de una lista.
--
-- *Main> indice 'b' "Hola mundo"
-- -1
-- *Main> indice 'a' "Hola mundo"
-- 3
-- *Main> indice 'u' "Hola mundo"
-- 6
-- ---------------------------------------------------------------------

indice :: (Eq a) => a -> [a] -> Maybe Int
indice x xs = elemIndex x xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Elimina el n-ésimo elemento de una lista
-- ---------------------------------------------------------------------

eliminaN :: (Eq a) => [a] -> Int -> [a]
eliminaN xs x
    | x < 0 = xs
    | x > length xs = xs
    | otherwise = (take x xs) ++ (drop m xs)
        where m = x + 1

-- [A B C D E] = xs
--  0 1 2 3 4  =>    take 3 xs = [A B C] 
--                      ++
--                   drop 4 xs = [E]
--                       =
--                    [A B C E]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Define funciones para determinar si un carácter es una 
-- letra mayúscula, es mínuscula o es un número.
--
-- *Main> esMayus 'A'
-- True
-- *Main> esMayus 'z'
-- False
-- *Main> esMinus 'A'
-- False
-- *Main> esMinus 'z'
-- True
-- ---------------------------------------------------------------------

esMayus :: Char -> Bool
esMayus x
    | isAlpha x == False = False
    | otherwise = if (isUpper x) then True else False

esMinus :: Char -> Bool
esMinus x
    | isAlpha x == False = False
    | otherwise = not (esMayus x)
    
esNum :: Char -> Bool
esNum x = isDigit x

-- ---------------------------------------------------------------------
-- Ejercicio 4. Define una función para determinar si un carácter es una 
-- letra mayúscula o es mínuscula o es un número
-- ---------------------------------------------------------------------

esLetraNum :: Char -> String
esLetraNum x
    | esNum x == True = "Numero"
    | otherwise = if (esMayus x) then "Mayuscula" else "Minuscula"

-- ---------------------------------------------------------------------
-- Ejercicio 5. Define una función que determine si hay n elementos 
-- consecutivos de una lista que sumen un valor dado.
--
-- sumaConsecutiva 3 5 [1,2,2,4,5,6,7]
-- True
-- ---------------------------------------------------------------------

-- Usamos 'any' (una función de orden superior) para tomar:
-- 1. Un predicado
-- 2. Una lista sobre la que aplicar el predicado
-- , de esta forma para cada lista generada por 'ventanas', se le aplica la 
-- función lambda declarada como: \sublista -> sum sublista == suma
-- El \ nos indica que es una lambda que solo compruba si la suma de esa sublista
-- coindice con el parámetro 'suma'. 
sumaConsecutiva :: Int -> Int -> [Int] -> Bool
sumaConsecutiva n suma xs
    | length xs < n = False 
    | otherwise = any (\sublista -> sum sublista == suma) (ventanas n xs)

-- Montamos una función auxiliar RECURSIVA que mientras el largo de la lista
-- original no llegue a 0, devuelve:
-- take n xs => toma los n primeros nº de xs
ventanas :: Int -> [a] -> [[a]]
ventanas n xs
    | length xs < n = []
    | otherwise = take n xs : ventanas n (tail xs)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Convierte una frase en capital case:
--
-- capital "hola mundo"
-- "Hola Mundo"
-- capital ""
-- ""
-- ---------------------------------------------------------------------

capital x = toUpper x

-- Fin