-- PD-Practica 3.2
-- Definiciones por comprensión con cadenas: El cifrado César.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta práctica es trabajar con la codificación de 
-- cadenas, caracteres y comprensión en Haskell implementando el cifrado
-- César. Se puede usar el ejemplo en
--    http://www.cs.us.es/~jalonso/cursos/i1m/temas/tema-5.pdf
-- donde se usó el cifrado cesar con solo minúsculas. Así, por ejemplo,  
--    ghci> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.Char
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    minuscula2int :: Char -> Int
-- tal que (minuscula2int c) es el entero correspondiente a la letra
-- minúscula c. Por ejemplo, 
--    minuscula2int 'a'  ==  0
--    minuscula2int 'd'  ==  3
--    minuscula2int 'z'  ==  25
-- ---------------------------------------------------------------------

minuscula2int :: Char -> Int
minuscula2int a
    | elem a abecedario = head [posicion | (letra,posicion) <- mapa, letra == a]
    | otherwise = -1

abecedario = ['a'..'z']
posiciones = [0..25]

mapa = zip abecedario posiciones

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mayuscula2int :: Char -> Int
-- tal que (mayuscula2int c) es el entero correspondiente a la letra
-- mayúscula c. Por ejemplo, 
--    mayuscula2int 'A'  ==  0
--    mayuscula2int 'D'  ==  3
--    mayuscula2int 'Z'  ==  25
-- ---------------------------------------------------------------------

mayuscula2int :: Char -> Int
mayuscula2int a
    | elem a abecedarioM = head [posicion | (letra,posicion) <- mapaM, letra == a]
    | otherwise = -1

abecedarioM = ['A'..'Z']
posicionesM = [0..25]

mapaM = zip abecedarioM posicionesM

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    int2minuscula :: Int -> Char
-- tal que (int2minuscula n) es la letra minúscula correspondiente al
-- entero n. Por ejemplo, 
--    int2minuscula 0   ==  'a'
--    int2minuscula 3   ==  'd'
--    int2minuscula 25  ==  'z'
-- ---------------------------------------------------------------------

int2minuscula :: Int -> Char
int2minuscula n
    | n >= 0 && n <= 25 = abecedario !! n
    | otherwise = ' '

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    int2mayuscula :: Int -> Char
-- tal que (int2mayuscula n) es la letra minúscula correspondiente al
-- entero n. Por ejemplo, 
--    int2mayuscula 0   ==  'A'
--    int2mayuscula 3   ==  'D'
--    int2mayuscula 25  ==  'Z'
-- ---------------------------------------------------------------------

int2mayuscula :: Int -> Char
int2mayuscula n
    | n >= 0 && n <= 25 = abecedarioM !! n
    | otherwise = ' '

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    desplaza :: Int -> Char -> Char
-- tal que (desplaza n c) es el carácter obtenido desplazando n
-- caracteres el carácter c. Por ejemplo, 
--    desplaza   3  'a'  ==  'd'
--    desplaza   3  'y'  ==  'b'
--    desplaza (-3) 'd'  ==  'a'
--    desplaza (-3) 'b'  ==  'y'
--    desplaza   3  'A'  ==  'D'
--    desplaza   3  'Y'  ==  'B'
--    desplaza (-3) 'D'  ==  'A'
--    desplaza (-3) 'B'  ==  'Y'
-- ---------------------------------------------------------------------

-- Versión poco eficiente, solo vale para minúsculas y no comprueba si
-- nos pasamos del índice
desplaza :: Int -> Char -> Char
desplaza n c = int2minuscula (p + abs(n))
    where p = minuscula2int c 

-- Versión completa, pero no hace desplazamiento inverso
desplazaC :: Int -> Char -> Char
desplazaC n c
    | esMinus c && (minuscula2int c + n) < 26 = int2minuscula ((minuscula2int c) + n)
    | esMayus c && (mayuscula2int c + n) < 26 = int2mayuscula ((mayuscula2int c) + n)
    | otherwise = error "No se puede desplazar"

-- Versión completa más limpio pero tampoco hace desplazamiento inverso
desplazaCA :: Int -> Char -> Char
desplazaCA n c
    | esMinus c && (minusMasN) < 26 = int2minuscula (minusMasN)
    | esMayus c && (mayusMasN) < 26 = int2mayuscula (mayusMasN)
    | otherwise = error "No se puede desplazar"
        where minusMasN = minuscula2int c + abs(n)
              mayusMasN = mayuscula2int c + abs(n)

-- Funciones auxiliares para ver si una letras es minúscula/mayúscula
esMayus c = elem c abecedarioM
esMinus c = elem c abecedario

-- Versión completa que hace desplazamiento en ambos sentidos, además
-- se comprueba el caso de recibir un caracter vacío, devolviendo lo mismo.
desplazaBidireccional :: Int -> Char -> Char
desplazaBidireccional n c
    | esMinus c && (minusMasN) < 26 && (minusMasN) >= 0 = int2minuscula (minusMasN)
    | esMayus c && (mayusMasN) < 26 && (mayusMasN) >= 0 = int2mayuscula (mayusMasN)
    | c == ' ' = ' '
    | otherwise = error "No se puede desplazar"
        where minusMasN = if (esPositivo n) then minuscula2int c + n else minuscula2int c - abs(n)
              mayusMasN = if (esPositivo n) then mayuscula2int c + n else mayuscula2int c - abs(n)

-- Funciones auxuliares para ver si un nº es positivo o negativo (me vale con uno)
esPositivo x = x >= 0

-- Nueva versión: bidireccional pero si llega al límite de caracteres continúa desde el principio:
desplazaBidireccionalN :: Int -> Char -> Char
desplazaBidireccionalN n c
    | esMinus c = int2minuscula (minusMasN)
    | esMayus c = int2mayuscula (mayusMasN)
    | c == ' ' = ' '
        where minusMasN = if (esPositivo n) then ((minuscula2int c + n) `mod` 26) else ((minuscula2int c - abs(n)) `mod` 26)
              mayusMasN = if (esPositivo n) then ((mayuscula2int c + n) `mod` 26) else ((mayuscula2int c - abs(n)) `mod` 26)

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    codifica :: Int -> String -> String
-- tal que (codifica n xs) es el resultado de codificar el texto xs con
-- un desplazamiento n. Por ejemplo, 
--    ghci> codifica   3  "En Todo La Medida" 
--    "Hq Wrgr Od Phglgd"
--    ghci> codifica (-3) "Hq Wrgr Od Phglgd"
--    "En Todo La Medida"
-- ---------------------------------------------------------------------

codifica :: Int -> String -> String
codifica n str = [desplazaBidireccionalN n letra | letra <- str]

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck que para cualquier entero n y
-- cualquier cadena cs se tiene que (codifica (-n) (codifica n cs)) es
-- igual a cs.
-- ---------------------------------------------------------------------

-- La propiedad es:
prop_codifica :: Int -> String -> Bool
prop_codifica n cs = codifica n (codifica n cs) == cs

-- ---------------------------------------------------------------------
-- Ejercicio 7 (resuelto). Definir la función
--    tabla :: [Float]
-- tal que tabla es la lista de la frecuencias de las letras en
-- castellano, Por ejemplo, la frecuencia de la 'a' es del 12.53%, la de
-- la 'b' es 1.42%. 
-- ---------------------------------------------------------------------

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    porcentaje :: Int -> Int -> Float
-- tal que (porcentaje n m) es el porcentaje de n sobre m. Por ejemplo,
--    porcentaje 2 5  ==  40.0  
-- ---------------------------------------------------------------------

porcentaje :: Int -> Int -> Float
porcentaje n m = fromIntegral(n) / fromIntegral(m) * 100

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    letras :: String -> String
-- tal que (letras xs) es la cadena formada por las letras de la cadena
-- xs. Por ejemplo,  
--    letras "Esto Es Una Prueba"  ==  "EstoEsUnaPrueba"
-- ---------------------------------------------------------------------

letras :: String -> String
letras xs = [x | x <- xs, x /= ' ']

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la función
--    ocurrencias :: Eq a => a -> [a] -> Int
-- tal que (ocurrencias x xs) es el número de veces que ocurre el
-- elemento x en la lista xs. Por ejemplo, 
--    ocurrencias 'a' "Salamanca"  ==  4  
-- ---------------------------------------------------------------------

ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias a xs = sum [1 | x <- xs, x == a]

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en una lista xs es igual que en su inversa.
-- ---------------------------------------------------------------------

-- La propiedad es 
prop_ocurrencia_inv :: Int -> [Int] -> Bool
prop_ocurrencia_inv x xs = ocurrencias x xs == ocurrencias x ys
    where ys = reverse xs

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en la concatenación de las listas xs e ys es igual a
-- la suma del número de ocurrencias de x en xs y en ys.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ocurrencia_conc :: Int -> [Int] -> [Int] -> Bool
prop_ocurrencia_conc x xs ys = ocurrencias x xys == (ocurrencias x xs + ocurrencias x ys)
    where xys = xs ++ ys

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    frecuencias :: String -> [Float]
-- tal que (frecuencias xs) es la frecuencia de cada una de las letras
-- del abecedario en la cadena xs. Por ejemplo, 
--    ghci> frecuencias "En Todo La Medida"
--    [14.3,0,0,21.4,14.3,0,0,0,7.1,0,0,7.1,
--     7.1,7.1,14.3,0,0,0,0,7.1,0,0,0,0,0,0]
-- ---------------------------------------------------------------------

frecuencias :: String -> [Float]
frecuencias xs = [frecuenciaEnLaTabla x | x <- xs]

-- Mapeo cada frecuencia a su letra en el abecedario:
tabla_mapeada = zip tabla abecedario

-- Esta primera versión asume que solo llegan caracteres en minúsculas
frecuenciaEnLaTabla :: Char -> Float
frecuenciaEnLaTabla n 
    | elem n abecedario = head [f | (f,l) <- tabla_mapeada, n == l]
    | otherwise = 0.0

-- Versión más completa donde los caracteres en mayúsculas también se cuentan:
tabla_mapeada_M = zip tabla abecedarioM

frecuenciaEnLaTablaM :: Char -> Float
frecuenciaEnLaTablaM n
    | esMinus n && elem n abecedario   = head [f | (f,l) <- tabla_mapeada, n == l]
    | esMayus n && elem n abecedarioM  = head [f | (f,l) <- tabla_mapeada_M, n == l]
    | otherwise = 0.0

frecuenciasCompleto :: String -> [Float]
frecuenciasCompleto xs = [frecuenciaEnLaTablaM x | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 13.1. Definir la función
--    chiCuad :: [Float] -> [Float] -> Float
-- tal que (chiCuad os es) es la medida chi cuadrado de las
-- distribuciones os y es. Por ejemplo, 
--    chiCuad [3,5,6] [3,5,6]  ==  0.0
--    chiCuad [3,5,6] [5,6,3]  ==  3.9666667
-- ---------------------------------------------------------------------

chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- ---------------------------------------------------------------------
-- Ejercicio 13.2, Comprobar con QuickCheck que para cualquier par de
-- listas xs e ys se verifica que (chiCuad xs ys) es 0 y xs e ys son
-- iguales. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_1 :: [Float] -> [Float] -> Bool
prop_chiCuad_1 xs ys = (chiCuad xs ys == 0) == (xs == ys)

-- Esto está pensado para fallar intencionadamente, dado que xs == ys 
-- puede verificarse con longitudes de listas distintas.

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista de los contraejemplos del apartado
-- anterior, qué condición hay que añadir para que se verifique la
-- propiedad.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_2 :: [Float] -> [Float] -> Property
prop_chiCuad_2 xs ys = length xs == length ys ==> (chiCuad xs ys == 0) == (xs == ys)

-- parámetros = condicion ==> implementación es una estructura permitida en Haskell
-- Sin embargo esto pasa cierto nº de pruebas de quickCheck pero no todas!!

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista del apartado anterior, el número de tests
-- que ha pasado puede ser menor que 100. Reescribir la propiedad de
-- forma que se verifique en los 100 tests.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_3 :: [Float] -> [Float] -> Bool
prop_chiCuad_3 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14.1. Definir la función
--    rota :: Int -> [a] -> [a]
-- tal que (rota n xs) es la lista obtenida rotando n posiciones los
-- elementos de la lista xs. Por ejemplo, 
--    rota  2 "manolo"              ==  "noloma"  
--    rota 10 "manolo"              ==  "lomano"
--    [rota n "abc" | n <- [0..5]]  ==  ["abc","bca","cab","abc","bca","cab"]
-- ---------------------------------------------------------------------

-- Es necesario usar el módulo para que el parámetro x no se pase de 
-- la distancia de la palabra:
rota :: Int -> [a] -> [a]
rota x xs
    | x == 0 = xs
    | length xs == 0 = xs
    | otherwise = drop n xs ++ take n xs
        where n = x `mod` length xs

-- [manolo] => rota 2 "manolo"
-- drop 2 [manolo] = [nolo]
--                          => [noloma]
-- take 2 [manolo] = [ma]

-- ---------------------------------------------------------------------
-- Ejercicio 14.2. Comprobar con QuickCkeck si para cualquier lista xs
-- si se rota n veces y el resultado se rota m veces se obtiene lo mismo
-- que rotando xs (n+m) veces, donde n y m son números no nulos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_rota :: Int -> Int -> [Int] -> Property
prop_rota n m xs = n > 0 && m > 0 ==> rota m (s) == rota (m+n) xs
    where s = rota n xs

-- rota m (rota n xs) == rota m+n xs

-- ---------------------------------------------------------------------
-- Ejercicio 15.1. Definir la función
--    descifra :: String -> String
-- tal que (descifra xs) es la cadena obtenida descodificando la cadena
-- xs por el anti-desplazamiento que produce una distribución de letras
-- con la menor deviación chi cuadrado respecto de la tabla de
-- distribución de las letras en castellano. Por ejemplo, 
--    ghci> codifica 5 "Todo Para Nada"
--    "Ytit Ufwf Sfif"
--    ghci> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"
-- ---------------------------------------------------------------------

descifra :: String -> String
descifra xs =  codifica (-factor) xs
    where factor = head (ubica (minimum tabChi) tabChi)
          tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
          tabla' = frecuencias xs

ubica :: Eq a => a -> [a] -> [Int]
ubica x xs = [i | (x',i) <- zip xs [0..], x == x']

-- Fin