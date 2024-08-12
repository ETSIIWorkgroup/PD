-- PD- El TAD de las colas.
-- Correspondiente a Relación 22 de I1M 2019-20
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las colas, utilizando las implementaciones estudiadas en el 
-- tema 15 transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m/temas/tema-15.html
-- 
-- Para realizar los ejercicios hay que instalar la librería I1M que
-- contiene la implementación de TAD de las pilas. Los pasos para
-- instalarla son los siguientes:
-- + cabal update
-- + cabal install I1M
-- 
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las colas:
-- + ColaConListas.hs    que está en http://bit.ly/21z3wQL
-- + ColaConDosListas.hs que está en http://bit.ly/21z3AQp

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD colas:
import ColaConListas
-- import ColaConDosListas
-- import I1M.Cola
    
-- ---------------------------------------------------------------------
-- Nota. A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de colas:
c1, c2, c3, c4, c5, c6 :: Cola Int
c1 = foldr inserta vacia [1..20]
c2 = foldr inserta vacia [2,5..18]
c3 = foldr inserta vacia [3..10]
c4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]
c5 = foldr inserta vacia [15..20]
c6 = foldr inserta vacia (reverse [1..20])
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el último elemento de la cola c. Por
-- ejemplo:
--    ultimoCola c4 == 4
--    ultimoCola c5 == 15
-- ---------------------------------------------------------------------

ultimoCola :: Cola a -> a
ultimoCola cola
    | esVacia (resto cola) = (primero cola)
    | otherwise = ultimoCola (resto cola)

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    longitudCola :: Cola a -> Int
-- tal que (longitudCola c) es el número de elementos de la cola c. Por
-- ejemplo, 
--     longitudCola c2 == 6
-- ---------------------------------------------------------------------

longitudCola :: Cola a -> Int
longitudCola cola
    | esVacia cola = 0
    | otherwise = 1 + (longitudCola (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función 
--    todosVerifican :: (a -> Bool) -> Cola a -> Bool
-- tal que (todosVerifican p c) se verifica si todos los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    todosVerifican (>0) c1 == True
--    todosVerifican (>0) c4 == False
-- ---------------------------------------------------------------------

todosVerifican :: (a -> Bool) -> Cola a -> Bool
todosVerifican f cola 
    | esVacia cola = True 
    | otherwise = (f (primero cola)) && (todosVerifican f (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    algunoVerifica :: (a -> Bool) -> Cola a -> Bool
-- tal que (algunoVerifica p c) se verifica si algún elemento de la cola
-- c cumple la propiedad p. Por ejemplo,
--   algunoVerifica (<0) c1 == False
--   algunoVerifica (<0) c4 == True
-- ---------------------------------------------------------------------

algunoVerifica :: (a -> Bool) -> Cola a -> Bool
algunoVerifica f cola
    | esVacia cola = False 
    | otherwise = (f (primero cola)) || (algunoVerifica f (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    ponAlaCola :: Cola a -> Cola a -> Cola a
-- tal que (ponAlaCola c1 c2) es la cola que resulta de poner los
-- elementos de c2 a la cola de c1. Por ejemplo,
--    ponAlaCola c2 c3 == C [17,14,11,8,5,2,10,9,8,7,6,5,4,3]
-- ---------------------------------------------------------------------

ponAlaCola :: Cola a -> Cola a -> Cola a
ponAlaCola colaA colaB
    | esVacia colaA = colaB
    | esVacia colaB = colaA
    | otherwise = ponAlaCola (inserta (primero colaB) colaA) (resto colaB)

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    mezclaColas :: Cola a -> Cola a -> Cola a
-- tal que (mezclaColas c1 c2) es la cola formada por los elementos de
-- c1 y c2 colocados en una cola, de forma alternativa, empezando por
-- los elementos de c1. Por ejemplo,
--    mezclaColas c2 c4 == C [17,4,14,3,11,3,8,0,5,10,2,8,3,7,-1,4]
-- ---------------------------------------------------------------------

mezclaColas :: Cola a -> Cola a -> Cola a
mezclaColas colaA colaB = mezclaColaAux colaA colaB vacia

-- Usando el método anterior, ponemos en la cola de aux (inicialmente vacío), los
-- elementos de colaA y colaB si su contrario es vacío. Si no son vacias, se llama
-- recursivamente con el resto de sus colas y aux toma el valor de insertar el primer
-- valor de colaB al primer valor de colaA (empieza por colaA).
mezclaColaAux :: Cola a -> Cola a -> Cola a -> Cola a
mezclaColaAux colaA colaB aux
    | esVacia colaA = ponAlaCola aux colaB -- Si esta vacio, pon B en aux
    | esVacia colaB = ponAlaCola aux colaA -- Si esta vacio, pon A en aux
    -- (inserta (primero colaB) (inserta (primero colaA) aux)
    -- Insertar, en la cola formada por insertar el primero de A en aux, el primero de B
    | otherwise = mezclaColaAux (resto colaA) (resto colaB) (inserta (primero colaB) (inserta (primero colaA) aux))

-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir la función
--    agrupaColas :: [Cola a] -> Cola a
-- tal que (agrupaColas [c1,c2,c3,...,cn]) es la cola formada mezclando
-- las colas de la lista como sigue: mezcla c1 con c2, el resultado con
-- c3, el resultado con c4, y así sucesivamente. Por ejemplo,
--    ghci> agrupaColas [c3,c3,c4]
--    C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]
-- ---------------------------------------------------------------------

agrupaColas :: [Cola a] -> Cola a
agrupaColas []     = vacia
agrupaColas [cola] = cola
agrupaColas (colaA:colaB:colas) = agrupaColas ((mezclaColas colaA colaB) : colas)

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir la función
--    perteneceCola :: Eq a => a -> Cola a -> Bool
-- tal que (perteneceCola x c) se verifica si x es un elemento de la
-- cola c. Por ejemplo, 
--    perteneceCola 7 c1  == True
--    perteneceCola 70 c1 == False
-- ---------------------------------------------------------------------

perteneceCola :: Eq a => a -> Cola a -> Bool
perteneceCola x cola
    | esVacia cola = False
    | otherwise = ((primero cola) == x) || (perteneceCola x (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--    contenidaCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (contenidaCola c1 c2) se verifica si todos los elementos de
-- c1 son elementos de c2. Por ejemplo, 
--    contenidaCola c2 c1 == True
--    contenidaCola c1 c2 == False
-- ---------------------------------------------------------------------

contenidaCola :: Eq a => Cola a -> Cola a -> Bool
contenidaCola colaA colaB
    | esVacia colaA = True  -- Si colaA es vacia, vacio pertenece a cualquier cola
    | esVacia colaB = False -- Si colaB es vacia, es imposible que colaA pertenezca a esta
    | otherwise = (perteneceCola (primero colaA) colaB) && (contenidaCola (resto colaA) colaB)

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la función
--    prefijoCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (prefijoCola c1 c2) se verifica si la cola c1 es un prefijo
-- de la cola c2. Por ejemplo, 
--    prefijoCola c3 c2 == False
--    prefijoCola c5 c1 == True
-- ---------------------------------------------------------------------

prefijoCola :: Eq a => Cola a -> Cola a -> Bool
prefijoCola colaA colaB
    | esVacia colaA = True  -- Si colaA es vacio, vacio siempre es prefijo de cualquier cosa
    | esVacia colaB = False -- Si colaB es vacio, colaA no puede ser prefijo de nada
    -- No está detallado pero para que sea prefijo el primer elemento debe coincidir
    -- hasta donde alcance la cola que se está comparando (colaA).
    | otherwise = ((primero colaA) == (primero colaB)) && (prefijoCola (resto colaA) (resto colaB))

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--    subCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (subCola c1 c2) se verifica si c1 es una subcola de c2. Por
-- ejemplo,  
--    subCola c2 c1 == False
--    subCola c3 c1 == True
-- ---------------------------------------------------------------------

subCola :: Eq a => Cola a -> Cola a -> Bool
subCola colaA colaB
    | esVacia colaA = True
    | esVacia colaB = False
    | otherwise = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12: Definir la función
--    ordenadaCola :: Ord a => Cola a -> Bool
-- tal que (ordenadaCola c) se verifica si los elementos de la cola c
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaCola c6 == True
--    ordenadaCola c4 == False
-- ---------------------------------------------------------------------

ordenadaCola :: Ord a => Cola a -> Bool
ordenadaCola cola
    | esVacia cola         = True -- Siempre que haya que comprobar un elemento y el siguiente,
    | esVacia (resto cola) = True -- hay que detallar que ocurre cuando el resto es vacio.
    -- Comprueba si el primero es mayor que el primero del resto (la cola sin el primero)
    | otherwise = ((primero cola) < (primero (resto cola))) && (ordenadaCola (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 13.1: Definir una función
--    lista2Cola :: [a] -> Cola a
-- tal que (lista2Cola xs) es una cola formada por los elementos de xs.
-- Por ejemplo,
--    lista2Cola [1..6] == C [1,2,3,4,5,6]
-- ---------------------------------------------------------------------

lista2Cola :: [a] -> Cola a
lista2Cola [] = vacia
lista2Cola xs = lista2ColaAux xs vacia

lista2ColaAux :: [a] -> Cola a -> Cola a
lista2ColaAux [] aux = aux
lista2ColaAux (x:xs) aux = lista2ColaAux xs (inserta x aux)

-- ---------------------------------------------------------------------
-- Ejercicio 13.2: Definir una función
--    cola2Lista :: Cola a -> [a]
-- tal que (cola2Lista c) es la lista formada por los elementos de p.
-- Por ejemplo,
--    cola2Lista c2 == [17,14,11,8,5,2]
-- ---------------------------------------------------------------------

-- Implementación excesivamente compleja: y el orden es invertido
cola2Lista :: Cola a -> [a]
cola2Lista cola = cola2ListaAux cola []

cola2ListaAux :: Cola a -> [a] -> [a]
cola2ListaAux cola aux
    | esVacia cola = aux
    | otherwise = cola2ListaAux (resto cola) ((primero cola) : aux)

-- Más sencillo: y el orden se respeta
cola2Lista2 :: Cola a -> [a]
cola2Lista2 cola
    | esVacia cola = []
    | otherwise = (primero cola) : (cola2Lista2 (resto cola))

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck que la función cola2Lista es
-- la inversa de  lista2Cola, y recíprocamente.
-- ---------------------------------------------------------------------

prop_cola2Lista :: Cola Int -> Bool
prop_cola2Lista cola = (lista2Cola (cola2Lista2 cola)) == cola

-- ghci> quickCheck prop_cola2Lista
-- +++ OK, passed 100 tests.

prop_lista2Cola :: [Int] -> Bool
prop_lista2Cola xs = (cola2Lista2 (lista2Cola xs)) == xs

-- ghci> quickCheck prop_lista2Cola
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14: Definir la función 
--    maxCola :: Ord a => Cola a -> a
-- tal que (maxCola c) es el mayor de los elementos de la cola c. Por
-- ejemplo, 
--    maxCola c4 == 10
-- ---------------------------------------------------------------------

maxCola :: Ord a => Cola a -> a
maxCola cola
    | esVacia cola         = error "La cola es vacia"       -- Ó es vacia
    | esVacia (resto cola) = primero cola                   -- Ó solo queda un elemento
    | otherwise = max (primero cola) (maxCola (resto cola)) -- O se compara el primer elemento frente al resto recursivamente

-- ---------------------------------------------------------------------
-- Generador de colas                                          --
-- ---------------------------------------------------------------------

-- genCola es un generador de colas de enteros. Por ejemplo,
--    ghci> sample genCola
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([7,8,4,3,7],[5,3,3])
--    C ([],[])
--    C ([1],[13])
--    C ([18,28],[12,21,28,28,3,18,14])
--    C ([47],[64,45,7])
--    C ([8],[])
--    C ([42,112,178,175,107],[])
genCola :: (Num a, Arbitrary a) => Gen (Cola a)
genCola = frequency [(1, return vacia),
                     (30, do n <- choose (10,100)
                             xs <- vectorOf n arbitrary
                             return (creaCola xs))]
          where creaCola = foldr inserta vacia

-- El tipo cola es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
    arbitrary = genCola

-- Fin