-- PD - 2021/22
-- Correspondiente a Relación 21 de I1M 2010-20
-- El TAD de las pilas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las pilas.

-- Para realizar los ejercicios hay descargar las implementaciones
-- de las pilas:
-- + PilaTA.hs que está en https://www.cs.us.es/cursos/pd-2021/ejercicios/PilaTA.hs
-- + PilasL.hs que está en https://www.cs.us.es/cursos/pd-2021/ejercicios/PilaL.hs

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- Hay que elegir una implementación del TAD pilas.
-- import PilaTA
-- import PilaL
import PilaTA

-- ---------------------------------------------------------------------
-- A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de pilas:
-- ---------------------------------------------------------------------

ejP1, ejP2, ejP3, ejP4, ejP5 :: Pila Int
ejP1 = foldr apila vacia [1..20]
ejP2 = foldr apila vacia [2,5..18]
ejP3 = foldr apila vacia [3..10]
ejP4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
ejP5 = foldr apila vacia [1..5]

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p pila) es la pila con los elementos de pila
-- que verifican el predicado p, en el mismo orden. Por ejemplo,
--    ghci> ejP1
--    1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|-
--    ghci> filtraPila even ejP1
--    2|4|6|8|10|12|14|16|18|20|-

-- ---------------------------------------------------------------------

-- 'vacia', 'apila', 'desapila' y 'cima' vienen del módulo PilaTA!!
filtraPila :: (a -> Bool) -> Pila a -> Pila a
filtraPila f pila
    -- Si es vacia = devuelve vacia.
    | esVacia pila = vacia
    -- Si la cima de la pila cumple f = apila la cima de pila sobre el resultado
    -- de seguir filtrando la pila (el resto de la pila desapilando el primer elemento).
    | f (cima pila) = apila (cima pila) (filtraPila f (desapila pila))
    -- En otro caso = desapila el elemento y sigue filtrando.
    | otherwise = filtraPila f (desapila pila)

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f pila) es la pila formada con las imágenes por f de
-- los elementos de pila, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) ejP1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------

mapPila :: (a -> a) -> Pila a -> Pila a
mapPila f pila
    | esVacia pila = vacia
    | otherwise = apila (f (cima pila)) (mapPila f (desapila pila)) -- Sintaxis: apila (elemento) (resto pila)

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    pertenecePila :: (Eq a) => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y sólo si y es un elemento
-- de la pila p. Por ejemplo,
--    pertenecePila 7 ejP1  == True
--    pertenecePila 70 ejP1 == False
-- ---------------------------------------------------------------------

pertenecePila :: (Eq a) => a -> Pila a -> Bool
pertenecePila n pila
    | esVacia pila = False
    | otherwise = ((cima pila) == n) || (pertenecePila n (desapila pila))

-- ---------------------------------------------------------------------
-- Ejercicio 4: definir la función
--    contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si y sólo si todos los
-- elementos de p1 son elementos de p2. Por ejemplo,
--    contenidaPila ejP2 ejP1 == True
--    contenidaPila ejP1 ejP2 == False
-- ---------------------------------------------------------------------

contenidaPila :: (Eq a) => Pila a -> Pila a -> Bool
contenidaPila pilaA pilaB
    | esVacia pilaA = True
    -- De forma inversa al ejercicio anterior, comprobamos si cada elemento de una
    -- de las dos pilas pertenece a la otra pila con 'pertenecePila'.
    -- En cada iteración se vuelve a llamar a contenidaPila quitando el elemento
    -- comprobado de la primera pila. Cuando no quedan elementos en la primera
    -- pila, es verdad.
    | otherwise = (pertenecePila (cima pilaA) pilaB) && (contenidaPila (desapila pilaA) pilaB)

-- ---------------------------------------------------------------------
-- Ejercicio 4: Defiir la función
--    prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila ejP3 ejP2 == False
--    prefijoPila ejP5 ejP1 == True
-- ---------------------------------------------------------------------

-- Que sea prefijo: 123 es prefijo de 1234567
prefijoPila :: (Eq a) => Pila a -> Pila a -> Bool
prefijoPila pilaA pilaB 
    | esVacia pilaA = True  -- Si la pilaA se acaba: es verdad
    | esVacia pilaB = False -- Si la pilaB se acaba: pilaA no puede ser prefijo de esta
    | otherwise = ((cima pilaA) == (cima pilaB)) && (prefijoPila (desapila pilaA) (desapila pilaB))

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    subPila :: (Eq a) => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2.
-- Por ejemplo, 
--    subPila ejP2 ejP1 == False
--    subPila ejP3 ejP1 == True
-- ---------------------------------------------------------------------

-- Parecido al ejercicio anterior pero usando 'pertenecePila' 
-- Esta implementacion no es correcta!!
subPila :: (Eq a) => Pila a -> Pila a -> Bool
subPila pilaA pilaB
    | esVacia pilaA = True  -- Si pilaA se queda sin elementos, es subpila de pilaB
    | esVacia pilaB = False -- Si pilaB se queda sin elementos, pilaA no puede ser subpila de esta
    | otherwise = (pertenecePila (cima pilaA) pilaB) && (subPila (desapila pilaA) pilaB)

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    ordenadaPila :: (Ord a) => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila ejP1 == True
--    ordenadaPila ejP4 == False
-- ---------------------------------------------------------------------

ordenadaPila :: (Ord a) => Pila a -> Bool
ordenadaPila pila
    | esVacia pila = True            -- Cuando la pila se acaba es verdad
    | esVacia (desapila pila) = True -- Obligatorio para no hacer 'cima' de una pila vacia (último elemento)
    | otherwise = ((cima pila) <= (cima (desapila pila))) && (ordenadaPila (desapila pila))

-- ---------------------------------------------------------------------
-- Ejercicio 7.1: Definir una función
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es una pila formada por los elementos de xs.
-- Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-
-- ---------------------------------------------------------------------

lista2Pila :: [a] -> Pila a
lista2Pila = foldr apila vacia 
-- Sintaxis foldr: foldr funcion acumulador
-- Luego a cada elemento de la lista se le aplica la funcion apila y su resultado
-- se almacena sobre una pila vacia. 

-- ---------------------------------------------------------------------
-- Ejercicio 7.2: Definir una función
--  pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de p.
-- Por ejemplo,
--    pila2Lista ejP2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------

pila2Lista :: Pila a -> [a]
pila2Lista pila
    | esVacia pila = []
    | otherwise = (cima pila) : (pila2Lista (desapila pila))
    -- Unir la cima de la pila como lista al resto de la pila desapilando ese elemento

-- ---------------------------------------------------------------------
-- Ejercicio 7.3: Comprobar con QuickCheck que la función pila2Lista es
-- la inversa de  lista2Pila, y recíprocamente.
-- ---------------------------------------------------------------------

prop_pila2Lista :: Pila Int -> Bool
prop_pila2Lista p = (lista2Pila (pila2Lista p)) == p

-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.

prop_lista2Pila :: [Int] -> Bool
prop_lista2Pila xs = (pila2Lista (lista2Pila xs)) == xs

-- ghci> quickCheck prop_lista2Pila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.1: Definir la función 
--    ordenaInserPila :: (Ord a) => Pila a -> Pila a
-- tal que (ordenaInserPila p) es una pila con los elementos de la pila
-- p, ordenados por inserción. Por ejemplo,
--    ghci> ordenaInserPila ejP4
--    -1|0|3|3|3|4|4|7|8|10|-
-- ---------------------------------------------------------------------

ordenaInserPila :: (Ord a) => Pila a -> Pila a
ordenaInserPila = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 9.2: Comprobar con QuickCheck que la pila 
---    (ordenaInserPila p) 
-- está ordenada correctamente.

prop_ordenaInserPila :: Pila Int -> Bool
prop_ordenaInserPila p = undefined

-- ghci> quickCheck prop_ordenaInserPila
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.1: Definir la función
--    nubPila :: (Eq a) => Pila a -> Pila a
-- tal que (nubPila p) es una pila con los elementos de p sin
-- repeticiones. Por ejemplo,
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> nubPila ejP4
--    -1|7|8|10|0|3|4|-
-- ---------------------------------------------------------------------

nubPila :: (Eq a) => Pila a -> Pila a
nubPila pila
    | esVacia pila = vacia                                                -- Si es vacia, devuelve vacia
    | pertenecePila (cima pila) (desapila pila) = nubPila (desapila pila) -- Si el elemento pertenece al resto, es que esta duplicado
    | otherwise = apila (cima pila) (nubPila (desapila pila))             -- De lo contrario, se apila 

-- ---------------------------------------------------------------------
-- Ejercicio 10.2: Definir la propiedad siguiente: "las composición de
-- las funciones nub y pila2Lista coincide con la composición de las
-- funciones pila2Lista y nubPila", y comprobarla con quickCheck.
-- En caso de ser falsa, redefinir la función nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nubPila :: Pila Int -> Bool
prop_nubPila p = nub(pila2Lista p) == pila2Lista(nubPila p)

-- La comprobación es
-- Hay que arreglar esto!!

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función 
--    maxPila :: (Ord a) => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> ejP4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila ejP4
--    10
-- ---------------------------------------------------------------------

maxPila :: (Ord a) => Pila a -> a
maxPila pila
    | esVacia pila = error "La pila esta vacia"
    | esVacia (desapila pila) = (cima pila)
    | otherwise = max (cima pila) (maxPila (desapila pila))
        
-- Esto está mal
-- if ((cima pila) > (cima (desapila pila))) then (apila (cima pila) (maxPila (desapila pila))) else maxPila (desapila pila)

-- ---------------------------------------------------------------------
-- Generador de pilas                                                 --
-- ---------------------------------------------------------------------

-- genPila es un generador de pilas. Por ejemplo,
--    ghci> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do xs <- listOf arbitrary
             return (foldr apila vacia xs)
         


-- El tipo pila es una instancia del arbitrario. 
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
    arbitrary = genPila

-- Fin