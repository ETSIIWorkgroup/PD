-- PD-Practica 6.1
-- Funciones de orden superior y definiciones por plegados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Redefinir por recursión la función
--    takeWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (takeWhile p xs) es la lista de los elemento de xs hasta el
-- primero que no cumple la propiedad p. Por ejemplo,
--    takeWhile' (<7) [2,3,9,4,5]  ==  [2,3]
-- ---------------------------------------------------------------------
 
-- Esta primera versión es errónea porque devuelve una lista con todos
-- los elementos que cumplen el predicado, NO para en el primero que no cumple.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = takeWhile' p xs
 
-- Esta versión SI para al primer elemento que NO cumple el predicado.
-- En lugar de llamar recursivamente a la función cuando no se cumple
-- el predicado, devolvemos una lista vacía como caso base.
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' _ [] = []
takeWhile'' p (x:xs)
    | p x = x : takeWhile'' p xs
    | otherwise = []
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Redefinir por recursión la función
--    dropWhile :: (a -> Bool) -> [a] -> [a]
-- tal que (dropWhile p xs) es la lista de eliminando los elemento de xs
-- hasta el primero que no cumple la propiedad p. Por ejemplo,
--    dropWhile' (<7) [2,3,9,4,5]  ==  [9,4,5]
-- ---------------------------------------------------------------------
 
-- Muy parecido a takeWhile pero cambia el caso base
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x : xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Redefinir, usando foldr, la función concat. Por ejemplo, 
--    concat' [[1,3],[2,4,6],[1,9]]  ==  [1,3,2,4,6,1,9]
-- ---------------------------------------------------------------------
 
-- 'foldr' (fold right) toma:
    -- 1. Una función
    -- 2. Un valor inicial (también conocido como acumulador) 
    -- 3. Una lista como argumentos 
-- Lo que hace es aplicar la función a los elementos de la lista desde la derecha hacia la izquierda,
-- acumulando un resultado a medida que avanza.

-- concat' funciona porque aplica (++) a cada elemento que se le pasa por parámetro 
-- (recorre la entrada) y acumula la salida sobre [], dando como resultado la lista.
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que la funciones concat',
-- y concat son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_concat :: [[Int]] -> Bool
prop_concat xss = concat' xss == concat xss

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que la longitud de 
-- (concat' xss) es la suma de las longitudes de los elementos de xss.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_longConcat :: [[Int]] -> Bool
prop_longConcat xss = length (concat' xss) == sum [length xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función segmentos con su signatura
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,9,6,4,5,7,2]  ==  [[2,0,4],[6,4],[2]]
--    segmentos odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[9],[5,7]]
-- ---------------------------------------------------------------------

segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p (x:xs)
    | p x = (takeWhile p (x:xs)) : (segmentos p (dropWhile p xs))
    | otherwise = segmentos p xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. La función 
--    divideMedia :: [Double] -> ([Double],[Double])
-- dada una lista numérica, xs, calcula el par (ys,zs), donde ys 
-- contiene los elementos de xs estrictamente menores que la media, 
-- mientras que zs contiene los elementos de xs estrictamente mayores 
-- que la media. Por ejemplo, 
--    divideMedia [6,7,2,8,6,3,4] ==  ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])
--    divideMedia [1,2,3]         ==  ([1.0],[3.0])
-- Definir la función divideMedia por filtrado y por recursión. 
-- ---------------------------------------------------------------------
 
-- La definición por filtrado es
divideMediaF :: [Double] -> ([Double],[Double])
divideMediaF xs = (min, max)
    where min = filter (< media xs) xs
          max = filter (> media xs) xs

-- La definición por recursión es
divideMediaR :: [Double] -> ([Double],[Double])
divideMediaR [] = ([],[])
divideMediaR (x:xs) = (menores m xs, mayores m xs)
    where m = media xs

menores :: Double -> [Double] -> [Double]
menores _ [] = []
menores m (x:xs)
    | x < m = x : menores m xs
    | otherwise = menores m xs

mayores :: Double -> [Double] -> [Double]
mayores _ [] = []
mayores m (x:xs)
    | x > m = x : mayores m xs
    | otherwise = mayores m xs 
 
media :: [Double] -> Double
media xs = (sum [x | x <- xs]) / fromIntegral(length xs)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    agrupa :: Eq a => [[a]] -> [[a]]
-- tal que (agrupa xss) es la lista de las listas obtenidas agrupando
-- los primeros elementos, los segundos, ... Por
-- ejemplo, 
--    agrupa [[1..6],[7..9],[10..20]]  ==  [[1,7,10],[2,8,11],[3,9,12]]
--    agrupa []                        ==  []
-- ---------------------------------------------------------------------
 
agrupa :: Eq a => [[a]] -> [[a]]
agrupa = undefined
 
-- ---------------------------------------------------------------------
-- Ejercicio 7. Se considera la función 
--    filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo,
--    filtraAplica (4+) (<3) [1..7]  =>  [5,6]
-- Se pide, definir la función
-- 1. por comprensión,
-- 2. usando map y filter,
-- 3. por recursión y
-- 4. por plegado (con foldr).
-- ---------------------------------------------------------------------
 
-- La definición con lista de comprensión es
filtraAplica_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_1 f p xs = [f x | x <- xs, p x]

-- La definición con map y filter es
filtraAplica_2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_2 f p xs = map f (filter p xs)
-- Ya que map mapea cada elemento de una lista a una funcion f y filter
-- filtra cualquier lista xs con un predicado p.

-- La definición por recursión es
filtraAplica_3 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_3 _ _ [] = []
filtraAplica_3 f p (x:xs)
    | p x = f x : filtraAplica_3 f p xs
    | otherwise = filtraAplica_3 f p xs
 
-- La definición por plegado es
filtraAplica_4 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplica_4 f p = foldr (\x res -> if p x then f x : res else res) []
-- La lista de entrada no se especifica al usar foldr!! 
-- Lo importante es especificar la función que se va a 
-- aplicar a cada elemento de lo que se pasa por parámetro.
-- \x res -> es una función lambda!!

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir, usando recursión, plegado, y acumulador, la 
-- función
--    inversa :: [a] -> [a]
-- tal que (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

inversaR :: [a] -> [a]
inversaR [] = []
inversaR (x:xs) = (inversaR xs) ++ [x]

inversaP :: [a] -> [a]
inversaP = foldr (\x res -> res ++ [x]) []

-- Usar un acumulador no es recursión; es usar un where y declarar
-- una variable condicionada.
inversaAC :: [a] -> [a]
inversaAC xs = aux [] xs
    where aux ys [] = ys
          aux ys (x:xs) = aux (x:ys) xs

-- ---------------------------------------------------------------------
-- Ejercicio 9. La función de plegado foldl está definida por
--    foldl :: (a -> b -> a) -> a -> [b] -> a
--    foldl f ys xs = aux ys xs
--        where aux ys []     = ys
--              aux ys (x:xs) = aux (f ys x) xs
-- Definir, mediante plegado con foldl, la función
--    inversaP' :: [a] -> [a]
-- tal que (inversaP' xs) es la inversa de la lista xs. Por ejemplo,
--    inversaP' [3,5,2,4,7]  ==  [7,4,2,5,3]
-- ---------------------------------------------------------------------

-- foldl es como foldr pero en lugar de operar hacia la derecha (right) 
-- lo hace hacia la izquierda (left). Pero el funcionamiento es el 
-- mismo: foldl (operacion) [acumulador]
inversaP' :: [a] -> [a]
inversaP' = foldl (\res x -> [x] ++ res) []

-- Esta misma función con foldr era: foldr (\x res -> res ++ [x]) []

-- ---------------------------------------------------------------------
-- Ejercicio 10. Redefinir, por recursión y plegado la función map. 
-- ---------------------------------------------------------------------

mapR :: (a -> b) -> [a] -> [b]
mapR _ []  = []
mapR f (x:xs) = f x : mapR f xs

mapP :: (a -> b) -> [a] -> [b]
mapP f = foldr (\x res -> [f x] ++ res) []

-- ---------------------------------------------------------------------
-- Ejercicio 11. Redefinir, usando foldl y foldr la función filter. Por
-- ejemplo, 
--    filter (<4) [1,7,3,2]  =>  [1,3,2]
-- ---------------------------------------------------------------------

filterL :: (a -> Bool) -> [a] -> [a]
filterL f = foldl (\res x -> if f x then [x] ++ res else res) []

filterR :: (a -> Bool) -> [a] -> [a]
filterR f = foldr (\x res -> if f x then [x] ++ res else res) []

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, mediante recursión, plegado, acumulador, y 
-- plegado con foldl la función
--    sumll :: Num a => [[a]] -> a
-- tal que (sumll xss) es la suma de las sumas de las listas de xss. 
-- Por ejemplo, 
--    sumll [[1,3],[2,5]]  ==  11
-- ---------------------------------------------------------------------

sumllR :: Num a => [[a]] -> a
sumllR [] = 0
sumllR (xs:xss) = sum xs + sumllR xss

-- El "acumulador" del plegado no necesariamente tiene que ser una lista
-- []; en este caso queremos un nº asi que ponemos 0 porque arranca ahí.
sumllP :: Num a => [[a]] -> a
sumllP = foldr (\xs xss -> sum xs + xss) 0

sumllA :: Num a => [[a]] -> a
sumllA xs = aux 0 xs
  where aux acc [] = acc
        aux acc (xs:xss) = aux (acc + sum xs) xss

-- Esto es foldl  (que lo llama acumulador plegado)
sumllAP :: Num a => [[a]] -> a
sumllAP = foldl (\xss xs -> xss + sum xs) 0

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir, mediante recursión y plegado, la función
--    borra :: Eq a => a -> a -> [a]
-- tal que (borra y xs) es la lista obtenida borrando las ocurrencias de
-- y en xs. Por ejemplo, 
--    borra 5 [2,3,5,6]    ==  [2,3,6]
--    borra 5 [2,3,5,6,5]  ==  [2,3,6]
--    borra 7 [2,3,5,6,5]  ==  [2,3,5,6,5]
-- ---------------------------------------------------------------------

borraR :: Eq a => a -> [a] -> [a]
borraR _ [] = []
borraR y (x:xs)
    | x == y = borraR y xs
    | otherwise = x : borraR y xs

borraP :: Eq a => a -> [a] -> [a]
borraP y = foldr (\x res -> if x == y then res else [x] ++ res) []

-- Adicional: por foldl
borraPA :: Eq a => a -> [a] -> [a]
borraPA y = foldl (\res x -> if x == y then res else [x] ++ res) []

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, mediante recursión y plegado la función
--    diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia del conjunto xs e ys; es
-- decir el conjunto de los elementos de xs que no pertenecen a ys. Por
-- ejemplo,  
--    diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
-- ---------------------------------------------------------------------

diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR _ [] = []
diferenciaR [] _ = []
diferenciaR (x:xs) (y:ys)
    | elem x ys = diferenciaR xs ys
    | otherwise = x : diferenciaR xs ys

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP ys = foldr (\x res -> if elem x ys then res else x:res) []

-- -------------------------------------------------------------------
-- Ejercicio 15. Definir mediante plegado la función 
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo, 
--    producto [2,1,-3,4,5,-6] == 720
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> a
producto = foldr (*) 1

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir mediante plegado la función 
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo, 
--    productoPred even [2,1,-3,4,-5,6] == 48
-- ---------------------------------------------------------------------

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred p = foldr (\x res -> if p x then x*res else res) 1

-- Adicional: por foldl
productoPredAP :: Num a => (a -> Bool) -> [a] -> a
productoPredAP p = foldl (\res x -> if p x then x*res else res) 1

-- ---------------------------------------------------------------------
-- Ejercicio 17.1. Definir, mediante recursión, la función
--    maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumR [3,7,2,5]                  ==  7
--    maximumR ["todo","es","falso"]      ==  "todo"
--    maximumR ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumR es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

-- Esta notación funciona porque x representa el primer elemento, pero
-- y:ys representa el resto de elementos quitando el x y sigue
-- siendo recursivo.
maximumR :: Ord a => [a] -> a
maximumR [x]      = x
maximumR (x:y:ys) = max x (maximumR (y:ys))
maximumR _        = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 17.2. La función de plegado foldr1 está definida por 
--    foldr1 :: (a -> a -> a) -> [a] -> a
--    foldr1 _ [x]    =  x
--    foldr1 f (x:xs) =  f x (foldr1 f xs)
-- 
-- Definir, mediante plegado con foldr1, la función
--    maximumP :: Ord a => [a] -> a
-- tal que (maximumR xs) es el máximo de la lista xs. Por ejemplo,
--    maximumP [3,7,2,5]                  ==  7
--    maximumP ["todo","es","falso"]      ==  "todo"
--    maximumP ["menos","alguna","cosa"]  ==  "menos"
-- 
-- Nota: La función maximumP es equivalente a la predefinida maximum.
-- ---------------------------------------------------------------------

-- foldr1 viene predefinida en la librería de Data.Foldable. Si solo
-- recibe un elemento, lo devuelve, si recibe una lista de ellos, 
-- aplica recursión: aplica el predicado f a cada elemento de la lista.

maximumP :: Ord a => [a] -> a
maximumP = foldr1 max

-- Esto funciona porque foldr1 hace max sobre cada elemento de la lista
-- frente al resto de elementos => f x (foldr1 f xs) => max x (foldr1 max xs)

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir, por plegado, la función
-- sumaDivP :: Int -> [Int] -> Int
-- tal que (SumaDivP x xs) es la suma de los cuadrados de los
-- elementos de xs que son divisibles por x. Por ejemplo,
-- sumaDivP 3 [1..7] == 45
-- sumaDivP 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivP :: Int -> [Int] -> Int
sumaDivP m = foldr (\x res -> if (x `mod` m == 0) then (x^2)+res else res) 0

-- ---------------------------------------------------------------------
-- Ejercicio 19.1. Definir, con la función all, la función
--    relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosA r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosA (<) [2,3,7,9]                ==  True
--    relacionadosA (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

-- zip junta todos los elementos por pares, luego la variable pred aplica
-- el predicado p a cada par (x,y) y all verifica si todos los 'pred'
-- son True o no.
relacionadosA :: (a -> a -> Bool) -> [a] -> Bool
relacionadosA p xs = all pred (zip xs (tail xs))
  where pred (x,y) = p x y

-- ---------------------------------------------------------------------
-- Ejercicio 19.2. Definir, con la función foldr, la función
--    relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
-- tal que (relacionadosP r xs) se verifica si para todo par (x,y) de
-- elementos consecutivos de xs se cumple la relación r. Por ejemplo,
--    relacionadosP (<) [2,3,7,9]                ==  True
--    relacionadosP (<) [2,3,1,9]                ==  False
-- ---------------------------------------------------------------------

relacionadosP :: (a -> a -> Bool) -> [a] -> Bool
relacionadosP = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19.3.
-- Una lista se dirá muy creciente si cada elemento es mayor estricto
-- que el triple del siguiente. 
-- Empleando tan solo (relacionadosA p xs), define el predicado 
--          muyCreciente :: [Integer] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es muy creciente. Por
-- ejemplo:
-- muyCreciente [1,5,23,115]  == True
-- muyCreciente [1,5,23,115]    == False
-- muyCreciente [7]           == True
-- muyCreciente []            == True
-- ---------------------------------------------------------------------

muyCreciente :: [Integer] -> Bool
muyCreciente [x] = True
muyCreciente [] = True
muyCreciente xs = relacionadosA pred xs
  where pred x y = x < y*3

-- Fin