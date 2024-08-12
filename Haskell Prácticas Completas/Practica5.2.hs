-- PD 2019-20: Practica 6.2
-- Funciones de orden superior y definiciones por plegados (II)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================
import Data.Char
import Data.List
import Data.Numbers.Primes
-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la función
--      resultadoPos :: (a -> Integer) -> [a] -> [a]
-- tal que (resultadoPos f xs) es la lista de los elementos de la lista
-- xs tales que el valor de la función f sobre ellos es positivo. Por ejemplo,
--   resultadoPos head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--   resultadoPos sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...),
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- -----------------------------------------------------------------------------

resultadoPosC :: (a -> Integer) -> [a] -> [a]
resultadoPosC p xss = [xs | xs <- xss, p xs > 0]

resultadoPosO :: (a -> Integer) -> [a] -> [a]
resultadoPosO p xss = filter (\xs -> p xs > 0) xss

resultadoPosR :: (a -> Integer) -> [a] -> [a]
resultadoPosR _ [] = []
resultadoPosR p (xs:xss)
    | p xs > 0 = xs : resultadoPosR p xss
    | otherwise = resultadoPosR p xss

resultadoPosP :: (a -> Integer) -> [a] -> [a]
resultadoPosP p = foldr (\xs xss -> if p xs > 0 then xs : xss else xss) []

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la función
--     intercala :: Int -> [Int] -> [Int]
-- tal que (intercala y xs) es la lista que resulta de intercalar el elemento
-- y delante de todos los elementos de la lista xs que sean menores que y.
-- Por ejemplo,
--   intercala 5 [1,2,6,3,7,9]  ==  [5,1,5,2,6,5,3,7,9]
--   intercala 5 [6,7,9,8]      ==  [6,7,9,8]
--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- Esta versión está mal, no está intercalando y con x porque no se puede hacer y++x
intercalaC :: Int -> [Int] -> [Int]
intercalaC y xs = [if x > y then x else y | x <- xs]

intercalaO :: Int -> [Int] -> [Int]
intercalaO y xs = concat (map (\x -> if x > y then [x] else [y,x]) xs) 

intercalaR :: Int -> [Int] -> [Int]
intercalaR _ [] = []
intercalaR y (x:xs)
    | x > y = x : intercalaR y xs
    | otherwise = y : x : intercalaR y xs

intercalaP :: Int -> [Int] -> [Int]
intercalaP y = foldr (\x res -> if x > y then x:res else y:x:res) []

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--    dec2ent :: [Integer] -> Integer
-- tal que (dec2ent xs) es el número entero cuyas cifras ordenadas son los
-- elementos de la lista xs. Por ejemplo,
--   dec2ent [2,3,4,5]  ==  2345
--   dec2ent [1..9]     ==  123456789
--
-- Defie esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- Esto funciona porque suma cada nº multiplicado por 1, 10, 100, 1000, etc al
-- elevar 10 a y donde y va desde 0 (10^0 = 1) hasta donde llegue la lista xs.
dec2entC :: [Integer] -> Integer
dec2entC xs = sum [x * (10^y) | (x,y) <- zip (reverse xs) [0,1..]]

-- Más fácil: read "lee" caracteres de teclado, luego se filtran si son nºs.
dec2entO :: [Integer] -> Integer
dec2entO xs = read (filter isDigit (show xs))

dec2entR :: [Integer] -> Integer
dec2entR [] = 0
dec2entR (x:xs) = x * (10^length xs) + dec2entR xs

dec2entP :: [Integer] -> Integer
dec2entP = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la función
--     diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, el conjunto de los elementos de la lista xs que no se
-- encuentran en la lista ys. Por ejemplo,
--   diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
--   diferencia [1,3,5,7] [2,4,6]  ==  [1,3,5,7]
--   diferencia [1,3] [1..9]       ==  []
--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

diferenciaC :: Eq a => [a] -> [a] -> [a]
diferenciaC xs ys = [x | x <- xs, notElem x ys]

diferenciaO :: Eq a => [a] -> [a] -> [a]
diferenciaO xs ys = filter (\x -> notElem x ys) xs

-- Realmente esta versión no es correcta por que el caso base no está bien definido:
diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR _ [] = []
diferenciaR [] _ = []
diferenciaR (x:xs) (y:ys)
    | notElem x ys = x : diferenciaR xs ys
    | otherwise = diferenciaR xs ys

-- Esta versión si es correcta, tiene en cuenta que estamos comparando elementos 
-- de xs en ys; luego si ys se queda sin elementos (ys = []) lo que queda de xs
-- (si queda algo), debería ser el resultado, no una lista vacía.
diferenciaR2 :: Eq a => [a] -> [a] -> [a]
diferenciaR2 [] _ = []
diferenciaR2 xs [] = xs
diferenciaR2 (x:xs) ys
    | elem x ys = diferenciaR2 xs ys
    | otherwise = x : diferenciaR2 xs ys

-- No olvidar que el primer parámetro es xs que no se especifica en el foldr pero 
-- el segundo parámetro (la segunda lista, ys) SI debe ir como parámetro!!
diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP ys = foldr (\x res -> if elem x ys then res else x:res) []

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la función
--   primerosYultimos :: [[a]] -> ([a],[a])
-- tal que (primerosYultimos xss) es el par formado por la lista de los
-- primeros elementos de las listas no vacías de xss y la lista de los
-- últimos elementos de las listas no vacías de xss. Por ejemplo,
--   primerosYultimos [[1,2],[5,3,4],[],[9]]  ==  ([1,5,9],[2,4,9])
--   primerosYultimos [[1,2],[1,2,3],[1..4]]  ==  ([1,1,1],[2,3,4])

--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

primerosYultimosC :: [[a]] -> ([a],[a])
primerosYultimosC xss = unzip [(head xs, last xs) | xs <- xss, length xs > 0]

primerosYultimosO :: [[a]] -> ([a],[a])
primerosYultimosO xss = undefined

-- Esta implementación no es correcta.
-- primerosYultimosR :: [[a]] -> ([a],[a])
-- primerosYultimosR [] = ([],[])
-- primerosYultimosR (xs:xss)
--     | length xs < 1 = []
--     | otherwise = (head xs, last xs) : primerosYultimosR xss

primerosYultimosP :: (Eq a) => [[a]] -> ([a],[a])
primerosYultimosP xss = unzip (foldr (\xs res -> (head xs, last xs) : res) [] (filter (/= []) xss))

-- ----------------------------------------------------------------------------
-- Ejercicio 6. Una lista hermanada es una lista de números estrictamente
-- positivos en la que cada elemento tiene algún factor primo en común con el
-- siguiente, en caso de que exista, o alguno de los dos es un 1. Por ejemplo,
-- [2,6,3,9,1,5] es una lista hermanada.

-- Se considera la función
--    hermanada :: [Int] -> Bool
-- tal que (hermanada xs) comprueba que la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanada [2,6,3,9,1,5]  ==  True
--    hermanada [2,3,5]        ==  False
--
-- Se pide definir esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------
-- Nota: Usa la función 'gcd'
-- ----------------------------------------------------------------------------

hermanadaC :: [Int] -> Bool
hermanadaC xs = and [esPrimo (gcd x y) || x == 1 || y == 1 | (x,y) <- zip xs (tail xs)]

-- Funciones auxiliares:
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n]

-- El uso de all: all (condicion) (parámetros)
hermanadaO :: [Int] -> Bool
hermanadaO xs = all (\(x,y) -> esPrimo (gcd x y) || x == 1 || y == 1) (zip xs (tail xs))

hermanadaR :: [Int] -> Bool
hermanadaR (_:[]) = True
hermanadaR (x:y:xs) = (esPrimo (gcd x y) || x == 1 || y == 1) && hermanadaR (y:xs)
-- Esto es: se cumple para (x,y) Y se cumple para el siguiente (elemento:resto elementos)

hermanadaP :: [Int] -> Bool
hermanadaP xs = undefined

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Un elemento de una lista es permanente si ninguno de los que
-- vienen a continuación en la lista es mayor que él. Consideramos la función
--   permanentes :: [Int] -> [Int]
-- tal que (permanentes xs) es la lista de los elementos permanentes de la
-- lista xs. Por ejemplo,
--   permanentes [80,1,7,8,4]  ==  [80,8,4]

-- Se pide definir esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ---------------------------------------------------------------------------
-- Nota: Usa la función 'tails' de Data.List.
-- ----------------------------------------------------------------------------

-- Esta implementación no es correcta, solo tomará el primer nº mayor que el resto
permanentesC :: [Int] -> [Int]
permanentesC xs = [x | x <- xs, mayorQueResto x (tail xs)]

mayorQueResto :: Int -> [Int] -> Bool
mayorQueResto n xs = and [n > x | x <- xs]

permanentesO :: [Int] -> [Int]
permanentesO xs = undefined

-- Esta primera versión no es completamente correcta, si solo queda un 
-- elemento, este no se comparará.
permanentesR :: [Int] -> [Int]
permanentesR (_:[]) = []
permanentesR (x:y:xs)
    | x > y = x : permanentesR (y:xs)
    | otherwise = permanentesR (y:xs)

-- Esta versión cubre esa falla, cuando solo queda un elemento, este
-- siempre es permanente porque es mayor que 'vacío'.
permanentesR' :: [Int] -> [Int] 
permanentesR' (x:[]) = [x]
permanentesR' (x:y:xs)
    | x > y = x : permanentesR' (y:xs)
    | otherwise = permanentesR' (y:xs)

permanentesP :: [Int] -> [Int] 
permanentesP xs = undefined
              
-- ---------------------------------------------------------------------
-- Ejercicio 8. Un número entero positivo n es muy primo si es n primo
-- y todos los números que resultan de ir suprimimiendo la última cifra
-- también son primos. Por ejemplo, 7193 es muy primo pues los números
-- 7193, 719, 71 y 7 son todos primos. 
-- 
-- Define la función 
--    muyPrimo :: Integer -> Bool
-- que (muyPrimo n) se verifica si n es muy primo. Por ejemplo,
--    muyPrimo 7193  == True
--    muyPrimo 71932 == False
-- --------------------------------------------------------------------

-- Ya hay definido arriba una función 'esPrimo'.
-- Al hacerlo recursivo no hay que preocuparse por cuantos '0s' hay que 
-- dividir n, porque cada llamada es un nº reducido.
muyPrimo :: Int -> Bool
muyPrimo 0 = True
muyPrimo n = (esPrimo n) && (muyPrimo (div n 10))

-- ---------------------------------------------------------------------
-- ¿Cuántos números de cinco cifras son muy primos?
-- ---------------------------------------------------------------------

-- El resultado es: 15 (tarda un rato)
n5cifrasMuyPrimos = sum [1 | x <- [10000..99999], muyPrimo x]

-- Fin