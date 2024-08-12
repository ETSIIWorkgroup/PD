-- PD-Práctica 3.1
-- Definiciones por comprensión
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones de
-- funciones por comprensión. Estos ejercicios se corresponden con el
-- tema 5 

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                  
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
import Data.Char
import Data.List
 
-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función
--    sumaDeCuadrados :: Integer -> Integer
-- tal que (sumaDeCuadrados n) es la suma de los cuadrados de los
-- primeros n números; es decir, 1^2 + 2^2 + ... + 100^2. Por ejemplo,
--    sumaDeCuadrados 3    ==  14
--    sumaDeCuadrados 100  ==  338350
-- ---------------------------------------------------------------------

-- La estructura de una lista de compresión es:
-- [expresión | generador+, guarda*] 
-- dónde generador = var <- lista y guarda es una condición.
sumaDeCuadrados :: Integer -> Integer
sumaDeCuadrados n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un entero positivo es perfecto si es igual a la suma de
-- sus factores, excluyendo el propio número. Usando una lista por
-- comprensión y la función factores (del tema), definir la función 
--    perfectos :: Int -> [Int]
-- tal que (perfectos n) es la lista de todos los números perfectos
-- menores que n. Por ejemplo: 
--    *Main> perfectos 500
--    [6,28,496]
-- ---------------------------------------------------------------------
 
-- Esta función no hace falta
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

sumaFactores :: Int -> Int
sumaFactores n = sum [x | x <- [1..n], (n `mod` x == 0) && (x /= n)]

esPerfecto :: Int -> Bool
esPerfecto n
    | n == sumaFactores n = True
    | otherwise = False

perfectos :: Int -> [Int]
perfectos n = [x | x <- [1..n], esPerfecto x && x /= n]
 
-- ---------------------------------------------------------------------
-- Ejercicio 3. El producto escalar de dos listas de enteros xs y ys de
-- longitud n viene dado por la suma de los productos de los elementos
-- correspondientes. Definir por comprensión la función 
--    productoEscalar :: [Int] -> [Int] -> Int
-- tal que (productoEscalar xs ys) es el producto escalar de las listas
-- xs e ys. Por ejemplo,
--    productoEscalar [1,2,3] [4,5,6]  =>  32 = 4 + 10 + 18
--
-- Usar QuickCheck para comprobar la propiedad conmutativa del producto
-- escalar.  
-- ---------------------------------------------------------------------
 
-- Está implementación es incorrecta pues hace todas las combinaciones 
-- posibles en lugar de multiplicar componente con componente.
productoEscalarMal :: [Int] -> [Int] -> Int
productoEscalarMal xs ys = sum [x * y | x <- xs, y <- ys]

-- Esta es la versión simple, usando zip xs ys que une ambas listas 
-- por pares: zip [1,2] [3,4] = [(1,3),(2,4)]
productoEscalar :: [Int] -> [Int] -> Int
productoEscalar xs ys = sum [x * y | (x,y) <- zip xs ys]

-- Aún más simple: zip pero con función como: zipWith funcion xs ys. 
-- Sin listas por compresión: zipWith multiplica xs ys y luego sumatorio de eso.
productoEscalarNoCompresion :: [Int] -> [Int] -> Int
productoEscalarNoCompresion xs ys = sum (zipWith (*) xs ys)

-- La propiedad conmutativa es
prop_conmutativa_productoEscalar xs ys = productoEscalar xs ys == productoEscalar ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 4 (Problema 1 del proyecto Euler) Definir la función
--    euler1 :: Integer -> Integer
-- (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que
-- n. Por ejemplo,
--    euler1 10  ==  23
-- 
-- Calcular la suma de todos los múltiplos de 3 ó 5 menores que 1000.
-- ---------------------------------------------------------------------

euler1 :: Integer -> Integer
euler1 n = sum [x | x <- [1..n-1], esMultiplo3o5 x]

esMultiplo3o5 :: Integer -> Bool
esMultiplo3o5 n
    | n `mod` 3 == 0 = True
    | n `mod` 5 == 0 = True
    | otherwise = False
    
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensión la función
--    replica :: Int -> a -> [a]
-- tal que (replica n x) es la lista formada por n copias del elemento
-- x. Por ejemplo,
--    *Main> replica 3 True
--    [True, True, True]
-- Se corresponde con la función replicate.
-- ---------------------------------------------------------------------
 
replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Un número natural n se denomina abundante si es menor
-- que la suma de sus divisores propios. Por ejemplo, 12 y 30 son
-- abundantes pero 5 y 28 no lo son.
-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función numeroAbundante tal que
-- (numeroAbundante n) se verifica si n es un número abundante. Por
-- ejemplo, 
--    numeroAbundante 5  == False
--    numeroAbundante 12 == True
--    numeroAbundante 28 == False
--    numeroAbundante 30 == True
-- ---------------------------------------------------------------------

-- Reutilizamos la funcion de factores pero sin incluir el propio nº
factoresNoN :: Int -> [Int]
factoresNoN n = [x | x <- [1..n], n `mod` x == 0 && n /= x]

numeroAbundante :: Int -> Bool
numeroAbundante n
    | n < sum(factoresNoN n) = True
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función numerosAbundantesMenores tal que
-- (numerosAbundantesMenores n) es la lista de números abundantes
-- menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

-- Podemos reutilizar factoresNoN
numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir la función todosPares tal que (todosPares n)
-- se verifica si todos los números abundantes menores o iguales que n
-- son pares. Por ejemplo,
--    todosPares 10    ==  True
--    todosPares 100   ==  True
--    todosPares 1000  ==  False
-- ---------------------------------------------------------------------

todosPares :: Int -> Bool
todosPares n
    | length (filter even xs) == length xs = True
    | otherwise = False
        where xs = numerosAbundantesMenores n

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. Definir la constante primerAbundanteImpar que calcule
-- el primer número natural abundante impar. Determinar el valor de
-- dicho número.
-- ---------------------------------------------------------------------

primerAbundanteImpar x = undefined

-- Su cálculo es

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función suma tal (suma n) es la suma de los
-- n primeros números. Por ejemplo,
--    suma 3  ==  6
-- ---------------------------------------------------------------------

suma :: Int -> Int
suma n = sum[x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Los triángulo aritmético se forman como sigue
--     A
--     B  C
--     D  E  F
--     G  H  I  J
--     K  L  M  N  Ñ
--    16 17 18 19 20 21
-- Definir la función linea tal que (linea n) es la línea n-ésima de los
-- triángulos aritméticos. Por ejemplo, 
--    linea 4  ==  [7,8,9,10]
--    linea 5  ==  [11,12,13,14,15]
-- ---------------------------------------------------------------------

linea :: Int -> [Int]
linea n = [n * (n + 1) `div` 2 + 1 .. n * (n + 1) `div` 2 + n]

-- Ejemplo de función recursiva
sumaLista :: Num a => [a] -> a
sumaLista []     = 0
sumaLista (x:ls) = x + sumaLista ls

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función triangulo tal que (triangulo n) es
-- el triángulo aritmético de altura n. Por ejemplo,
--    triangulo 3  ==  [[1],[2,3],[4,5,6]]
--    triangulo 4  ==  [[1],[2,3],[4,5,6],[7,8,9,10]]
-- ---------------------------------------------------------------------

triangulo :: Int -> [[Int]]
triangulo n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función circulo tal que (circulo n) es la
-- cantidad de pares de números naturales (x,y) que se encuentran dentro
-- del círculo de radio n. Por ejemplo, 
--    circulo 3  ==  9
--    circulo 4  ==  15
--    circulo 5  ==  22
-- La ecuación de los puntos que están en la circunferencia de centro (0,0)
--  y radio n es: x^2 + y^2 = n^2. Los puntos del interior cumplen la 
-- desigualdad: x^2 + y^2 < n^2 
-- ---------------------------------------------------------------------

circulo :: Int -> Int
circulo n = length[(x,y) | x <- [0..n], y <- [0..n], x^2 + y^2 < n^2]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función ocurrenciasDelMaximo tal que
-- (ocurrenciasDelMaximo xs) es el par formado por el mayor de los
-- números de xs y el número de veces que este aparece en la lista
-- xs, si la lista es no vacía y es (0,0) si xs es la lista vacía. Por
-- ejemplo,  
--    ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,1)
--    ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,3)
--    ocurrenciasDelMaximo [8,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,4)
-- ---------------------------------------------------------------------

ocurrenciasDelMaximo :: [Int] -> (Int,Int)
ocurrenciasDelMaximo xs
    | length xs == 0 = (0,0)
    | otherwise = (mayor, length [x | x <- xs, x == mayor])
        where mayor = maximum xs

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir, por comprensión, la función tienenS tal que
-- (tienenS xss) es la lista de las longitudes de las cadenas de xss que
-- contienen el caracter 's' en mayúsculas o minúsculas. Por ejemplo, 
--    tienenS ["Este","es","un","examen","de","hoy","Suerte"]  ==  [4,2,6]
--    tienenS ["Este"]                                         ==  [4]
--    tienenS []                                               ==  []
--    tienenS [" "]                                            ==  []
-- ---------------------------------------------------------------------

tienenS :: [String] -> [Int]
tienenS [] = []
tienenS [""] = []
tienenS xss = [length xs | xs <- xss, contieneS xs]

contieneS xs
    | elem 'S' xs = True
    | elem 's' xs = True
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 13. Decimos que una lista está algo ordenada si para todo
-- par de elementos consecutivos se cumple que el primero es menor o
-- igual que el doble del segundo. Definir, por comprensión, la función
-- (algoOrdenada xs) que se verifica si la lista xs está algo ordenada. 
-- Por ejemplo, 
--    algoOrdenada [1,3,2,5,3,8]  ==  True
--    algoOrdenada [3,1]          ==  False
-- ---------------------------------------------------------------------

-- El generador de la lista hace: zip [3,1] [1] = [(3,1),(1,1)] => que al
-- aplicar la condición x <= y*2 devuelve: [False (3<=2), True (1<=2)] y 
-- el operador 'and' aplicado a la lista hace False && True = False.
algoOrdenada :: [Int] -> Bool
algoOrdenada xs = and [x <= y*2 | (x,y) <- zip xs (tail xs)]

-- No es necesario usar esto.
esMenorOIgual :: (Int,Int) -> Bool
esMenorOIgual (x,y)
    | x <= y*2 = True
    | otherwise = False

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir, por comprensión, la función tripletas tal 
-- que (tripletas xs) es la listas de tripletas de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    tripletas [8,7,6,5,4] == [[8,7,6],[7,6,5],[6,5,4]]
--    tripletas "abcd"      == ["abc","bcd"]
--    tripletas [2,4,3]     == [[2,4,3]]
--    tripletas [2,4]       == []
-- ---------------------------------------------------------------------

-- Importante: NO son los n elementos consecutivos sino n=3 siempre.
tripletas :: [a] -> [[a]]
tripletas [] = [[]]
tripletas xs = [take 3 (drop i xs) | i <- [0..length xs - 3]]

-- take 3 xs dónde xs = [8,7,6,5,4] - i => [6,5,4]  => take 3 [6,5,4] = [6,5,4]

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función tresConsecutivas tal que
-- (tresConsecutivas x ys) se verifica si x ocurre tres veces seguidas
-- en la lista ys. Por ejemplo,
--    tresConsecutivas 3 [1,4,2,3,3,4,3,5,3,4,6]  ==  False
--    tresConsecutivas 'a' "abcaaadfg"            ==  True
-- ---------------------------------------------------------------------

-- Con la función auxiliar, podemos probar si todos los elementos de 
-- alguna de las sublistas está compuesto del mismo elemento x.
-- any (patrón) [lista] comprueba si alguna de esas listas coincide con el patrón
tresConsecutivas :: (Eq a) => a -> [a] -> Bool
tresConsecutivas x ys
    | ys == [] = False
    | elem x ys == False = False
    | otherwise = any ([x,x,x]==) [l | l <- trozosDeTres ys]

-- [x,x,x,y,y,y,z,z,z] => [[x,x,x],[y,y,y],[z,z,z]]
-- Esto funciona porque [0,3..n] genera listas de 3 elementos
-- desde 0 a n. take 3 toma trozos de 3 de las listas generadas después
-- de haber eliminado los 3 primeros elementos y sucesivamente.
trozosDeTres :: [a] -> [[a]]
trozosDeTres xs = [take 3 (drop x xs) | x <- [0,3..length xs]]

-- ---------------------------------------------------------------------
-- Ejercicio 16.1. Definir la función unitarios tal que (unitarios n) es
-- la lista de números [n,nn, nnn, ....]. Por ejemplo. 
--    take 7 (unitarios 3) == [3,33,333,3333,33333,333333,3333333]
--    take 3 (unitarios 1) == [1,11,111]
-- ---------------------------------------------------------------------

unitarios :: Int -> [Int]
unitarios x = undefined

-- Si la función tomase el nº de réplicas, sería tan simple como:
unitariosR :: Int -> Int -> [Int]
unitariosR x y = replicate x y
-- , hace que: unitariosR 3 4 = [4,4,4]

-- ---------------------------------------------------------------------
-- Ejercicio 16.2. Definir la función multiplosUnitarios tal que
-- (multiplosUnitarios x y n) es la lista de los n primeros múltiplos de
-- x cuyo único dígito es y. Por ejemplo,
--    multiplosUnitarios 7 1 2  == [111111,111111111111]
--    multiplosUnitarios 11 3 5 == [33,3333,333333,33333333,3333333333]
-- ---------------------------------------------------------------------

multiplosUnitarios x y n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función primosEntre tal que (primosEntre x y)
-- es la lista de los número primos entre x e y (ambos inclusive). Por
-- ejemplo, 
--    primosEntre 11 44  ==  [11,13,17,19,23,29,31,37,41,43]
-- ---------------------------------------------------------------------

-- factores devuelve los divisores del nº, para que el nº sea primo solo
-- puede ser divisible entre si mismo y 1. Osea la lista devuelta por
-- factores tiene que ser tamaño = 2. 
esPrimo :: Int -> Bool
esPrimo x
    | length (factores x) == 2 = True
    | otherwise = False

primosEntre x y = [n | n <- [x..y], esPrimo n]

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función cortas tal que (cortas xs) es la
-- lista de las palabras más cortas (es decir, de menor longitud) de la
-- lista xs. Por ejemplo,
--    ghci> cortas ["hoy", "es", "un", "buen", "dia", "de", "sol"]
--    ["es","un","de"]
-- ---------------------------------------------------------------------

cortas :: [String] -> [String]
cortas xs = [s | s <- xs, length s == masChico xs]

masChico :: [String] -> Int
masChico xs = minimum [length s | s <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 19. Un entero positivo n es libre de cuadrado si no es
-- divisible por ningún m^2 > 1. Por ejemplo, 10 es libre de cuadrado
-- (porque 10 = 2*5) y 12 no lo es (ya que es divisible por 2^2). 
-- Definir la función libresDeCuadrado tal que (libresDeCuadrado n) es
-- la lista de los primeros n números libres de cuadrado. Por ejemplo,
--    libresDeCuadrado 15  ==  [1,2,3,5,6,7,10,11,13,14,15]
-- ---------------------------------------------------------------------

libresDeCuadrado :: Int -> [Int]
libresDeCuadrado n = [x | x <- [1..n], esLibreDeCuadrado x]

-- Comprueba si un nº es libre de cuadrado al recuperar todos sus divisores
-- (excepto el mismo y 1) y probar que NO es divisible por ningún 
-- divisor ^ 2.
esLibreDeCuadrado :: Int -> Bool
esLibreDeCuadrado n
    | length [x | x <- d, n `mod` x^2 == 0] == 0 = True
    | otherwise = False
        where d = divisores n

-- Con este método nos quedamos solo con los divisores de un nº sin el 
-- mismo ni 1. Así nos quitamos una comprobación extra en esLibreDeCuadrado
divisores :: Int -> [Int]
divisores n = [x | x <- factores n, n /= x && x /= 1]

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función masOcurrentes tal que
-- (masOcurrentes xs) es la lista de los elementos de xs que ocurren el
-- máximo número de veces. Por ejemplo,
--    masOcurrentes [1,2,3,4,3,2,3,1,4] == [3,3,3]
--    masOcurrentes [1,2,3,4,5,2,3,1,4] == [1,2,3,4,2,3,1,4]
--    masOcurrentes "Salamanca"         == "aaaa"
-- ---------------------------------------------------------------------

masOcurrentes xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 21.1. Definir la función numDiv tal que (numDiv x) es el
-- número de divisores del número natural x. Por ejemplo, 
--    numDiv 11 == 2 
--    numDiv 12 == 6 
-- ---------------------------------------------------------------------

-- Reutilizamos "factores" que ya existe:
numDiv :: Int -> Int
numDiv 0 = 0
numDiv x = length(factores x)

-- ---------------------------------------------------------------------
-- Ejercicio 21.2. Definir la función entre tal que (entre a b c) es la
-- lista de los naturales entre a y b con, al menos, c divisores. Por
-- ejemplo,  
--    entre 11 16 5 == [12, 16]
-- ---------------------------------------------------------------------

entre :: Int -> Int -> Int -> [Int]
entre a b c
    | a == 0 || b == 0 = [0]
    | a == b = divisores a
    | otherwise = [x | x <- [a..b], (numDiv x) == c]

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función conPos tal que (conPos xs) es la
-- lista obtenida a partir de xs especificando las posiciones de sus
-- elementos. Por ejemplo, 
--    conPos [1,5,0,7] == [(1,0),(5,1),(0,2),(7,3)]
-- ---------------------------------------------------------------------

conPos :: [Int] -> [(Int,Int)]
conPos xs = [(x,y) | (x,y) <- zip xs [0..length xs]]

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función tal que (pares cs) es la cadena
-- formada por los caracteres en posición par de cs. Por ejemplo, 
--    pares "el cielo sobre berlin" == "e il or eln"
-- ---------------------------------------------------------------------

pares cs = [c | c <- cs, esPar (elemIndex c cs)]

-- Como elemIndex devuelve un MaybeInt, hace falta comprobar si retorna
-- un nº antes de comprobar si el nº es par:
esPar :: Maybe Int -> Bool
esPar (Just x) = even x
esPar Nothing  = False

-- ---------------------------------------------------------------------
-- Ejercicio 24.1. Una terna (x,y,z) de enteros positivos es pitagórica
-- si x^2 + y^2 = z^2. Usando una lista por comprensión, definir la
-- función 
--    pitagoricas :: Int -> [(Int,Int,Int)]
-- tal que (pitagoricas n) es la lista de todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n. Por ejemplo, 
--    pitagoricas 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-- ---------------------------------------------------------------------

pitagoricas :: Int -> [(Int,Int,Int)]
pitagoricas n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], isPitagorico x y z]
    
isPitagorico :: Int -> Int -> Int -> Bool
isPitagorico x y z = x^2 + y^2 == z^2

-- ---------------------------------------------------------------------
-- Ejercicio 24.2. Definir la función 
--    numeroDePares :: (Int,Int,Int) -> Int
-- tal que (numeroDePares t) es el número de elementos pares de la terna
-- t. Por ejemplo,
--    numeroDePares (3,5,7)  ==  0
--    numeroDePares (3,6,7)  ==  1
--    numeroDePares (3,6,4)  ==  2
--    numeroDePares (4,6,4)  ==  3
-- ---------------------------------------------------------------------

numeroDePares :: (Int,Int,Int) -> Int
numeroDePares = undefined
--numeroDePares (x,y,z) = length[n | n <- (x,y,z), even n]

-- ---------------------------------------------------------------------
-- Ejercicio 24.3. Definir la función
--    conjetura :: Int -> Bool
-- tal que (conjetura n) se verifica si todas las ternas pitagóricas
-- cuyas componentes están entre 1 y n tiene un número impar de números
-- pares. Por ejemplo,
--    conjetura 10  ==  True
-- ---------------------------------------------------------------------

conjetura :: Int -> Bool
conjetura n = undefined

-- Esto es innecesario
numeroTernasPitagoricas :: Int -> Int
numeroTernasPitagoricas n = length (pitagoricas n)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir, por comprensión, la función
--    sumaConsecutivos :: [Int] -> [Int]
-- tal que (sumaConsecutivos xs) es la suma de los pares de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaConsecutivos [3,1,5,2]  ==  [4,6,7]
--    sumaConsecutivos [3]        ==  []
-- ---------------------------------------------------------------------

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos [] = []
sumaConsecutivos xs
    | length xs <= 1 = xs
    | length xs == 2 = [sum xs]
    | otherwise = [x+y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 26. Los polinomios pueden representarse de forma dispersa o
-- densa. Por ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar
-- de forma dispersa por [6,0,-5,4,-7] y de forma densa por
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la función 
--    densa :: [Int] -> [(Int,Int)]
-- tal que, si xs es la forma dispersa de un polinomio, (densa xs) es la 
-- forma densa. 
-- representación dispersa es xs. Por ejemplo, 
--   densa [6,0,-5,4,-7]  ==  [(4,6),(2,-5),(1,4),(0,-7)]
--   densa [6,0,0,3,0,4]  ==  [(5,6),(2,3),(0,4)]
-- ---------------------------------------------------------------------

-- Dónde b es la base y e es el exponente
densa :: [Int] -> [(Int,Int)]
densa xs = reverse [(y,x) | (x,y) <- zip (reverse xs) [0..], x /= 0]

densa2 :: [Int] -> [(Int,Int)]
densa2 xs = [(encuentraPosicion b xs, b) | b <- xs, b /= 0]

-- Necesito la posición de un elemento, pero hay que comprobar si existe
-- para asegurar que elemIndex devuelva un Int y NO un Maybe Int.
encuentraPosicion :: Int -> [Int] -> Int
encuentraPosicion x xs =
  case elemIndex x xs of
    Just index -> index
    Nothing    -> -1

-- ---------------------------------------------------------------------
-- Ejercicio 27. La función 
--    pares2 :: [a] -> [b] -> [(a,b)]
-- definida por
--    pares2 xs ys = [(x,y) | x <- xs, y <- ys]
-- toma como argumento dos listas y devuelve la listas de los pares con
-- el primer elemento de la primera lista y el segundo de la
-- segunda. Por ejemplo,
--    ghci> pares2 [1..3] [4..6]
--    [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- 
-- Definir, usando dos listas por comprensión con un generador cada una,
-- la función 
--    pares2' :: [a] -> [b] -> [(a,b)]
-- tal que pares2' sea equivalente a pares2.
-- 
-- Indicación: Utilizar la función predefinida concat y encajar una
-- lista por comprensión dentro de la otra. 
-- ---------------------------------------------------------------------

-- La definición de pares es
pares2 :: [a] -> [b] -> [(a,b)]
pares2 xs ys = [(x,y) | x <- xs, y <- ys]

-- La redefinición de pares es
pares2' :: [a] -> [b] -> [(a,b)]
pares2' xs ys = concat [[(x,y) | y <- ys] | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 28. La bases de datos sobre actividades de personas pueden
-- representarse mediante listas de elementos de la forma (a,b,c,d),
-- donde a es el nombre de la persona, b su actividad, c su fecha de
-- nacimiento y d la de su fallecimiento. Un ejemplo es la siguiente que
-- usaremos a lo largo de este ejercicio,
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
            ("Velazquez","Pintura",1599,1660),
            ("Picasso","Pintura",1881,1973),
            ("Beethoven","Musica",1770,1823),
            ("Poincare","Ciencia",1854,1912),
            ("Quevedo","Literatura",1580,1654),
            ("Goya","Pintura",1746,1828),
            ("Einstein","Ciencia",1879,1955),
            ("Mozart","Musica",1756,1791),
            ("Botticelli","Pintura",1445,1510),
            ("Borromini","Arquitectura",1599,1667),
            ("Bach","Musica",1685,1750)]

-- ---------------------------------------------------------------------
-- Ejercicio 28.1. Definir la función nombres tal que (nombres bd) es
-- la lista de los nombres de las personas de la base de datos bd. Por
-- ejemplo,  
--    ghci> nombres personas
--     ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
--      "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini","Bach"]
-- ---------------------------------------------------------------------

nombres :: [(String,String,Int,Int)] -> [String]
nombres bd = [nombre | (nombre,_,_,_) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 28.2. Definir la función musicos tal que (musicos bd) es
-- la lista de los nombres de los músicos de la base de datos bd. Por
-- ejemplo,  
--    ghci> musicos personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos :: [(String,String,Int,Int)] -> [String]
musicos bd = [nombre | (nombre,profesion,_,_) <- bd, profesion == "Musica"]

-- ---------------------------------------------------------------------
-- Ejercicio 28.3. Definir la función seleccion tal que (seleccion bd m) 
-- es la lista de los nombres de las personas de la base de datos bd
-- cuya actividad es m. Por ejemplo,  
--    ghci> seleccion personas "Pintura"
--    ["Velazquez","Picasso","Goya","Botticelli"]
-- ---------------------------------------------------------------------

seleccion :: [(String,String,Int,Int)] -> String -> [String]
seleccion bd m = [nombre | (nombre,profesion,_,_) <- bd, profesion == m]

-- ---------------------------------------------------------------------
-- Ejercicio 28.4. Definir, usando el apartado anterior, la función
-- musicos' tal que (musicos' bd) es la lista de los nombres de los
-- músicos de la base de datos bd. Por ejemplo,  
--    ghci> musicos' personas
--    ["Beethoven","Mozart","Bach"]
-- ---------------------------------------------------------------------

musicos' :: [(String,String,Int,Int)] -> [String]
musicos' bd = seleccion bd "Musica"

-- ---------------------------------------------------------------------
-- Ejercicio 28.5. Definir la función vivas tal que (vivas bd a) es la
-- lista de los nombres de las personas de la base de datos bd  que
-- estaban vivas en el año a. Por ejemplo,  
--    ghci> vivas personas 1600
--    ["Cervantes","Velazquez","Quevedo","Borromini"]
-- ---------------------------------------------------------------------

vivas :: [(String,String,Int,Int)] -> Int -> [String]
vivas ps a = [nombre | (nombre,_,nacimiento,defuncion) <- ps, nacimiento <= a && a <= defuncion]

-- ---------------------------------------------------------------------
-- Ejercicio 29.1. En este ejercicio se consideran listas de ternas de
-- la forma (nombre, edad, población). 
-- 
-- Definir la función puedenVotar tal que (puedenVotar t) es la
-- lista de las personas de t que tienen edad para votar. Por ejemplo,
--    ghci> :{
--    *Main| puedenVotar [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), ("Alba", 19, "Camas"), ("Pedro",18,"Sevilla")]
--    *Main| :}
--    ["Juan","Alba","Pedro"]
-- ---------------------------------------------------------------------

puedenVotar :: [(String,Int,String)] -> [String]
puedenVotar t = [nombre | (nombre,edad,_) <- t, edad >= 18]

-- ---------------------------------------------------------------------
-- Ejercicio 29.2. Definir la función puedenVotarEn tal que (puedenVotar
-- t p) es la lista de las personas de t que pueden votar en la
-- población p. Por ejemplo, 
--    ghci> :{
--    *Main| puedenVotarEn [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), ("Alba", 19, "Camas"),("Pedro",18,"Sevilla")] "Sevilla"
--    *Main| :}
--    ["Pedro"]
-- ---------------------------------------------------------------------

puedenVotarEn :: [(String,Int,String)] -> String -> [String]
puedenVotarEn t c = [nombre | (nombre,edad,ubicacion) <- t, edad >= 18 && ubicacion == c]

-- ---------------------------------------------------------------------
-- Ejercicio 30. Dos listas xs, ys de la misma longitud son
-- perpendiculares si el producto escalar de ambas es 0, donde el
-- producto escalar de dos listas de enteros xs e ys viene
-- dado por la suma de los productos de los elementos correspondientes.
-- 
-- Definir la función perpendiculares tal que (perpendiculares xs yss)
-- es la lista de los elementos de yss que son perpendiculares a xs.
-- Por ejemplo,
--    ghci> perpendiculares [1,0,1] [[0,1,0],[2,3,1],[-1,7,1],[3,1,0]]
--    [0,1,0],[-1,7,1]]
--
-- ---------------------------------------------------------------------

perpendiculares :: [Int] -> [[Int]] -> [[Int]]
perpendiculares xs yss = [ys | ys <- yss, nuevoProductoEscalar xs ys == 0]

-- Ya hay un ejercicio previo de producto escalar
nuevoProductoEscalar :: [Int] -> [Int] -> Int
nuevoProductoEscalar xs ys = sum (zipWith (*) xs ys)

-- ---------------------------------------------------------------------
-- Ejercicio 31.1. Un número natural n
-- es especial si para todo divisor d de n, d+n/d es primo. Definir la
-- función  
--    especial :: Integer -> Bool
-- tal que (especial x) se verifica si x es especial. Por ejemplo,
--    especial 30  ==  True
--    especial 20  ==  False

especial :: Int -> Bool
especial n = and [esPrimo (div (d+n) d) | d <- factores n]

-- ---------------------------------------------------------------------
-- Ejercicio 31.2. Definir la función 
--    sumaEspeciales :: Integer -> Integer
-- tal que (sumaEspeciales n) es la suma de los números especiales
-- menores o iguales que n. Por ejemplo, 
--    sumaEspeciales 100  ==  401
-- ---------------------------------------------------------------------

sumaEspeciales :: Int -> Int
sumaEspeciales n = sum [x | x <- [0..n], x <= n && especial x]

-- ---------------------------------------------------------------------
-- Ejercicio 32.1. Un número es muy compuesto si tiene más divisores que
-- sus anteriores. Por ejemplo, 12 es muy compuesto porque tiene 6
-- divisores (1, 2, 3, 4, 6, 12) y todos los números del 1 al 11 tienen
-- menos de 6 divisores.  
-- 
-- Definir la función
--    esMuyCompuesto :: Int -> Bool
-- tal que (esMuyCompuesto x) se verifica si x es un número muy
-- compuesto. Por ejemplo,
--    esMuyCompuesto 24  ==  True
--    esMuyCompuesto 25  ==  False
-- Calcular  el menor número muy compuesto de 4 cifras.
-- ---------------------------------------------------------------------

-- 'factores' es el metodo que devuelve todos los divisores incluyendo el 1 y el mismo

esMuyCompuesto :: Int -> Bool
esMuyCompuesto x
    | and [nDivisores x > y | y <- divisoresDeLosDemas x] = True
    | otherwise = False

-- Calculo el nº de divisores de un nº incluyendo 1 y a si mismo
nDivisores :: Int -> Int
nDivisores n = length (factores n)

-- Calculo la lista del nº de divisores de los nº menores a uno dado
divisoresDeLosDemas :: Int -> [Int]
divisoresDeLosDemas n = [nDivisores x | x <- factores n, x < n]

-- Calculo los divisores de un nº sin incluir a si mismo (es decir, los anteriores a el)
divisoresSinElMismo :: Int -> [Int]
divisoresSinElMismo n = [x | x <- [1..n], n `mod` x == 0 && n /= x]

-- Calculo la lista de longitudes de una lista de listas
longitudListas :: [[Int]] -> [Int]
longitudListas xss = [length xs | xs <- xss]

-- Todo lo anterior es sobre complicado
esMuyCompuesto2 :: Int -> Bool
esMuyCompuesto2 x = and [numDiv x > numDiv i | i <- [1..x-1]]

-- Menor nº de 4 cifras muy compuesto
menorMuyCompuesto4 = head [x | x <- [1000..9999], esMuyCompuesto2 x]

-- ---------------------------------------------------------------------
-- Ejercicio 32.2. Definir la función
--    muyCompuesto :: Int -> Int
-- tal que (muyCompuesto n) es el n-ésimo número muy compuesto. Por
-- ejemplo, 
--    muyCompuesto 10  ==  180
-- ---------------------------------------------------------------------

muyCompuesto :: Int -> Int
muyCompuesto n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir la función  
--     todosIguales :: Eq a => [a] -> Bool
-- tal que (todosIguales xs) se verifica si los elementos de la 
-- lista xs son todos iguales. Por ejemplo,   
--     todosIguales [1..5]    == False
--     todosIguales [2,2,2]   == True
--     todosIguales ["a","a"] == True
-- ---------------------------------------------------------------------

todosIguales :: (Eq a) => [a] -> Bool
todosIguales xs = and [x == head xs | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 34.1. Las bases de datos de alumnos matriculados por
-- provincia y por especialidad se pueden representar como sigue 
--    matriculas :: [(String,String,Int)]
--    matriculas = [("Almeria","Matematicas",27),
--                  ("Sevilla","Informatica",325),
--                  ("Granada","Informatica",296),
--                  ("Huelva","Matematicas",41),
--                  ("Sevilla","Matematicas",122),
--                  ("Granada","Matematicas",131),
--                  ("Malaga","Informatica",314)]
-- Es decir, se indica que por ejemplo en Sevilla hay 325 alumnos
-- matriculados en Informática. 
-- 
-- Definir la función 
--    totalAlumnos :: [(String,String,Int)] -> Int
-- tal que (totalAlumnos bd) es el total de alumnos matriculados,
-- incluyendo todas las provincias y todas las especialidades, en la
-- base de datos bd. Por ejemplo,
--    totalAlumnos matriculas == 1256
-- ---------------------------------------------------------------------

matriculas :: [(String,String,Int)]
matriculas = [("Almeria","Matematicas",27),
              ("Sevilla","Informatica",325),
              ("Granada","Informatica",296),
              ("Huelva","Matematicas",41),
              ("Sevilla","Matematicas",122),
              ("Granada","Matematicas",131),
              ("Malaga","Informatica",314)]

totalAlumnos :: [(String,String,Int)] -> Int
totalAlumnos bd = sum [matriculas | (_,_,matriculas) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 34.2. Definir la función 
--    totalMateria :: [(String,String,Int)] -> String -> Int
-- tal que (totalMateria bd m) es el número de alumnos de la base de
-- datos bd matriculados en la materia m. Por ejemplo, 
--    totalMateria matriculas "Informatica" == 935
--    totalMateria matriculas "Matematicas" == 321
--    totalMateria matriculas "Fisica"      == 0
-- ---------------------------------------------------------------------

totalMateria :: [(String,String,Int)] -> String -> Int
totalMateria bd m = sum [matriculas | (_,materia,matriculas) <- bd, materia == m]

-- ---------------------------------------------------------------------
-- Ejercicio 35. Dada una lista de números enteros, definiremos el
-- mayor salto como el mayor valor de las diferencias (en valor
-- absoluto) entre números consecutivos de la lista. Por ejemplo, dada
-- la lista [2,5,-3] las distancias son 
--    3 (valor absoluto de la resta 2 - 5) y
--    8 (valor absoluto de la resta de 5 y (-3))
-- Por tanto, su mayor salto es 8. No está definido el mayor salto para
-- listas con menos de 2 elementos
--
-- Definir la función 
--    mayorSalto :: [Integer] -> Integer
-- tal que (mayorSalto xs) es el mayor salto de la lista xs. Por
-- ejemplo, 
--    mayorSalto [1,5]              == 4
--    mayorSalto [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------

mayorSalto :: [Int] -> Int
mayorSalto xs = sum [abs(x-y) | (x,y) <- zip xs (tail xs)]

-- Fin