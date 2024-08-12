-- PD-Practica 1
-- Definiciones de funciones, tipos y clases.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- A continuación se importa el módulo QuickCheck. Necesita ser instalado
-- previamente con Cabal o Stack
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Evalúa las siguientes líneas para entender cómo funciona
-- el sistema de tipos que proporciona Haskell.
-- ---------------------------------------------------------------------
-- :type True
-- :t True
-- :t 1
-- :t 1.1
-- :t 'a'
-- :t "a"
-- :t [1,2]
-- :t [1,2.1]
-- :t [1,'a']
-- :t (1,'s')
-- :t [[1],[1,2]] 
-- :t not
-- :t sum
-- :t (+)
-- :t []
-- :t ()
-- :t (3+)
-- :t length
-- :t zip
-- :t take

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sin evaluar las expresiones en GHC, decide qué tipo es  
-- el adecuado para cada una de ellas. Intenta dar el tipo más general.
-- ---------------------------------------------------------------------

-- i1:: Integer  -- El primero va de regalo
-- i1 = 45

-- i2 = "123"
-- i3 = 45 <= i1
-- i4 = 'c'
-- i5 = ["abc","ok"]
-- i6 = head i5
-- i7 = tail "abc"
-- i8 = (True,4.5)
-- i9 = [i1,34]
-- i10 = sum
-- i11 x = length [1..x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Para cada una de las siguientes expresiones, reemplaza
-- undefined por una expresión válida para el tipo que la declara.
-- ---------------------------------------------------------------------

j1:: (String,Integer)
j1 = ("Hola", 4)

j2:: [Integer]
j2 = undefined

j3:: Char
j3 = undefined

j4:: Double
j4 = undefined

j5:: (Integer,String,Integer,Char)
j5 = undefined

j6:: ([Char],(Bool,String))
j6 = undefined

j7:: [[Bool]]
j7 = undefined

j8:: [(String,Bool)]
j8 = undefined

j9:: Integer -> Integer
j9 = undefined

j10:: Float -> [Bool] -> Bool
j10 = undefined

j11:: [Char] -> [[Int]]
j11 = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 4. Conocemos el cambio actual del euro a dólares estadounidenses: 1
-- Euro son 1.17507 dólares

--   - Definir la constante tipoCambio con dicho valor.
--   - Calcular la expresiones para el cambio a dólares de distintas cantidades 
--     de euros y viceversa
-- ---------------------------------------------------------------------

tipoCambio = 1.17507

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir dos funciones, aEuros y aDolares, que dada una cantidad de
-- dólares (resp. euros) permita obtener la cantidad de euros (resp.
-- dólares) equivalente. Volver a calcular los cambios anteriores utilizando las 
-- funciones definidas.

-- Nota: No es necesario redondear el resultado.
-- ---------------------------------------------------------------------

aEuros :: Float -> Float
aEuros n = n / tipoCambio

aDolares :: Float -> Float
aDolares n = n * tipoCambio

-- ---------------------------------------------------------------------
-- Ejercicio 6. Escribir la siguiente propiedad: dada cualquier cantidad de euros,
-- si la cambiamos a dólares y los dólares obtenidos los volvemos a
-- cambiar a euros, obtenemos la cantidad de euros original.

-- Nota: una propiedad de es función que devuelve un booleano y su cuerpo
--       define una expresión para comprbar una propiedad.
-- ---------------------------------------------------------------------

propiedad_cambio :: Float -> Bool
propiedad_cambio n = aEuros (aDolares n) == n

-- ---------------------------------------------------------------------
-- Ejercicio 7. Si la propiedad anterior ha fallado analiza el posible problema y
-- busca una solución al mismo.
-- ---------------------------------------------------------------------

-- propiedad_cambio' = undefined


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir una función que determinar si una cadena de 
-- caracteres es palíndromo.
--
--
-- $ es_palindromo "anilina"
-- True
-- $ es_palindromo "dábale arroz a la zorra el abad"
-- True
-- $ es_palindromo []
-- False
-- $ es_palindromo "hola"
-- False

-- Nota: consideramos que la cadena vacía no es palíndromo.
-- ---------------------------------------------------------------------

-- Esta funcion viene dada para hacer otro tipo de implementación
-- donde se calcula la mitad de la palabra primero como:
mitad :: String -> String
mitad n = take (length n `div` 2) n 

-- y luego toma la primera mitad de la palabra y la compara frente 
-- a la misma mitad reversada:
es_palindromo_2 :: String -> Bool
es_palindromo_2 [] = False
es_palindromo_2 n = mitad n == mitad(reverse(n))

es_palindromo :: String -> Bool
es_palindromo [] = False
es_palindromo n = reverse(n) == n

-- ---------------------------------------------------------------------
-- Ejercicio 9. Crear una función que genere la letra de DNI. 
--    Para calcular la letra del DNI o caracter de verificación solo debes 
--    de realizar los siguientes pasos:

--    1. Dividir la parte numérica del DNI entre el número 23.
--    2. Tomamos el resto de dicha división y buscamos la lista de 
--       letras
-- ---------------------------------------------------------------------

letrasDNI = "TRWAGMYFPDXBBJZSQVHLCKE"

letraDNI :: Int -> Char
letraDNI n = letrasDNI !! (n `mod` 23)

-- ---------------------------------------------------------------------
-- Ejercicio 10.  Conocemos que 0ºC se corresponden con 32ªF y que un incremento de
-- 5ºC suponen un incremento de 9ºF.

-- Definir una función que permita pasar de ºC a ºF (y otra para el
-- cambio contrario).

-- Si para mañana está prevista un mínimo de 19ºC y un máximo de
-- 34ºC, ¿cuál sería el rango expresado en ºF?
-- ---------------------------------------------------------------------

c2f :: Float -> Float
c2f c = 5/9 * c - 32

f2c :: Float -> Float
f2c f = 9/5 * f + 32

-- ---------------------------------------------------------------------
-- Ejercicio 11. Una tienda vende las mallas de 2kg de patatas a 2.70 euros. Para 
--  favorecer la venta de cantidades mayores ofrece un precio reducido
--  de 2.20 euros a partir de la quinta malla. Es decir, si un cliente
--  compra 18 mallas, las cinco primeras las cobra a 2.70 y las 13
--  restantes a 2.20.

--  * Definir una función que, dada la cantidad de mallas calcule el
--    precio sin tener en cuenta la promoción. Calcular el precio del
--    ejemplo proporcionado.

--  * Definir una función que, dada la cantidad de mallas, calcular el
--    precio correspondiente según la promoción. Usar dicha función
--    para calcular, de nuevo, el precio del ejemplo.

--  La oferta ha tenido tanto éxito que el vendedor decide ampliarla
--  reduciendo el precio a 2 euros a partir de la décima malla.

--  * Definir una función para la nueva promoción y volver al calcular
--    el precio del ejemplo.
-- ---------------------------------------------------------------------

patatas :: Int -> Float
patatas n = fromIntegral n * 2.70

patatasDescontadas :: Int -> Float
patatasDescontadas n 
    | n <= 5 = patatas n
    | otherwise = patatas 5 + (2.20 * fromIntegral(n - 5))

patatasSuperDescontadas :: Int -> Float
patatasSuperDescontadas n 
    | n <= 5 = patatas n
    | n > 5 && n <= 10 = patatas 5 + (2.20 * fromIntegral(n - 5))
    | otherwise = patatas 5 + (2.20 * fromIntegral(n - 5)) + (2 * fromIntegral(n - 10))

-- ---------------------------------------------------------------------
-- Ejercicio 12. Consideremos el siguiente juego: Dado un número mayor que 1, si es
--  par divídelo entre 2 y si es impar multiplícalo por 3 y súmale 1.
--  Si el resultado es 1 ya has terminado, en caso contrario repite el
--  procedimiento sobre el resultado.

juego :: Int -> Int
juego n 
    | n == 1 = 1
    | (fromIntegral n) `mod` 2 == 0 = juego ((fromIntegral n) `div` 2)
    | otherwise = juego ((n * 3) + 1)

--  Pregunta: Dado un número inicial cualquiera, cuántas veces tendrás
--  que aplicar el procedimiento.

--  Ejemplos:

--  Si empezamos por 10 => dividimos por 2 y obtenemos 5 =>
--  multiplicamos por 3 y sumamos 1, obteniendo 16 => toca volver a
--  dividir y obtenemos 8 => repetimos y obtenemos 4 => seguimos y
--  obtenemos 2 => alcanzamos el 1.

--  los valores han sido 5, 16, 8, 4, 2, 1: lo hemos aplicado 6 veces

--  Si empezamos por 7 los valores serán 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5,
--  16, 8, 4, 2, 1: lo hemos aplicado 16 veces.


--  * Definir una función que aplique una vez el procedimiento
--    anterior. Utilizarla sucesivamente para verificar que los
--    resultados proporcionados a partir de 10 y de 7 son correctos.

--    Nota: Pueden ser de utilidad las funciones even y div

--  * Definir una función que dado un número natural mayor que uno
--    calcule el número de veces que se repite el resultado.

--  * Definir una función que devuelva la lista de resultados hasta
--    llegar  a 1.
-- ---------------------------------------------------------------------

juegoSimple :: Int -> Int
juegoSimple n 
    | n == 1 = n
    | even n = juegoSimple ((fromIntegral n) `div` 2)
    | otherwise = juegoSimple ((n * 3) + 1)

-- Lo que hace es extraer la lógica a un método y luego llamar a ese método
-- desde otra función de forma recursiva!
juegoSimpleIfElse :: Int -> Int
juegoSimpleIfElse n = 
    if (n == 1)
    then n
    else (if (even n) 
        then n `div` 2
        else n * 3 + 1)

-- Como el operador : permite añadir un elemento al comienzo de una lista,
-- la llamada recursiva reinvoca la función con el valor de juegoSimpleIfElse
-- y además mantiene la lista con el valor de esa ejecución.
listaRecursiva :: Int -> [Int]
listaRecursiva n 
    | n == 1 = [1]
    | otherwise = n:listaRecursiva (juegoSimpleIfElse n)

-- Para contar, simplemente se hace el lenght de la lista:
repeticiones :: Int -> Int
repeticiones n = length (listaRecursiva n)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Defina la funciÃ³n 'al_ultimo' que toma una lista y
-- envÃ­a a su primer elemento al final de la lista.
--
-- 
-- > al_ultimo [1..3]
-- [2,3,1]
-- > al_ultimo []
-- []
--
-- ---------------------------------------------------------------------

al_ultimo :: [a] -> [a]
al_ultimo [] = []
al_ultimo xs = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 14. Defina la funciÃ³n 'el_del_medio' que toma una lista y
-- devuelve el elemento central. Si el nÃºmero de elementos N es par, debe
-- devolver el elemento N/2. 
--
-- 
-- > el_del_medio [1..3]
-- 2
-- > el_del_medio []
-- *** Exception: Prelude.!!: index too large
-- > el_del_medio [1..4]
-- 3
--
-- ---------------------------------------------------------------------  

-- La comprobación even (largo xs) es inutil para este enunciado, ya que se
-- devuelve el elemento n/2 indiferentemente.
el_del_medio :: [a] -> a
el_del_medio [] = error "Empty List"
el_del_medio xs 
    | even (length xs) = xs !! (length xs `div` 2)
    | otherwise = xs !! (length xs `div` 2)

-- Esta versión se adecua también al enunciado pero sin comprobación ninguna.
el_del_medio_2 :: [a] -> a
el_del_medio_2 [] = error "Empty List"
el_del_medio_2 xs = xs !! (length xs `div` 2)

-- ---------------------------------------------------------------------
-- Ejercicio 15. Defina la funciÃ³n 'particiona' que toma una lista y 
-- una posiciÃ³n y devuelve una listas con dos listas en su interior 
-- con la particion de la lista original por la posiciÃ³n dada.
--
-- 
-- > particiona [1..10] 3
-- [[1,2,3],[4,5,6,7,8,9,10]]
--
-- > particiona [1..10] 0
-- [[],[1,2,3,4,5,6,7,8,9,10]]
--
-- > > particiona [1..10] 20
-- [[1,2,3,4,5,6,7,8,9,10],[]]

-- ---------------------------------------------------------------------  

-- Funciona en tanto que take n xs toma los n primeros elementos de xs y
-- drop n xs borra los n primeros elementos de xs; luego une ambos trozos.
particiona :: [a] -> Int -> [[a]]
particiona [] _ = [[],[]]
particiona xs n
    | n <= 0 = [[], xs]
    | n > length xs = [xs, []]
    | otherwise = [take n xs, drop n xs]

-- ---------------------------------------------------------------------
-- Ejercicio 16. Define la funciÃ³n 'inserta' que aÃ±ade un nuevo elemento
-- a una lista dada en una posiciÃ³n indicada.
--
-- 
-- > inserta 20 [1..10] 3
-- [1,2,3,20,4,5,6,7,8,9,10]
-- 
-- > inserta 20 [1..10] 20
-- [1,2,3,4,5,6,7,8,9,10,20]
-- 
-- > inserta 20 [1..10] 0
-- [20,1,2,3,4,5,6,7,8,9,10]
-- 
-- --------------------------------------------------------------------- 

inserta :: (Eq a) => a -> [a] -> Int -> [a]
inserta p xs n
    | n < 0 = xs
    | n > length xs = xs
    | otherwise = take n xs ++ [p] ++ drop n xs

-- Fin