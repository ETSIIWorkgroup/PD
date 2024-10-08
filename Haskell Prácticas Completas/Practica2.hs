-- PD-Practica 2
-- Definiciones con condicionales, guardas o patrones.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones elementales
-- (no recursivas) de funciones que usan condicionales, guardas o
-- patrones. 
-- 
-- Estos ejercicios se corresponden con el tema 4

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

-- Primera versión -> Con if/then/else
divisionSegura :: Double -> Double -> Double
divisionSegura x y = if (y == 0) 
  then 9999 
  else x / y

-- Segunda versión -> Por guardas
divisionSegura2 :: Double -> Double -> Double
divisionSegura2 x y 
  | y == 0 = 9999
  | otherwise = x / y

-- Tercera versión -> Por patrones (el más específico arriba)
divisionSegura3 :: Double -> Double -> Double
divisionSegura3 x 0 = 9999
divisionSegura3 x y = x / y

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False 
--    True  | False | True
--    False | True  | True
--    False | False | False
--    
-- Definir la función 
--    xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada línea
-- de la tabla. 
-- ---------------------------------------------------------------------

-- Una línea por opción
xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False = False

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 x True = if x == True then False else True
xor2 x False = if x == True then True else False

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada 
-- a partir de la disyunción (||), conjunción (&&) y negación (not). 
-- Usar 1 ecuación. 
-- ---------------------------------------------------------------------

xor3 :: Bool -> Bool -> Bool
xor3 x y = (x && not y) || (not x && y)

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la función 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor4 x y) es la disyunción excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

xor4 :: Bool -> Bool -> Bool
xor4 x y = x /= y

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que las cuatros definiciones
-- de xor son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_xor_equivalentes x y = xor1 x y == xor2 x y && xor2 x y == xor3 x y && xor3 x y == xor4 x y

-- La comprobación es
--quickCheck prop_xor_equivalentes

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las dimensiones de los rectángulos puede representarse 
-- por pares; por ejemplo, (5,3) representa a un rectángulo de base 5 y 
-- altura 3. 
-- 
-- Definir la función 
--    mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área ente
-- r1 y r2. Por ejemplo,  
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ---------------------------------------------------------------------

mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo (x,y) (z,t)
  | x*y >= z*t = (x,y)
  | otherwise = (z,t)

-- Otra versión con if/else:
mayorRectanguloIfElse :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectanguloIfElse (x,y) (z,t) = 
  if x*y >= z*t 
    then (x,y)
  else (z,t)

-- Otra versión extrayendo la multiplicacion a un método externo:
area :: (Int, Int) -> Int
area (x,y) = x*y

mayorRectanguloExterno :: (Int,Int) -> (Int,Int) -> (Int,Int)
mayorRectanguloExterno (x,y) (z,t)
  | (area(x,y)) >= (area(z,t)) = (x,y)
  | otherwise = (z,t)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia (a,b) = (b,a)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la función intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia (x,y) = intercambia(intercambia(x,y)) == (x,y)

-- La comprobación es
-- quickCheck prop_intercambia

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función 
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo, 
--    distancia (1,2) (4,6)  ==  5.0
-- ---------------------------------------------------------------------
 
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x,y) (z,t) = sqrt(cuadrado_diferencia(x,z) + cuadrado_diferencia(y,t))

cuadrado_diferencia :: (Double, Double) -> Double
cuadrado_diferencia (x,y) = (x-y)^2

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3,
-- la distancia de p1 a p3 es menor o igual que la suma de la distancia
-- de p1 a p2 y la de p2 a p3.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Bool
prop_triangular (x,y) (z,t) (m,n) = distancia (x,y) (m,n) <= (distancia (x,y) (z,t) + distancia (z,t) (m,n))

-- La comprobación es
-- quickCheck prop_triangular

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir una función 
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo, 
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
-- ---------------------------------------------------------------------

-- Primera versión: uniendo el ultimo elemento de la lista a xs sin el primer elemento
ciclo1 :: [a] -> [a]
ciclo1 xs = last xs : init xs

-- Segunda versión (posibilidad de lista vacía)
ciclo2 :: [a] -> [a]
ciclo2 xs
  | length xs == 0 = []
  | length xs == 1 = xs
  | otherwise = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar que la longitud es un invariante de la
-- función ciclo; es decir, la longitud de (ciclo xs) es la misma que la
-- de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ciclo xs = length xs == length(ciclo2 xs)

-- La comprobación es
-- quickCheck prop_ciclo

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y
  | x * 10 + y >= y * 10 + x = x * 10 + y
  | otherwise = y * 10 + x

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raíces reales de la
-- ecuación a*x^2 + b*x + c = 0. Por ejemplo,
--    numeroDeRaices 2 0 3    ==  0
--    numeroDeRaices 4 4 1    ==  1
--    numeroDeRaices 5 23 12  ==  2
-- ---------------------------------------------------------------------

numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
  | raiz < 0 = 0
  | raiz == 0 = 1
  | otherwise = 2
    where raiz = b^2 - 4*a*c

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función 
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raíces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
-- ---------------------------------------------------------------------

raices :: Double -> Double -> Double -> [Double]
raices a b c 
    | d >= 0    = [(-b+e)/t,(-b-e)/t]
    | otherwise = []
    where d = b**2 - 4*a*c
          e = sqrt d
          t = 2*a

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milésima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y = abs (x - y) < 0.001
infix 4 ~=

-- infix 4 ~=  lo que hace es setear la precedencia de dicho operador
-- teniendo en cuenta que la lista de precedencias es:

    -- 9: .
    -- 8: **
    -- 7: *, /, `div`, `mod`
    -- 6: +, -
    -- 5: ++,:
    -- 4: ==, /=, <, <=, >, >=
    -- 3: &&
    -- 2: ||
    -- 1: >>, >>=

-- , lo cual hace que x ~= y tenga la misma precedencia que ==.

-- ---------------------------------------------------------------------
-- Ejercicio 10. En geometría, la fórmula de Herón, descubierta por
-- Herón de Alejandría, dice que el área de un triángulo cuyo lados
-- miden a, b y c es la raíz cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperímetro 
--    s = (a+b+c)/2
-- 
-- Definir la función 
--    areaTriangulo :: Double -> Double -> Double -> Double 
-- tal que (areaTriangulo a b c) es el área del triángulo de lados a, b y c. Por
-- ejemplo, 
--    areaTriangulo 3 4 5  ==  6.0
-- ---------------------------------------------------------------------

areaTriangulo :: Double -> Double -> Double -> Double
areaTriangulo a b c = sqrt(s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior). 
-- 
-- Definir la función 
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1, a2] [b1, b2]
  | mayor a1 b1 <= menor a2 b2 = [mayor a1 b1, menor a2 b2]
  | otherwise = []
interseccion _ _ = error "ERROR"

mayor :: Ord a => a -> a -> a
mayor x y
  | x >= y = x
  | otherwise = y

menor :: Ord a => a -> a -> a
menor x y 
  | x >= y = y
  | otherwise = x

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que la intersección de
-- intervalos es conmutativa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Bool
prop_interseccion a b c d = interseccion [a,b][c,d] == interseccion [c,d][a,b]

-- La comprobación es
-- quickCheck prop_interseccion

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Los números racionales pueden representarse mediante
-- pares de números enteros. Por ejemplo, el número 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la función 
--    formaReducida :: (Int,Int) -> (Int,Int) 
-- tal que (formaReducida x) es la forma reducida del número racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
-- ---------------------------------------------------------------------

formaReducida :: (Int,Int) -> (Int,Int)
formaReducida (x,y) = (x `div` d, y `div` d)
  where d = gcd x y

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la función 
--    sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (sumaRacional x y) es la suma de los números racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
-- ---------------------------------------------------------------------

sumaRacionalSinReducir :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacionalSinReducir (a,b) (c,d) = (a*d+b*c, b*d)

sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = formaReducida(a*d+b*c, b*d)

-- a   c   a*d + c*b
-- _ + _ = _________ 
-- b   d      b*d

-- ---------------------------------------------------------------------
-- Ejercicio 12.3.1 Definir la función 
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (productoRacional x y) es el producto de los números
-- racionales x e y. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------

productoRacionalSinReducir :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacionalSinReducir (a,b) (c,d) = (a*c, b*d)

productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = formaReducida(a*c, b*d)

-- a   c   a*c
-- _ * _ = ___
-- b   d   b*d

-- ---------------------------------------------------------------------
-- Ejercicio 12.3.2 Definir la función 
--    divisionRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (divisionRacional x y) es el producto de los números
-- racionales x e y. Por ejemplo, 
--    divisionRacional (7,6) (2,5)  ==  (35,12)
-- ---------------------------------------------------------------------

divisionRacionalSinReducir :: (Int,Int) -> (Int,Int) -> (Int,Int)
divisionRacionalSinReducir (a,b) (c,d) = (a*d, b*c)

divisionRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
divisionRacional (a,b) (c,d) = formaReducida(a*d, b*c)

-- a   c   a*d
-- _ / _ = ___
-- b   d   b*c

-- ---------------------------------------------------------------------
-- Ejercicio 12.4. Definir la función 
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tal que (igualdadRacional x y) se verifica si los números racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------

igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d)
  | division(a,b) == division(c,d) = True
  | otherwise = False

division :: (Int,Int) -> Double
division (x,y) = fromIntegral(x) / fromIntegral(y)

-- ---------------------------------------------------------------------
-- Ejercicio 12.5. Comprobar con QuickCheck la propiedad distributiva
-- del producto racional respecto de la suma.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_distributiva :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Property
prop_distributiva x@(_,d1) y@(_,d2) z@(_,d3) =
  d1>0 && d2>0 && d3>0 ==> igualdadRacional prodSuma sumaProd
  where prodSuma = productoRacional x (sumaRacional y z)
        sumaProd = sumaRacional (productoRacional x y) (productoRacional x z)

-- La comprobación es
-- quickCheck prop_distributiva

-- Fin