-- PD. Práctica 6.5 
-- Tipos: definición y uso de tipos (exámenes antiguos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Data.Default

-- ---------------------------------------------------------------------
-- Ejercicio 1
-- Las Torres de Hanoi es un juego matemático. Consiste en tres varillas verticales y
-- un número indeterminado de discos que determinarán la complejidad de la solución.
-- No hay dos discos iguales, están colocados de mayor a menor en la primera varilla
-- ascendentemente, y no se puede colocar ningún disco mayor sobre uno menor a él
-- en ningún momento. El juego consiste en pasar todos los discos a la tercera varilla
-- colocados de mayor a menor ascendentemente.
--
-- En este ejercicio representamos las varillas del juego con las cadenas "I", "C" y
-- "D" y los tamaños de los n discos del juego con los números enteros de 1 a n.
-- Definir el tipo de datos PilaDeDiscos como un sinónimo de una lista de números
-- de tipo Int y el tipo de datos Varilla como un sinónimo de cadena.
-- Definir un nuevo tipo de datos TorreDeHanoi que tenga un único constructor
-- con tres argumentos que sean del tipo PilaDeDiscos. Siempre asumiremos que al
-- construir un valor de este tipo de datos las pilas de discos proporcionadas son
-- correctas.
--
-- Definir la función moverDisco que reciba una TorreDeHanoi y dos Varillas y
-- devuelva la TorreDeHanoi resultante de mover un disco de la primera a la segunda
-- Varilla proporcionadas. Siempre asumiremos que todos los argumentos recibidos
-- son correctos y que el movimiento se puede realizar.
--
-- Ejemplo:
--
torre = Torre [1,2,3] [4,5,6] [7,8,9]

-- > moverDisco torre "I" "C"
-- Torre [2,3] [1,4,5,6] [7,8,9]
-- > moverDisco torre "D" "I"
-- Torre [7,1,2,3] [4,5,6] [8,9]

torre2 = Torre [1,2,3,7,8,9] [4,5,6] []

-- > moverDisco torre2 "D" "I"
-- Torre [1,2,3,7,8,9] [4,5,6] []

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2:
-- Consideremos la siguiente definición de un nuevo tipo de dato que representa de
-- manera recursiva los polinomios con coeficientes enteros:

type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
                   deriving Show

-- Es decir, un polinomio con coeficientes enteros es el polinomio cero (0x^0)
-- o un polinomio obtenido añadiendo un nuevo término con un cierto coeficiente entero y
-- un cierto grado a un polinomio ya existente. Siempre asumiremos que no añadimos
-- un término de grado igual a uno del polinomio ya existente, con la única excepción
-- de que el término sea de grado 0, que se podrá añadir si el único término de grado
-- 0 del polinomio ya existente es 0x^0. Por otra parte, los términos no tienen por qué
-- añadirse en orden creciente de grado.
-- Definir la función grado que reciba un Polinomio y devuelva el grado de ese
-- polinomio. Siempre asumiremos que el polinomio recibido está construido de manera
-- correcta.
--
pol1, pol2 :: Polinomio Int
pol1 = Pol (1,2) PolCero
pol2 = Pol (1,2) (Pol (3,3) PolCero)

-- > grado pol1 
-- 2
-- > grado pol1 
-- 3

-- ---------------------------------------------------------------------


-- -------------------------------------------------------------------
-- Ejercicio 3. (parcial 2 del curso 2018/19)
--
-- 1. Defina, con sintaxis de registro, un nuevo tipo que contenga la
--    información sobre planetas que aparecen en las películas
--    de star wars:
--    * name, diameter, population, de tipo String
--    * residents, de tipo lista de String
--
-- 2. Haga que el tipo anterior disponga de un valor por defecto,
-- de modo que podamos posteriormente crear elementos del tipo
-- sin necesidad de proporcionar todos los datos solicitados
--
-- 3. Defina un tipo sinónimo de una lista de planetas
--
-- -------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 4. 
-- Los árboles binarios con valores distintos en nodos y en hojas se 
-- pueden representar mediante el tipo Arbol definido por 

data Arbol a b = H b 
                | N a (Arbol a b) (Arbol a b)
            deriving Show

-- Por ejemplo, un árbol con pares de enteros en los nodos y enteros en las
-- hojas:
--        (4,8)
--         / \ 
--        /   \
--       /     \
--    (4,2)   (8,5)
--     / \     / \
--    4   2   8   5 
--
-- se puede definir por
--    N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5)) 

-- Un árbol como el anterior lo vamos a llamar Árbol de Máximos, ya que para
-- cada nodo interno hay un par que indica con la primera componente cuál es
-- el valor máximo de las hojas por el subárbol izquierdo, y en la segunda
-- componente cual es el máximo de las hojas del subárbol derecho. Define 
-- la función
--    maximosHojas :: Arbol Int Int -> Arbol (Int,Int) Int
-- tal que (maximosHojas a) calcule el árbol de máximos, tal y como se ha 
-- ilustrado arriba, para el árbol a. Por ejemplo,

ej1,ej2,ej3 :: Arbol Int Int 
ej1 = (N 1 (N 1 (H 4) (H 2)) (N 1 (H 8) (H 5)))
ej2 = (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
ej3 = (N 29 (H 1) (N 524 (H 1) (H 1)))

-- > maximosHojas ej1
-- N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5))
-- > maximosHojas ej2
-- N (8,9) (N (8,4) (H 8) (H 4)) (N (4,9) (H 4) (H 9))
-- > maximosHojas ej3
-- N (1,1) (H 1) (N (1,1) (H 1) (H 1))
-- ---------------------------------------------------------------------