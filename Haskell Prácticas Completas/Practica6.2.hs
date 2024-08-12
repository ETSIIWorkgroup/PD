-- PD-Práctica 6.2
-- Árboles con tipos de datos algebráicos
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta ejercicios sobre árboles binarios
-- definidos como tipos de datos algebraicos.

-- ---------------------------------------------------------------------
-- Nota. En los siguientes ejercicios se trabajará con los árboles
-- binarios definidos como sigue 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Donde la H representa que es una Hoja, y la N es un nodo interior
-- Por ejemplo, el árbol
--         9 
--        / \
--       /   \
--      3     7
--     / \  
--    2   4 
-- se representa por
--    N 9 (N 3 (H 2) (H 4)) (H 7) 
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------

nHojas :: Arbol a -> Int
nHojas (H _)     = 1
nHojas (N _ i d) = nHojas i + nHojas d

-- Esto funciona porque al ser árboles binarios, si viene una hoja (H _) devolvemos 1;
-- si viene un N siempre tendra dos caminos (de ahí el binario, rama i y rama d), de 
-- las cuales continuamos recursivamente.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ---------------------------------------------------------------------

nNodos :: Arbol a -> Int
nNodos (H _ ) = 0
nNodos (N _ i d) = 1 + nNodos i + nNodos d

-- A la inversa del ejercicio anterior, cuando llegamos a una hoja (H _) ignoramos, 
-- seguimos recursivamente por ambas ramas del arbol, con cada N sumamos 1.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
-- ---------------------------------------------------------------------

profundidad :: Arbol a -> Int
profundidad (H _) = 0                                                   -- Si el arbol es una hoja, la profundidad es 0.
profundidad (N _ i d) = 1 + max (profundidad i) (profundidad d)         -- De lo contrario 1 + la rama más profunda (recursivamente)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    anadeHojas :: Arbol a -> a -> a -> Arbol a
-- tal que (anadeHojas a x y) añade a cada hoja del árbol a
-- dos hojas con los datos x e y. Por ejemplo,
--   anadeHojas (H 5) 0 10 == N 5 (H 0) (H 10)
--   anadeHojas (N 7 (H 5) (H 9)) 1 4 == N 7 (N 5 (H 1) (H 4)) (N 9 (H 1) (H 4))
-- ---------------------------------------------------------------------

anadeHojas :: Arbol a -> a -> a -> Arbol a
anadeHojas (H n) x y = N n (H x) (H y)                -- Asi se convierte H en un nodo y se añaden dos Hojas (H x) y (H y)
anadeHojas (N n ramaA ramaB) x y = N n (anadeHojas ramaA x y) (anadeHojas ramaB x y)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ---------------------------------------------------------------------

preorden :: Arbol a -> [a]
preorden (H n) = [n]
preorden (N n ramaA ramaB) = n : (preorden ramaA ++ preorden ramaB)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    inorden :: Arbol a -> [a]
-- tal que (inorden x) es la lista correspondiente al recorrido
-- inorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación la raíz, y finalmente el subárbol derecho. 
-- Por ejemplo,
--    inorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,3,4,9,7]
-- ---------------------------------------------------------------------

inorden :: Arbol a -> [a]
inorden (H n) = [n]
inorden (N n ramaA ramaB) = inorden ramaA ++ [n] ++ inorden ramaB

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
-- del árbol. Por ejemplo,
--    postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ---------------------------------------------------------------------

postorden :: Arbol a -> [a]
postorden (H n) = [n]
postorden (N n ramaA ramaB) = postorden ramaA ++ postorden ramaB ++ [n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
-- ---------------------------------------------------------------------

espejo :: Arbol a -> Arbol a
espejo (H n) = H n
espejo (N n ramaA ramaB) = N n (espejo ramaB) (espejo ramaA)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La función take está definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
-- 
-- Definir la función 
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el subárbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
-- ---------------------------------------------------------------------
 
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol _ (H n) = H n
takeArbol 0 (N n _ _) = H n
takeArbol p (N n ramaA ramaB) = N n (takeArbol (p-1) ramaA) (takeArbol (p-1) ramaB)

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La función
--    repeat :: a -> [a]
-- está definida de forma que (repeat x) es la lista formada por
-- infinitos elementos x. Por ejemplo,
--    repeat 3  ==  [3,3,3,3,3,3,3,3,3,3,3,3,3,...
-- La definición de repeat es
--    repeat x = xs where xs = x:xs
-- 
-- Definir la función
--    repeatArbol :: a -> Arbol a
-- tal que (repeatArbol x) es es árbol con infinitos nodos x. Por
-- ejemplo, 
--    takeArbol 0 (repeatArbol 3) == H 3
--    takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--    takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ---------------------------------------------------------------------

repeatArbol :: a -> Arbol a
repeatArbol x = N x t t
  where t = repeatArbol x

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La función 
--    replicate :: Int -> a -> [a]
-- está definida por 
--    replicate n = take n . repeat
-- es tal que (replicate n x) es la lista de longitud n cuyos elementos
-- son x. Por ejemplo,
--    replicate 3 5  ==  [5,5,5]
-- 
-- Definir la función 
--    replicateArbol :: Int -> a -> Arbol a
-- tal que (replicate n x) es el árbol de profundidad n cuyos nodos son
-- x. Por ejemplo,
--    replicateArbol 0 5  ==  H 5
--    replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--    replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ---------------------------------------------------------------------

replicateArbol :: Int -> a -> Arbol a
replicateArbol n = takeArbol n . repeatArbol

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la función
--    mapArbol :: (a -> a) -> Arbol a -> Arbol a
-- tal que (mapArbol f x) es el árbol obtenido aplicándole a cada nodo de
-- x la función f. Por ejemplo,
--    ghci> mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7)) 
--    N 18 (N 6 (H 4) (H 8)) (H 14)
-- ---------------------------------------------------------------------

mapArbol :: (a -> a) -> Arbol a -> Arbol a
mapArbol f (H n) = H (f n)
mapArbil f (N n ramaA ramaB) = N (f n) (mapArbol f ramaA) (mapArbol f ramaB)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Se consideran los árboles con operaciones booleanas
-- definidos por   
--    data ArbolB = HB Bool 
--                | Conj ArbolB ArbolB
--                | Disy ArbolB ArbolB
--                | Neg ArbolB
-- 
-- Por ejemplo, los árboles
--                Conj                            Conj          
--               /   \                           /   \          
--              /     \                         /     \         
--           Disy      Conj                  Disy      Conj     
--          /   \       /  \                /   \      /   \    
--       Conj    Neg   Neg True          Conj    Neg   Neg  True 
--       /  \    |     |                 /  \    |     |        
--    True False False False          True False True  False     
--
-- se definen por
--    ej1, ej2:: ArbolB
--    ej1 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB False)))
--               (Conj (Neg (HB False))
--                     (HB True))
--    
--    ej2 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB True)))
--               (Conj (Neg (HB False))
--                     (HB True))
-- 
-- Definir la función 
--    valorB :: ArbolB -> Bool
-- tal que (valorB ar) es el resultado de procesar el árbol realizando
-- las operaciones booleanas especificadas en los nodos. Por ejemplo,
--    valorB ej1 == True
--    valorB ej2 == False
-- ---------------------------------------------------------------------

-- Este 'tipo' lo que define es un Arbol cuyas ramas pueden ser de tres tipos diferentes;
-- esto es, hay ramas de tipo Conj (CONJUNCIÓN - AND), Disy (DISYUNCIÓN - OR) y Neg (NEGACIÓN - NOT). 
-- Que a su vez se ramifican en nuevos ArbolB (uno solo en el caso de Neg). El 'dato' ArbolB es 
-- de tipo Booleano por eso las hojas son de tipo Bool siempre.

data ArbolB = HB Bool 
            | Conj ArbolB ArbolB
            | Disy ArbolB ArbolB
            | Neg ArbolB

ej1 :: ArbolB
ej1 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB False)))
           (Conj (Neg (HB False))
                 (HB True))
ej2 :: ArbolB
ej2 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB True)))
           (Conj (Neg (HB False))
                 (HB True))

valorB:: ArbolB -> Bool
valorB (HB n) = n                                             -- Cuando es una hoja (True/False) = se devuelve el valor
valorB (Neg n) = not (valorB n)                               -- Cuando es una negación, solo hay un valor booleano, se niega el valor
valorB (Conj ramaA ramaB) = (valorB ramaA) && (valorB ramaB)  -- Cuando es una conjunción (hay dos ramas) se hace el AND de ambas
valorB (Disy ramaA ramaB) = (valorB ramaA) || (valorB ramaB)  -- Cuando es una disyunción (hay dos ramas) se hace el OR de ambas

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los árboles generales se pueden representar mediante el
-- siguiente tipo de dato  
--    data ArbolG a = N a [ArbolG a]
--                  deriving (Eq, Show)
-- Por ejemplo, los árboles
--      1               3               3
--     / \             /|\            / | \
--    2   3           / | \          /  |  \
--        |          5  4  7        5   4   7
--        4          |     /\       |   |  / \
--                   6    2  1      6   1 2   1
--                                     / \
--                                    2   3
--                                        |
--                                        4
-- se representan por
--    ejG1, ejG2, ejG3 :: ArbolG Int
--    ejG1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ejG2 = N 3 [N 5 [N 6 []], 
--               N 4 [], 
--               N 7 [N 2 [], N 1 []]]
--    ejG3 = N 3 [N 5 [N 6 []], 
--               N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
--               N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--     ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
-- tal que (ramifica a1 a2 p) el árbol que resulta de añadir una copia
-- del árbol a2 a los nodos de a1 que cumplen un predicado p. Por
-- ejemplo, 
--    ramifica ejG1 (NG 8 []) (>4) =>  NG 1 [NG 2 [],NG 3 [NG 4 []]]
--    ramifica ejG1 (NG 8 []) (>3) =>  NG 1 [NG 2 [],NG 3 [NG 4 [NG 8 []]]]
--    ramifica ejG1 (NG 8 []) (>2) =>  NG 1 [NG 2 [],NG 3 [NG 4 [NG 8 []],NG 8 []]]
--    ramifica ejG1 (NG 8 []) (>1) =>  NG 1 [NG 2 [NG 8 []],NG 3 [NG 4 [NG 8 []],NG 8 []]]
--    ramifica ejG1 (NG 8 []) (>0) =>  NG 1 [NG 2 [NG 8 []],NG 3 [NG 4 [NG 8 []],NG 8 []],NG 8 []]
-- ---------------------------------------------------------------------

data ArbolG a = NG a [ArbolG a]
              deriving (Eq, Show)

ejG1 :: ArbolG Int
ejG1 = NG 1 [NG 2 [],NG 3 [NG 4 []]]

ejG2 :: ArbolG Int
ejG2 = NG 3 [NG 5 [NG 6 []], 
           NG 4 [], 
           NG 7 [NG 2 [], NG 1 []]]

ejG3 :: ArbolG Int
ejG3 = NG 3 [NG 5 [NG 6 []], 
           NG 4 [NG 1 [NG 2 [],NG 3 [NG 4 []]]], 
           NG 7 [NG 2 [], NG 1 []]]

ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
--        NG valor lista_de_valores      NG valor lista_de_valores
ramifica (NG x xs) arbolDado predicado = NG x [ramifica rama arbolDado predicado | rama <- xs]
--                                                      Donde rama es el resto de las listas de NG quitando x

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    nHojasG :: ArbolG a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojasG ejG1  ==  2
--    nHojasG ejG2  ==  4
--    nHojasG ejG3  ==  5
-- ---------------------------------------------------------------------

-- Si un ArbolG es como: NG 1 [NG 2 [],NG 3 [NG 4 []]]

nHojasG :: ArbolG a -> Int
nHojasG (NG _ []) = 1
nHojasG (NG _ xs) = sum $ map nHojasG xs

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    profundidad :: ArbolG a -> Int
-- tal que (profundidadG x) es la profundidad del árbol x. Por ejemplo,
--    profundidadG ejG1  ==  2
--    profundidadG ejG2  ==  2
--    profundidadG ejG3  ==  4
-- ---------------------------------------------------------------------

-- Si un ArbolG es como: NG 1 [NG 2 [],NG 3 [NG 4 []]]

profundidadG :: ArbolG a -> Int
profundidadG (NG _ []) = 0
profundidadG (NG _ xs) = 1 + (maximum (map profundidadG xs))

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    bin2gen :: ArbolG a -> Int
-- tal que (bin2gen x) es la traducción del árbol x definido con el tipo
-- "Arbol" (es decir, árbol binario) a "ArbolG" (es decir, árbol
-- genérico). Por ejemplo,
--    bin2gen (N 9 (N 3 (H 2) (H 4)) (H 7)) ==  (NG 9 [NG 3 [NG 2 [],NG 4 []], NG 7 []])
-- ---------------------------------------------------------------------

bin2gen :: Arbol a -> ArbolG a
bin2gen (H n) = NG n []
bin2gen (N n ramaA ramaB) = NG n [izquierda, derecha]
      where izquierda = bin2gen ramaA 
            derecha   = bin2gen ramaB

--gen2bin :: ArbolG a -> Arbol a
--gen2bin (NG x []) = H x
--gen2bin (NG x xs) = N x [gen2bin xs] [gen2bin xs]

-- Fin