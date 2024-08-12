-- PD-Práctica 6.3
-- Expresiones Aritméticas con tipos de datos algebráicos
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================



-- ---------------------------------------------------------------------
-- Ejercicio 1. Las expresiones aritméticas básicas pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr1 = C1 Int 
--               | S1 Expr1 Expr1 
--               | P1 Expr1 Expr1  
--               deriving Show
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P1 (C1 2) (S1 (C1 3) (C1 7))
-- 
-- Definir la función evalua, tal que (evalua e) es el valor de la 
-- expresión aritmética e. Por ejemplo, 
--    evalua (P1 (C1 2) (S1 (C1 3) (C1 7)))  ==  20
-- ---------------------------------------------------------------------

data Expr1 = C1 Int 
           | S1 Expr1 Expr1 
           | P1 Expr1 Expr1  
           deriving Show

evalua :: Expr1 -> Int
evalua (C1 n) = n
evalua (S1 s1 s2) = (evalua s1) + (evalua s2)
evalua (P1 p1 p2) = (evalua p1) * (evalua p2)

-- Ejemplo de teoría
data Nat = Cero | Suc Nat
                deriving Show

nat2int :: Nat -> Int
nat2int Cero    = 0
nat2int (Suc n) = 1 + nat2int n

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función aplica, tal que (aplica f e) es la 
-- expresión obtenida aplicando la función f a cada uno de los números 
-- de la expresión e. Por ejemplo, 
--    ghci> aplica (+2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 5) (C1 7)) (P1 (C1 8) (C1 9))
--    ghci> aplica (*2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 6) (C1 10)) (P1 (C1 12) (C1 14))
-- ---------------------------------------------------------------------

aplica :: (Int -> Int) -> Expr1 -> Expr1
aplica f (C1 n) = C1 (f n)
aplica f (S1 s1 s2) = S1 (aplica f s1) (aplica f s2)
aplica f (P1 p1 p2) = P1 (aplica f p1) (aplica f p2)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas construidas con una
-- variable (denotada por X), los números enteros y las operaciones de
-- sumar y multiplicar se pueden representar mediante el tipo de datos
-- Expr2 definido por     
--    data Expr2 = X
--               | C2 Int
--               | S2 Expr2 Expr2
--               | P2 Expr2 Expr2
-- Por ejemplo, la expresión "X*(13+X)" se representa por
-- "P2 X (S2 (C2 13) X)".
-- 
-- Definir la función evaluaE, tal que (evaluaE e n) es el valor de la 
-- expresión e cuando se sustituye su variable por n. Por ejemplo,
--    evaluaE (P2 X (S2 (C2 13) X)) 2  ==  30
-- ---------------------------------------------------------------------
 
data Expr2 = X
           | C2 Int
           | S2 Expr2 Expr2
           | P2 Expr2 Expr2

evaluaE :: Expr2 -> Int -> Int
evaluaE X n = n
evaluaE (C2 x) n = x
evaluaE (S2 s1 s2) n = (evaluaE s1 n) + (evaluaE s2 n)
evaluaE (P2 p1 p2) n = (evaluaE p1 n) * (evaluaE p2 n)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función numVars, tal que (numVars e) es el 
-- número de variables en la expresión e. Por ejemplo, 
--    numVars (C2 3)                 ==  0
--    numVars X                      ==  1
--    numVars (P2 X (S2 (C2 13) X))  ==  2
-- ---------------------------------------------------------------------

numVars :: Expr2 -> Int
numVars X      = 1
numVars (C2 n) = 0
numVars (S2 s1 s2) = (numVars s1) + (numVars s2)
numVars (P2 p1 p2) = (numVars p1) + (numVars p2)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las expresiones aritméticas con variables genéricas 
-- pueden representarse usando el siguiente tipo de datos  
--    data Expr3 = C3 Int 
--               | V3 Char 
--               | S3 Expr3 Expr3 
--               | P3 Expr3 Expr3  
--               deriving Show
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P3 (C3 2) (S3 (V3 'a') (C3 5))
-- 
-- Definir la función evaluaG, tal que (evaluaG e c) es el valor de la 
-- expresión e en el contexto c (es decir, el valor de la expresión 
-- donde las variables de e se sustituyen por los valores según se 
-- indican en el contexto c). Por ejemplo,
--    ghci> evaluaG (P3 (C3 2) (S3 (V3 'a') (V3 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

data Expr3 = C3 Int 
           | V3 Char 
           | S3 Expr3 Expr3 
           | P3 Expr3 Expr3  
           deriving Show

evaluaG :: Expr3 -> [(Char,Int)] -> Int 
evaluaG (C3 n) c = n
evaluaG (V3 n) c = operando c n         
evaluaG (S3 s1 s2) c = (evaluaG s1 c) + (evaluaG s2 c)
evaluaG (P3 p1 p2) c = (evaluaG p1 c) * (evaluaG p2 c)

operando :: [(Char,Int)] -> Char -> Int               
operando c n = head [numero | (caracter, numero) <- c, caracter == n]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función sumas, tal que (sumas e) es el 
-- número de sumas en la expresión e. Por ejemplo, 
--    sumas (P3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  1
--    sumas (S3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  2
--    sumas (P3 (V3 'z') (P3 (C3 3) (V3 'x')))  ==  0
-- ---------------------------------------------------------------------
                   
sumas :: Expr3 -> Int
sumas (C3 n) = 0
sumas (V3 n) = 0
sumas (S3 s1 s2) = 1 + (sumas s1) + (sumas s2)     
sumas (P3 p1 p2) = (sumas p1) + (sumas p2)           

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función sustitucion, tal que 
-- (sustitucion e s) es la expresión obtenida sustituyendo las variables 
-- de la expresión e según se indica en la sustitución s. Por ejemplo, 
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'x'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (C3 7))
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'y'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (V3 'y'))
-- ---------------------------------------------------------------------

-- sustitucion :: Expr3 -> [(Char,Int)] -> Expr3
-- sustitucion (C3 n) c = C3 n
-- sustitucion (V3 n) c = cambia c n
-- sustitucion (S3 s1 s2) c = S3 (sustitucion s1 c) (sustitucion s2 c)
-- sustitucion (P3 p1 p2) c = P3 (sustitucion p1 c) (sustitucion p2 c)

-- cambia :: [(Char,Int)] -> Char -> Expr3
-- cambia c n
--     | notElem n [caracter | (caracter,numero) <- c] = V3 n
--     | otherwise = V3 caracterCambiado
--         where caracterCambiado = head [numero | (caracter, numero) <- c, caracter == n]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función reducible, tal que (reducible e) se 
-- verifica si e es una expresión reducible; es decir, contiene alguna 
-- operación en la que los dos operandos son números. Por ejemplo,
--    reducible (S3 (C3 3) (C3 4))               == True
--    reducible (S3 (C3 3) (V3 'x'))             == False
--    reducible (S3 (C3 3) (P3 (C3 4) (C3 5)))   == True
--    reducible (S3 (V3 'x') (P3 (C3 4) (C3 5))) == True
--    reducible (S3 (C3 3) (P3 (V3 'x') (C3 5))) == False
--    reducible (C3 3)                           == False
--    reducible (V3 'x')                         == False
-- ---------------------------------------------------------------------

reducible :: Expr3 -> Bool
reducible (C3 n) = False
reducible (V3 n) = False
reducible (S3 s1 s2) = (reducible s1) && (reducible s2)
reducible (P3 p1 p2) = (reducible p1) && (reducible p2)
reducible (S3 (C3 _) (C3 _)) = True
reducible (P3 (C3 _) (C3 _)) = True

-- Fin