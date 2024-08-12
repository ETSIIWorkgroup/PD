-- PilaConListas.hs
-- ImplementaciÃ³n de las pilas mediante listas.
-- JosÃ© A. Alonso JimÃ©nez <jalonso@us.es>
-- Sevilla, 11 de Septiembre de 2010
-- ---------------------------------------------------------------------

module PilaL
    (Pila,
     vacia,    -- Pila a
     apila,    -- a -> Pila a -> Pila a
     cima,     -- Pila a -> a
     desapila, -- Pila a -> Pila a
     esVacia   -- Pila a -> Bool
    ) where

-- RepresentaciÃ³n de las pilas mediante listas.
data Pila a = P [a]
    deriving Eq

-- Procedimiento de escritura de pilas.
instance (Show a) => Show (Pila a) where
    showsPrec p (P [])     cad = showChar '-' cad
    showsPrec p (P (x:xs)) cad
        = shows x (showChar '|' (shows (P xs) cad))

-- Ejemplo de pila:
--    > p1
--    1|2|3|-
p1 = apila 1 (apila 2 (apila 3 vacia))

-- vacia es la pila vacÃ­a. Por ejemplo,
--    > vacia
--    -
vacia   :: Pila a
vacia = P []

-- (apila x p) es la pila obtenida aÃ±adiendo x encima de la pila p. Por
-- ejemplo, 
--    apila 4 p1  =>  4|1|2|3|-
apila :: a -> Pila a -> Pila a
apila x (P xs) = P (x:xs)

-- (cima p) es la cima de la pila p. Por ejemplo,
--    cima p1  ==  1
cima :: Pila a -> a
cima (P [])    = error "cima de la pila vacia"
cima (P (x:_)) = x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo, 
--    desapila p1  =>  2|3|-
desapila :: Pila a -> Pila a
desapila (P [])     = error "desapila la pila vacia"
desapila (P (_:xs)) = P  xs

-- (esVacia p) se verifica si p es la pila vacÃ­a. Por ejemplo,
--    esVacia p1     ==  False
--    esVacia vacia  ==  True
esVacia :: Pila a -> Bool
esVacia (P xs) = null xs
