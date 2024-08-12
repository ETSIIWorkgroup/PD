-- PD-Práctica 6.4
-- Árboles Trie
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Un árbol Trie es un árbol que codifica un diccionario, es decir,
-- asociaciones de clave - valor. La peculiaridad de estos árboles es que
-- los nodos internos codifican las claves de forma eficiente, ya que
-- los prefijos comunes a las claves solo aparecen una vez. En
-- concreto, se puede usar para almacenar cadenas como claves de forma
-- eficiente, si en cada nodo interno se almacena una letra y sus hijos
-- son las posibles letras que le pueden suceder. De esta forma la 
-- representación de todo un vocabulario se compacta. A continuación se
-- muestra un ejemplo, donde las Claves son nombres de personas y los
-- Valores son enteros que representan un número de teléfono.
--
--                              ""
--                             /  \
--                           "J"  "I"
--                            |    | \ 
--                           "U"  "V" "N"
--                           / \    \   \
--                         "A" "L"  "A" "E"
--                         /     \   |    \
--                       "N"    "I" "N"   "S"
--                      / |      |    \     \
--                   "A" 68972  "A"  69712  66631
--                    |         / \
--                 63822    67321 62375
--                              
-- el árbol de ejemplo almacena los teléfonos de los siguientes contactos:
--  "JUAN" -> 68972, "JULIA" -> 67321, "JULIA" -> 62375, "IVAN" -> 69712,
--  "JUANA" -> 63822, "INES" -> 66631
-- Nótese que hay dos nombres repetidos ("JULIA"). De esta manera, las
-- claves se distribuyen en los nodos internos, de tal forma que cada nodo
-- tiene asociado tan solo un carácter en forma de cadena (la clave es la
-- concatenación de la sucesión de nodos internos desde la raíz hasta la
-- hoja.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir el tipo de datos para un árbol Trie polimórfico,
-- donde los nodos internos almacenen un elemento de un tipo Clave y puedan
-- tener más de un hijo, y las hojas almacenen tan solo un Valor. El árbol
-- debe ser imprimible. Llámalo ArbolTrie, y usa como constructores HT
-- para las hojas y NT para los nodos internos.

data ArbolT = NODO String [ArbolT] 
            | HOJA Int 
            deriving Show

-- Ejercicio 1.2. Definir un sinónimo de árbol Trie que emplee cadenas
-- como Claves y enteros como Valores. Llámalo ArbolTrieContactos.

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir las funciones siguientes:
--    (a) (arbolTrieVacio), que devuelva un árbol con solo el nodo raíz,
--         el cual tiene como clave la cadena vacía ("") y ningún hijo.
--    (b) (clave n), que devuelva la clave asociado al nodo n. Si n es
--         una hoja, devolver la cadena vacía "".
--    (c) (esHoja n), que indique con un booleano si el nodo n es una hoja.
-- Por ejemplo,
-- λ> arbolTrieVacio
-- NT "" []
-- λ> clave (NT "c" [])
-- "c"
-- λ> clave (HT 433)
-- ""
-- λ> esHoja (HT 433)
-- True
-- λ> esHoja (NT "c" [])
-- False

arbolTrieVacio = undefined

clave = undefined

esHoja = undefined 

-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Ejercicio 3.1 Definir la función (buscaClave s as), tal que reciba una
-- lista de árboles as y una cadena de un solo carácter s, y devuelva el
-- árbol cuya clave coincida con s. Si tal árbol no existe, entonces
-- será un nodo interno nuevo con clave s y sin hijos. Por ejemplo,
-- λ> buscaClave "s" [NT "o" [],NT "s" [HT 100]]
-- NT "s" [HT 100]
-- λ> buscaClave "x" [NT "o" [],NT "s" [HT 100]]
-- NT "x" []

buscaClave = undefined

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 3.2 Definir la función (siguienteNodo hs s), que reciba una
-- lista de árboles as y una cadena de un solo carácter s, y devuelva un
-- par tal que:
--  1. El primer elemento del par será el resultado de llamar a la función
--     buscaClave con s y as.
--  2. El segundo elemento del par serán todos los nodos de as cuyas claves
--     no coincidan con s.
-- Por ejemplo,
-- λ> siguienteNodo "s" [NT "o" [],NT "s" [HT 100]]
-- (NT "s" [HT 100],[NT "o" []])
-- λ> siguienteNodo "x" [NT "o" [],NT "s" [HT 100]]
-- (NT "x" [],[NT "o" [],NT "s" [HT 100]])

siguienteNodo = undefined 

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función (inserta a p ), que reciba un
-- árbol Trie, a, y un par p = (clave, valor), siendo clave una cadena
-- de caracteres, y valor es un entero. La función debe devolver el
-- árbol a extendido tal que incluya el nuevo par (clave,valor). El
-- procedimiento es como sigue:
--   - Si la clave es la cadena vacía, se añade el valor como hoja del
--     nodo interno actual
--   - En otro caso, se llama a siguienteNodo con la primera letra de
--     la clave, y la lista lista de nodos hijos del nodo actual y la
--     primera letra de la clave. El resultado se utiliza para llamar
--     de nuevo a la función inserta y se añade como hijo del nodo actual.
-- Por ejemplo,
-- λ> inserta  arbolTrieVacio ("ok",43)
-- NT "" [NT "o" [NT "k" [HT 43]]]
-- λ> inserta (NT "" [NT "o" [NT "k" [HT 43]]]) ("os",542)
-- NT "" [NT "o" [NT "s" [HT 542],NT "k" [HT 43]]]

inserta = undefined 

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función (insertaElemsEnArbol a cs), que reciba un
-- árbol Trie, a, y una lista, cs, de pares (clave, valor), y devuelva un
-- árbol con todos los elementos insertados. Por ejemplo, 
--    insertaElemsEnArbol arbolTrieVacio
--        [("IVAN",69712),("JULIA",62375),("JULIA",67321),("JUAN",68972)]
-- NT ""
-- [NT "J" [NT "U" [NT "A" [NT "N" [HT 68972]],NT "L" [NT "I" [NT "A" [HT 67321,HT 62375]]]]],
--  NT "I" [NT "V" [NT "A" [NT "N" [HT 69712]]]]]

insertaElemsEnArbol = undefined 

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la (consultaValor a cs), tal que reciba un árbol
-- Trie a y una Clave cs, y devuelva los valores asociados a ella. Si la
-- clave no está en el árbol o no tiene asociados valores, devolver la
-- lista vacía. Por ejemplo,
-- let a = NT "" [NT "J" [NT "U" [NT "A" [NT "N" [HT 68972]],NT "L" [NT "I" [NT "A" [HT 67321,HT 62375]]]]],NT "I" [NT "V" [NT "A" [NT "N" [HT 69712]]]]]
-- λ> consultaValor a "JUAN"
-- [68972]
-- λ> consultaValor a "JULIA"
-- [67321,62375]
-- λ> consultaValor a "JULIO"
-- []

consultaValor = undefined 

-- ---------------------------------------------------------------------

-- Fin