-- PD: Práctica sobre acceso a fuentes de datos en ficheros.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- Alejandro Fernández Trigo
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

--
-- Esta práctica trata de asentar algunos de los primeros conceptos
-- introducidos en el tema de entrada y salida con ficheros.

 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
import System.Environment (getArgs)
import System.Directory
import Text.CSV
import Text.Printf
import Data.List
-- ---------------------------------------------------------------------

main0 :: IO ()
main0 = do
  putStrLn "Vamos con los ejercicios propuestos..."
  putStrLn "--------------------------------------"
  putStrLn "Comente la linea main=main0 y descomente las de ejercicios"
  putStrLn "posteriores para que su main vaya llamando a cada mainN"
  putStrLn "conforme vaya avanzando."

--main = main0

-- ---------------------------------------------------------------------
-- Ejercicio 1. Crear un programa que lea el fichero "lorem_ipsum.txt"
-- (descárguelo de http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt) 
-- y devuelva una tupla conteniendo:
--    * El número de párrafos
--    * El número de palabras por párrafo
--    * El número de apariciones de la letra 'e' por párrafo
-- 
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[41,60,16,24,28])
-- ---------------------------------------------------------------------

main1 :: IO()
main1 = leerBien "lorem_ipsum.txt"

-- 'readFile' sirve para cargar el fichero especificado en la variable texto
-- Para iterar por líneas usamos 'lines', para contar las palabras usamos 'words'
-- 'lines' devuelve una lista de filas (usando los saltos de linea como separación)
-- 'words' junta todo el String en una lista de palabras (omite los espacios y saltos)
leer :: FilePath -> IO ()
leer fichero = do
  texto <- readFile fichero

  let parrafos = lines texto
  let palabras = [length (words linea) | linea <- parrafos]
  let eVeces   = [length lista | linea <- parrafos, let lista = filter (=='e') linea]

  let resultado = (length parrafos, palabras, eVeces)
  -- 'show' es necesario para imprimir la tupla
  putStrLn (show resultado)

-- Esta primera versión no filtra las filas vacías!!
leerBien :: FilePath -> IO ()
leerBien fichero = do
  texto <- readFile fichero

  let parrafos = [linea | linea <- lines texto, length linea > 0]
  let palabras = [length (words linea) | linea <- parrafos]
  let eVeces   = [length lista | linea <- parrafos, let lista = filter (=='e') linea]

  let resultado = (length parrafos, palabras, eVeces)
  putStrLn (show resultado)

--main = main1

-- ---------------------------------------------------------------------
-- Ejercicio 2. Adaptar el ejercicio anterior para que podamos
-- pasarle al main como argumento el nombre del archivo a leer
-- (Descargue los ficheros: 
-- http://www.cs.us.es/cursos/pd/ejercicios/lorem_ipsum.txt
-- http://www.cs.us.es/cursos/pd/ejercicios/otro.txt
-- )
-- Si no recibimos archivo, debemos tratar el del ejercicio 1
-- 
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica7.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
-- ---------------------------------------------------------------------

-- 'getArgs' permite leer desde teclado
main2 :: IO ()
main2 = do
  fichero <- getArgs 
  if length fichero > 0 then do
    -- Hay que usar 'head' porque 'getArgs' devuelve una lista de elementos
    leerBien (head fichero)
  else do
    putStrLn "Debe indicar un fichero"
    main1
  
--main = main2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Adaptar el ejercicio anterior para que trate el posible
-- error de lectura del fichero, como hemos visto en el tema.
--
--    $ runhaskell Practica7.2.hs
--    (5,[105,158,46,64,52],[79,108,38,43,30])
--
--    $ runhaskell Practica7.2.hs otro.txt
--    (8,[86,89,65,103,97,100,95,112],[62,57,50,69,63,60,69,65])
--
--    $ runhaskell Practica7.2.hs inexistente.txt
--    inexistente.txt: openFile: does not exist (No such file or directory)
--    "El fichero no existe"
-- ---------------------------------------------------------------------

-- Usamos 'doesFileExist'
main3 :: IO ()
main3 = do  
  fichero <- getArgs
  if length fichero > 0 then do
    -- Hay que usar 'head' porque 'getArgs' devuelve una lista de elementos
    existe <- doesFileExist (head fichero)
    if existe then do
      leerBien (head fichero)
    else do
      putStrLn "El fichero no existe"
  else do
    putStrLn "Debe indicar un fichero"
  return()

--main = main3

-- ---------------------------------------------------------------------
-- Ejercicio 4. Procesar el archivo CSV "clima.csv" 
-- descárgalo de http://www.cs.us.es/cursos/pd/ejercicios/clima.csv) 
-- y devolver los nombres de sus atributos.
-- Por ejemplo:
--    ghci> main
--    Cielo
--    Temperatura
--    Humedad
--    Viento
--    JugarTenis
-- ---------------------------------------------------------------------

main4 :: IO ()
main4 = undefined

--main = main4

-- ---------------------------------------------------------------------
-- Ejercicio 5. Procesar el archivo CSV "clima.csv" 
-- (descárgalo de http://www.cs.us.es/cursos/pd/ejercicios/clima.csv) 
-- y devolver la frecuencia de cada atributo.
--
-- Salida esperada:
--
-- Cielo
-- -----
-- Soleado: 0.357
-- Nublado: 0.286
-- Lluvia: 0.357
-- Temperatura
-- -----------
-- Baja: 0.286
-- Alta: 0.286
-- Suave: 0.429
-- Humedad
-- -------
-- Normal: 0.500
-- Alta: 0.500
-- Viento
-- ------
-- Debil: 0.571
-- Fuerte: 0.429
-- JugarTenis
-- ----------
-- Si: 0.643
-- No: 0.357
-- ---------------------------------------------------------------------

main5 :: IO ()
main5 = undefined

-- main = main5

-- Fin