-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Módulo auxiliar para leer ficheros usando un handler, de forma que evite el bloqueo de ficheros

module Source.Seguridad (
    leerSeguro
) where

import System.IO

{-
    Función para leer el contenido de un fichero de forma segura; evitamos asi el bloqueo de ficheros
    al usar un manejador, esto es:

    - abrimos el ficheros
    - obtenemos el contenido ==> http://aprendehaskell.es/content/EntradaSalida.html
    - forzamos la evaluación del contenido leido para evitar el cierre del fichero ===> https://stackoverflow.com/questions/28353159/haskell-getcontents-wait-for-eof
    - retornamos el contenido

    , de esta forma evitamos la lectura perezosa o "lazy" de Haskell que nos causaba problemas de lectura.
-}
leerSeguro :: FilePath -> IO String
leerSeguro fichero = do
    handle <- openFile fichero ReadMode 
    contenido <- hGetContents handle    
    length contenido `seq` hClose handle
    return contenido

--