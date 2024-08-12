-- Alejandro Fernández Trigo & Mario García Gonzalez
-- Programación Declarativa 2023/2024

-- Definición de funciones para elaborar un ranking de puntuaciones.

module Source.Ranking (
    ranking,
) where

-- Librerias importadas
import Data.Array
import Data.List
import Data.Ord
import Source.Tipos
import Source.Tablero
import qualified Data.Map as Map

{-
    Función que lee el fichero de puntación, parsea cada línea, y devuelve un ranking de 
    todos los jugadores con su máxima puntuación usando un tipo Map.

    Usa funciones de orden superior y librerias adicionales
-}
ranking :: FilePath -> IO ()
ranking fichero = do
    puntuaciones <- leerRanking fichero

    -- Genera la lista de puntuaciones
    let tabla = crearRanking puntuaciones

    putStrLn "_PUNTUACIÓN DE JUGADORES_"
    -- Es necesario una lambda para recuperar las tuplas de tipo (nombre,puntuacion) pero ordenando el contenido en base
    -- a la puntuación (descendente), obtenida como el segundo argumento de la tupla. sortBy permite ordenarlo y admite 
    -- como parámetros el criterio 
    mapM_ (\(nombre, puntuacion) -> putStrLn $ nombre ++ ": " ++ show puntuacion) (sortBy (comparing (Down . snd)) tabla)

{-
    Función auxiliar para leer el fichero de puntuación. Llama a otra para parsear cada línea.
    El resultado tiene el formato ["Jugador", "Puntuación"]

    Usa lectura de ficheros y funciones de orden superior
-}
leerRanking :: FilePath -> IO [(String, Int)]
leerRanking fichero = do
    contenido <- readFile fichero
    return $ map parseaPuntuacion (lines contenido)

{-
    Función auxiliar para parsear cada línea del fichero.
    El resultado tiene el formato "Jugador", "Puntuación"
-}
parseaPuntuacion :: String -> (String, Int)
parseaPuntuacion linea = (nombre, read puntuacion)
  where
    nombre = words linea !! 1
    puntuacion = words linea !! 3

{-
    Función auxiliar para dado una lista de formato puntación (jugador, puntación),
    generar un mapa (usando la libreria Map de la asignatura). 

    Mediante fromListWith recoge todos las puntuaciones del mismo jugador y se queda con la mayor (max).

    Usa librerias adicionales
-}
crearRanking :: [(String, Int)] -> [(String, Int)]
crearRanking puntuaciones = Map.toList (mapa)
    where mapa = Map.fromListWith max puntuaciones

--