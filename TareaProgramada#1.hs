-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                           --
--                                                                                                                                           --
--                                                         TAREA PROGRAMADA # 1                                                              --
--                                                               HASKELLL                                                                    --
--  Ignacio Álvarez Barrantes                                                                                                                --
--  2019039643                                                                                                                               --
-----------------------------------------------------------------------------------------------------------------------------------------------

-- Imports
import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List 
import System.IO

-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                        TIPOS ABSTRACTOS DE DATOS                                                          --
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------

-- Token 
data Token = Word String |
             Blank |
             HypWord String 
             deriving (Show,Eq);

-- Line
type Line = [Token]


-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                            FUNCIONES                                                                      --
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------

-- A) string2Line
--
--E: Recibe un string
--S: Una Line
--D: Dado un String retorna un line formado por el string (El string no contiene palabras separadas)

string2Line :: String -> Line
string2Line texto = linea where
    palabras = words texto
    linea = map Word palabras

-- B) line2String
--
--E: Una linea
--S: Un String
--D: Dada una linea retorna un string formado por la linea (Debe haber espacios entre cada palabra)

line2String :: Line -> String
line2String linea = result where 
    textoArray = map getString linea
    dirtytext = intercalate " " textoArray
    result = cleanText dirtytext


------------------------------------------------------------------------------------- Funciones Auxiliares -------------------------------------------------------------------------------------

--E: Una Token
--S: Un String
--D: Dado un Token retorna su contenido en tipo String (Word -> palabra | Blank -> " " | HypWord -> palabra-)
getString :: Token -> String
getString (Word texto) = texto
getString (Blank) = ""
getString (HypWord texto) = texto++"-"

--E: Una String
--S: Un String
--D: Dado un String limpia espacios al inicio y al final

cleanText :: String -> String
cleanText texto =
    if (head texto) == ' '
        then cleanText (Data.List.drop 1 texto)
    else if (last texto) == ' ' 
        then cleanText (init texto)
    else texto

