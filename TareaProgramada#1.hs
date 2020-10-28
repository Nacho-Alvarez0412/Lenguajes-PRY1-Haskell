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
import Data.Map as Map  hiding (sort,map,foldl)
import Data.Maybe as Maybe
import Data.Char
import Data.List as List
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

-- Mapeado de Separaciones Correctas
type HypMap = Map.Map String [String]

-- Ejemplo de Mapa creado 
enHyp :: HypMap
enHyp = Map.fromList [ ("controla",["con","tro","la"]), 
                            ("futuro",["fu","tu","ro"]),
                            ("presente",["pre","sen","te"])]


-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                            FUNCIONES                                                                      --
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------




------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------ A) string2Line --------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
--E: Recibe un string
--S: Una Line
--D: Dado un String retorna un line formado por el string (El string no contiene palabras separadas)

string2Line :: String -> Line
string2Line texto = linea where
    palabras = words texto
    linea = map Word palabras


-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- B) line2String --------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Una linea
--S: Un String
--D: Dada una linea retorna un string formado por la linea (Debe haber espacios entre cada palabra)

line2String :: Line -> String
line2String linea = result where 
    textoArray = map getString linea
    dirtytext = intercalate " " textoArray
    result = cleanText dirtytext

-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- C) tokenLength --------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un Token
--S: Un Int
--D: Dado un Token determinar el largo 
tokenLength :: Token -> Int
tokenLength token = largo where
    tokenString = getString token
    palabra = blankChecker tokenString
    largo = length palabra

-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- D) lineLength  --------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Una line
--S: Un Int
--D: Dado una line determinar el largo 

lineLength :: Line -> Int
lineLength linea = largo where
    texto = line2String linea
    largo = length texto


-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- E) breakLine  ---------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un Int y una Linea
--S: Una tupla de Lineas
--D: Dado un limite por linea dividir la linea en 2 para que no se exceda dicho limite

breakLine :: Int -> Line -> (Line,Line)
breakLine limite linea = lineas where
    lineas = breakLineAux limite linea []


-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- F) mergers  -----------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un Lista de Strings
--S: Un Lista de tuplas de Strings
--D: Dada una lista con diversas tiras y retorne un lista con las posibles combinaciones en orden de dichas tiras 

mergers :: [String] -> [(String,String)]
mergers lista = 
    if length lista == 1 || length lista == 0
        then []
    else mergerCleaner (mergersAux lista 1)

-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- g) hyphenate   --------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un HypMap y un Token 
--S: Una Lista de Tuples de Tokens
--D: Dado un mapa y un token, retorna las diversas formas en las que ese Token puede ser separado

hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate map palabra = res where
    strPalabra = getString palabra
    cleanPalabra = extractPunctuation strPalabra
    division = Map.lookup (fst cleanPalabra) map
    strWord = maybe2StringArray division
    finalWord = addPunctuation strWord (snd cleanPalabra)
    res = hyphenateAux finalWord

-------------------------------------------------------------- Funciones Auxiliares --------------------------------------------------------------

--E: Una Token
--S: Un String
--D: Dado un Token retorna su contenido en tipo String (Word -> palabra | Blank -> " " | HypWord -> palabra-)

getString :: Token -> String
getString (Word texto) = texto
getString (Blank) = ""
getString (HypWord texto) = texto++"-"

--E: Un String
--S: Un String
--D: Dado un String limpia espacios al inicio y al final

cleanText :: String -> String
cleanText texto =
    if (head texto) == ' '
        then cleanText (List.drop 1 texto)
    else if (last texto) == ' ' 
        then cleanText (init texto)
    else texto

--E: Un String
--S: Un String
--D: Transforma el Blank en un espacio

blankChecker :: String -> String
blankChecker texto =
    if texto == ""
        then " "
    else texto  


--E: Un Int y una Linea
--S: Una tupla de Lineas
--D: Dado un limite por linea dividir la linea en 2 para que no se exceda dicho limite

breakLineAux :: Int -> Line -> Line -> (Line,Line)
breakLineAux limite linea respuesta =
    if (List.null linea)
        then (respuesta,linea)
    else if (limite - tokenLength (head linea) >= 0)
        then breakLineAux (limite - tokenLength (head linea)) (List.drop 1 linea) (respuesta ++ [head linea])
    else (respuesta,linea)

--E: Un Lista de Strings, un Int y un Lista de tuplas de String
--S: una lista de tuplas de lista de string
--D: Dada una lista con diversas tiras y retorne un lista con las posibles combinaciones en orden de dichas tiras

mergersAux :: [String] -> Int -> [([String],[String])]
mergersAux lista itr =
    if itr == (length lista)
        then []
    else [(List.splitAt itr lista)] ++ mergersAux lista (itr+1)

--E: una lista de tuplas de lista de string
--S: una lista de tuplas de String
--D: Arregla el formato de la lsita pasada

mergerCleaner :: [([String],[String])] -> [(String,String)]
mergerCleaner [] = []
mergerCleaner lista = [(intercalate "" (fst (head lista)) , intercalate "" (snd (head lista)))] ++ mergerCleaner (List.drop 1 lista)

--E: Un Maybe [String]
--S: Un array de string
--D: Dado un Maybe [String] verifica su valor y lo trasnforma

maybe2StringArray :: Maybe [String] -> [String]
maybe2StringArray divWord  =
    if Maybe.isJust divWord
        then Maybe.fromJust divWord
    else []

--E: Una lista de tuples de Strings
--S: Una lista de Tuplas de Tokens [(Token,Token)]
--D: Dado el total de combinaciones posibles las transforma a Tokens

string2Token :: [(String,String)] -> [(Token,Token)]
string2Token array =
    if array == []
        then []
    else [ ( HypWord (fst (head array)), Word (snd (head array)) ) ] ++ string2Token (List.drop 1 array)

--E: Un Maybe [String]
--S: Una lista de Tuplas de Tokens [(Token,Token)]
--D: Dado un Array de string separados los acomoda por medio del merge y lo convierte en Tokens

hyphenateAux :: [String] -> [(Token,Token)]
hyphenateAux divWord = res where
    combinaciones = mergers divWord
    res = string2Token combinaciones

--E: String
--S: Tuple de Strings
--D: Dado un string verifica y extrae el signo de puntuación en caso de tenerlo

extractPunctuation :: String -> (String,String)
extractPunctuation palabra =
    if (List.find (==(last palabra)) puntuaciones ) /= Nothing
        then (init palabra,[last palabra])
    else (palabra,"")

--E: Un Maybe [String]
--S: Una lista de Tuplas de Tokens [(Token,Token)]
--D: Dado un Array de string separados los acomoda por medio del merge y lo convierte en Tokens

puntuaciones :: [Char]
puntuaciones = ['!','?',',',':',';','.']


addPunctuation :: [String] -> String -> [String]
addPunctuation word punctuation = res where
    final = last word
    frstHalf = init word
    res = frstHalf ++ ([final++punctuation])

