module Alineador where

-- Imports
import Prelude hiding (null, lookup, map, filter)
import Data.Maybe as Maybe
import Data.Char
import Data.List as List
import System.IO
import Data.Map as Map  hiding (sort,map,foldl)

import TDAs


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
    cleanPalabra = extractPunctuation strPalabra ""
    division = Map.lookup (fst cleanPalabra) map
    strWord = maybe2StringArray division (fst cleanPalabra)
    finalWord = addPunctuation strWord (snd cleanPalabra)
    check = hyphenateAux finalWord
    inv = hyphenChecker check (head(finalWord))
    res = reverse inv


-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- h) lineBreaks   -------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un HypMap y un Int y una Linea
--S: Una Lista de Tuples de Lineas
--D: Dado un mapa y un limite y una linea se divide la linea de todas formas posibles donde la primera linea no supere el limite dado

lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks map limit line = res where
    lstCombinations = hyphenate map (last line)
    lineCombinations = (getLineCombinations (init line) lstCombinations) ++ [line]
    dirtyCombs = lineBreaksAux limit lineCombinations
    res = cleanLineBreaks dirtyCombs

-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- i) insertBlanks    ----------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Una linea y un Int
--S: Una Linea
--D: Dada una linea y un numero de espacios en blanco inserta los espacios en blanco entre cada palabra

insertBlanks :: Int ->Line -> Line
insertBlanks limit [] = []
insertBlanks limit linea = 
    if List.length linea < 2
        then linea
    else insertBlanksAux limit linea []   


-------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------- j) separarYalinear    ----------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
--E: Un String y un Int
--S: Una Lista de Strings
--D: Dada una linea y un limite devuelve una lista de tiras de caracteres que no sean más largas que el tamaño especificado

separarYalinear :: Int -> Bool -> Bool -> String -> HypMap-> [String]
separarYalinear limite separar ajustar texto map = res where
    linea = string2Line texto
    divLines = divideLinesInText limite linea separar map
    cleanLines =  cleanFromBlanks divLines
    adjustedLines = adjustLines limite cleanLines ajustar
    fixedText = lines2String adjustedLines
    res = fixedText



   
-------------------------------------------------------------- Funciones Auxiliares --------------------------------------------------------------

--E: Un Token
--S: Un Bool
--D: Dado un Token indica si es HypWord

isHypWord :: Token -> Bool
isHypWord (HypWord x)  = True
isHypWord x = False

--E: Un Token
--S: Un Bool
--D: Dado un Token indica si es Blank

isBlank :: Token -> Bool
isBlank Blank = True
isBlank x = False

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
        then cleanText (tail texto)
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
    else if (limite - lineLength (respuesta++[head linea]) >= 0)
        then breakLineAux limite (tail linea) (respuesta ++ [head linea])
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
mergerCleaner lista = [(intercalate "" (fst (head lista)) , intercalate "" (snd (head lista)))] ++ mergerCleaner (tail lista)

--E: Un Maybe [String]
--S: Un array de string
--D: Dado un Maybe [String] verifica su valor y lo trasnforma

maybe2StringArray :: Maybe [String] -> String -> [String]
maybe2StringArray divWord word  =
    if Maybe.isJust divWord
        then Maybe.fromJust divWord
    else [word]

--E: Una lista de tuples de Strings
--S: Una lista de Tuplas de Tokens [(Token,Token)]
--D: Dado el total de combinaciones posibles las transforma a Tokens

string2Token :: [(String,String)] -> [(Token,Token)]
string2Token array =
    if array == []
        then []
    else [ ( HypWord (fst (head array)), Word (snd (head array)) ) ] ++ string2Token (tail array)

--E: Un Maybe [String]
--S: Una lista de Tuplas de Tokens [(Token,Token)]
--D: Dado un Array de string separados los acomoda por medio del merge y lo convierte en Tokens

hyphenateAux :: [String] -> [(Token,Token)]
hyphenateAux divWord = res where
    combinaciones = mergers divWord
    res = string2Token combinaciones

--E: 2 Strings
--S: Tuple de Strings
--D: Dado un string verifica y extrae el signo de puntuación en caso de tenerlo

extractPunctuation :: String -> String -> (String,String)
extractPunctuation palabra punctuation =
    if (List.find (==(last palabra)) puntuaciones ) /= Nothing
        then extractPunctuation (init palabra) (punctuation ++[last palabra])
    else (palabra,punctuation)

--E: 
--S: Una lista de chars
--D: Retorna una colección de signos de puntuación

puntuaciones :: [Char]
puntuaciones = ['!','?',',',':',';','.']

--E: Una lista de Strings y 1 String
--S: Una lista de Strings
--D: Retorna la lista de string más el signo de puntuación agregado

addPunctuation :: [String] -> String -> [String]
addPunctuation word punctuation = res where
    final = last word
    frstHalf = init word
    res = frstHalf ++ ([final++punctuation])

--E: Una Linea y Un Arry de tuples de Token 
--S: Una Lista de Lineas
--D: Dada una Linea y un array con un token separado, retorna un array con las posibles separaciones que puede tener dicho Token

getLineCombinations :: Line -> [(Token,Token)] -> [Line]
getLineCombinations linea [] = []
getLineCombinations linea tokens = [linea ++ [fst (head tokens)] ++ [snd (head tokens)]] ++ getLineCombinations linea (tail tokens)

--E: 1 Int y un Array de Lineas
--S: Una lista de tuples de lineas
--D: Crea todas las combinaciones posibles de quiebres de linea para cada una de las lineas dadas

lineBreaksAux :: Int -> [Line] -> [(Line,Line)]
lineBreaksAux limit [] = []
lineBreaksAux limit lineas = [breakLine limit (head lineas)] ++ lineBreaksAux limit (tail lineas)

--E: Una lista de tuples de lineas
--S: Una lista de tuples de lineas
--D: Limpia elo resultado de todas aquellas combinaciones no válidas

cleanLineBreaks :: [(Line,Line)] -> [(Line,Line)]
cleanLineBreaks combinaciones = 
    if List.null combinaciones 
        then []
    else if List.length (fst (head combinaciones)) < 2
        then combinaciones
    else if isHypWord (last (init (fst (head combinaciones)))) -- Selecciona el antepenultimo Token del primer Tuple
        then cleanLineBreaks (tail combinaciones)
    else if isHypWord (head (snd(head combinaciones)))
        then cleanLineBreaks (tail combinaciones)
    else [(head combinaciones)] ++ cleanLineBreaks (tail combinaciones)
    


--E: Una linea y un Int
--S: Una Linea
--D: Dada una linea y un numero de espacios en blanco inserta los espacios en blanco entre cada palabra

insertBlanksAux :: Int -> Line -> Line -> Line
insertBlanksAux limite linea res = 
   if limite == 0
       then res ++ linea
    else if List.length linea == 1
        then insertBlanksAux limite (res ++ linea) []
    else if isBlank(head linea)
        then insertBlanksAux limite (tail linea) (res ++ [Blank])
    else insertBlanksAux (limite-1) (tail linea) (res++([(head linea)]++[Blank]))

--E: Una linea Un HypMap y un limite
--S: Una Lista de lineas 
--D: Dada una linea un diccionario y un limite te separa todo el texto en lineas menores al numero solicitado

divideLinesInText :: Int -> Line -> Bool -> HypMap -> [Line]
divideLinesInText limit [] x map = []
divideLinesInText limit line True map = divideLinesInTextAux limit line map [] []
divideLinesInText limit line False map =  [fst(breakLine limit line)] ++ divideLinesInText limit (snd(breakLine limit line)) False map

--E: Int, 2 Line, HypMap, Array de Lineas
--S: Array de Lineas
--D: Dada un mapa un limite y una linea te separab eltexto para calzar la linea

divideLinesInTextAux :: Int -> Line -> HypMap -> Line -> [Line] -> [Line]
divideLinesInTextAux limite [] map temp res = (res ++ [temp])
divideLinesInTextAux limite linea map temp res =
    if limite - lineLength (temp ++ [head linea]) >= 0
        then divideLinesInTextAux limite (tail linea) map (temp ++ [head linea]) res
    else divideLinesInTextAux limite (snd(head(lineBreaks map limite (temp ++ [head linea])))++(tail linea)) map [] (res++[fst(head(lineBreaks map limite (temp ++ [head linea])))])
-- (snd(head(lineBreaks enHyp limite (temp ++ [head linea]))) -> Es el elemento que sobra al aplicar lineBreaks
-- (res++[fst(head(lineBreaks enHyp limite (temp ++ [head linea])))]) -> Es el primer elemento al aplicar lineBreaks


--E: Int, 2 Line, HypMap, Array de Lineas
--S: Array de Lineas
--D: Dada un mapa un limite y una linea te separab eltexto para calzar la linea

adjustLines :: Int -> [Line] -> Bool -> [Line]
adjustLines limite lineas False = lineas
adjustLines limite [] x = []
adjustLines limite lineas True =
    if lineLength (head lineas) < limite
        then [insertBlanks (limite - lineLength (head lineas)) (head lineas)] ++ adjustLines limite (tail lineas) True
    else [(head lineas)] ++ adjustLines limite (tail lineas) True

--E: Un Array de Lineas
--S: Un Array de String
--D: Toma una Linea y la convierte en String

lines2String :: [Line] -> [String]
lines2String [] = [] 
lines2String lineas = [line2String (head lineas)] ++ lines2String (tail lineas)


hyphenChecker :: [(Token,Token)] -> String -> [(Token,Token)]
hyphenChecker finalWrd provitional = 
    if length finalWrd == 0
        then [(Blank,Word provitional)]
    else finalWrd

cleanFromBlanks :: [Line] -> [Line]
cleanFromBlanks lines = map cleanFromBlanksAux lines

cleanFromBlanksAux :: Line -> Line
cleanFromBlanksAux linea =
    if isBlank (last linea)
        then cleanFromBlanksAux (init linea)
    else linea





