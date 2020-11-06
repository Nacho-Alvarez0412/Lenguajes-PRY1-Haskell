-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                           --
--                                                                                                                                           --
--                                                         TAREA PROGRAMADA # 1                                                              --
--                                                               HASKELLL                                                                    --
--  Ignacio Álvarez Barrantes                                                                                                                --
--  2019039643                                                                                                                               --
-----------------------------------------------------------------------------------------------------------------------------------------------

-- IMPORTS
import Data.Map.Internal 
import qualified Data.Map as Map
import TDAs (HypMap)
import Alineador
import System.IO
import Prelude hiding (filter, lookup, map, null)
import Data.List as List
import System.IO as SysIO
import System.Directory
import Data.Char as Char (ord)





type Estado = HypMap

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do
  mainloop (Map.fromList [])

-- Ciclo de ejecución:
--  1. Recibe un Estado
--  2. lee un comando
--  3. Ejecuta un comando que produce un nuevo Estado
--  4. Se invoca recursivamente con el nuevo Estado.

mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens = words inpStr
  let comando = tokens !! 0

  -- Check for command
  case comando of

    "load" -> do
      -- Get file name
      let nombreArchivo = (tokens !! 1)

      fileExists <- doesFileExist nombreArchivo

      if fileExists then do
        -- Create handle
        inh <- openFile nombreArchivo ReadMode
        -- New state
        nuevoEstado <- loadDiccionario inh estado
        hClose inh
        putStrLn (("Diccionario cargado ") ++ (show (length (Map.keys nuevoEstado))) ++ " palabras cargadas")
        mainloop nuevoEstado

      else do
        putStrLn ("Diccionario ingresado no existe")
        mainloop estado

    "show" -> do
      SysIO.putStrLn (List.unlines [(fst x) ++ " " ++ (List.intercalate "-" (snd x)) | x <- Map.toList estado])
      mainloop estado

    "ins" -> do
      let word = (tokens !! 1)
      let silabas = (tokens !! 2)
      let silabasSep = (words [if c == '-' then ' ' else c | c <- silabas])
      let nuevoEstado = addToken estado word silabasSep
      putStrLn (("Palabra ")++word++(" agregada"))
      mainloop nuevoEstado

    "save" -> do
      let nombreArchivo = (tokens !! 1)
      let mapFormatted = (List.map formatTuple (sort (toList estado)))
      outh <- openFile nombreArchivo WriteMode 
      descargar outh mapFormatted
      hClose outh
      mainloop estado

    "split" -> do 
      
      let cantidad = read (tokens !! 1) :: Int
      let separar = head [if c == 'n' then False else True | c <- (tokens !! 2)]
      let alinear = head [if c == 'n' then False else True | c <- (tokens !! 3)]
      let text = intercalate " " (List.drop 4 tokens)

      let texto = Alineador.separarYalinear cantidad separar alinear text estado
      let finalText = intercalate "\n" texto
      let normalizedText = normalizeText finalText
      putStrLn (normalizedText)
      mainloop estado

    "splitf" -> do 
      
      let cantidad = read (tokens !! 1) :: Int
      let separar = head [if c == 'n' then False else True | c <- (tokens !! 2)]
      let alinear = head [if c == 'n' then False else True | c <- (tokens !! 3)]

      let fileName = (tokens !! 4)

      fileExists <- doesFileExist fileName

      if fileExists then do

        handle <- openFile fileName ReadMode
        text <- hGetContents handle

        let texto = Alineador.separarYalinear cantidad separar alinear text estado
        let finalText = intercalate "\n" texto
        let normalizedText = normalizeText finalText
        putStrLn (normalizedText)
        
        let saveSpot = getSaveSpot tokens
        saveText saveSpot (unnormalizeText normalizedText)
        mainloop estado

      else  do
        putStrLn ("El archivo ingresado no existe")
        mainloop estado

      

    "exit" -> do
      putStrLn "Saliendo..."
    _ -> do
      putStrLn $ "Comando desconocido (" ++ comando ++ "): '" ++ inpStr ++ "'"
      mainloop estado

-- Recibe el estado actual, el token y sus silabas
-- Agrega un token nuevo al diccionario
addToken :: Estado -> String -> [String] -> Estado
addToken estado token silabas =
  -- Check for diccionary member
  if Map.member token estado
    then estado
    else Map.insert token silabas estado

-- Recibe el handle del archivo y el estado actual
-- Carga el diccionario a partir de un handle del archivo
loadDiccionario :: Handle -> Estado -> IO Estado
loadDiccionario inh estado = do
  ineof <- hIsEOF inh
  if ineof
    then return estado
    else do
      inpStr <- hGetLine inh
      let fileLine = words (inpStr)
      let silabas = fileLine !! 1
      let silabasSep = (words [if c == '-' then ' ' else c | c <- silabas])
      -- Insert new slot for diccionary
      let nuevoEstado = addToken estado (head fileLine) silabasSep
      loadDiccionario inh nuevoEstado

-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs


formatTuple :: (String,[String]) -> (String,String)
formatTuple tuple = (fst tuple,intercalate "-" (snd tuple))

getSaveSpot :: [String] -> String
getSaveSpot tokens =
  if List.length tokens == 5
    then ""
  else tokens !! 5

saveText :: String -> String -> IO()
saveText "" text = return ()
saveText path text = res where
  res = writeFile path text

-- Returns a string replacing the decomposed ascii for accents with the composed ascii for accents
normalizeText :: String -> String
normalizeText [] = []
normalizeText (x:xs)
    | Char.ord x == 9500 && Char.ord (List.head xs) == 237 = "á" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 8976 = "é" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 161 = "í" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 9474 = "ó" ++ (normalizeText (List.drop 1 xs))
    | Char.ord x == 9500 && Char.ord (List.head xs) == 9553 = "ú" ++ (normalizeText (List.drop 1 xs))
    | otherwise = [x] ++ normalizeText xs

-- Returns a string replacing the composed ascii for accents with the decomposed ascii for accents
unnormalizeText :: String -> String
unnormalizeText [] = []
unnormalizeText (x:xs)
    | x == 'á' = "\9500\237" ++ unnormalizeText xs
    | x == 'é' = "\9500\8976" ++ unnormalizeText xs
    | x == 'í' = "\9500\161" ++ unnormalizeText xs
    | x == 'ó' = "\9500\9474" ++ unnormalizeText xs
    | x == 'ú' = "\9500\9553" ++ unnormalizeText xs
    | otherwise = [x] ++ unnormalizeText xs
