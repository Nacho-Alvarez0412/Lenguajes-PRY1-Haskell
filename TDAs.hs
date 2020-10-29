-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
--                                                        TIPOS ABSTRACTOS DE DATOS                                                          --
-----------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------
module TDAs where

--imports 
import Data.Map as Map  hiding (sort,map,foldl)

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
                            ("presente",["pre","sen","te"]),
                            ("aquel",["a","quel"]),
                            ("el",["el"]),
                            ("pasado",["pa","sa","do"]),
                            ("Quien",["Quien"])]
