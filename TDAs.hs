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

-- Mapeado de Separaciones Correctasla
type HypMap = Map.Map String [String]

-- Ejemplo de Mapa creado 
enHyp :: HypMap
enHyp = Map.fromList [("elementalmente",["e","le","men","tal","men","te"]),("aquel",["a","quel"]),("asi",["a","si"]),("bestias",["bes","tias"]),("celebro",["ce","le","bro"]),("controla",["con","tro","la"]),("demasiado",["de","ma","sia","do"]),("dormir",["dor","mir"]),("el",["el"]),("enflaquece",["en","fla","que","ce"]),("fermosura",["fer","mo","su","ra"]),("futuro",["fu","tu","ro"]),("hace",["ha","ce"]),("hicieron",["hi","cie","ron"]),("hombres",["hom","bres"]),("leer",["le","er"]),("manera",["ma","ne","ra"]),("mucho",["mu","cho"]),("para",["pa","ra"]),("pasado",["pa","sa","do"]),("pero",["pe","ro"]),("poco",["po","co"]),("presente",["pre","sen","te"]),("quejo",["que","jo"]),("quien",["quien"]),("razon",["ra","zon"]),("seco",["se","co"]),("sienten",["sien","ten"]),("sino",["si","no"]),("sinrazon",["sin","ra","zon"]),("tristezas",["tris","te","zas"]),("vuelven",["vuel","ven"]),("vuestra",["vues","tra"])]
