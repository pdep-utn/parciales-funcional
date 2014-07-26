module Parcial.Power.Rangers where -- => Eso es para darle un nombre lindo al Modulo

import Data.Char -- => Esto hay que hacerlo para poder usar funciones como toUpper


--Punto 1

type Habilidad = String
type EsBueno = Bool -- ==> Lo mejor es modelarlo como un booleano
type Persona = ([Habilidad], EsBueno)

habilidades = fst
esBueno = snd

kimberly :: Persona
kimberly = (["acrobacia"], True)

type Color = String
type NivelPelea = Int
type PowerRanger = (Color, [Habilidad], NivelPelea)

colorRanger :: PowerRanger -> Color
colorRanger (color, _, _) = color

habilidadesRanger :: PowerRanger -> [Habilidad]
habilidadesRanger (_, habilidades, _) = habilidades

nivelPeleaRanger :: PowerRanger -> NivelPelea 
nivelPeleaRanger (_, _, nivelPelea) = nivelPelea

rangerRosa :: PowerRanger
rangerRosa = ("rosa", ["superAcrobacia"], 9)

rangerRojo :: PowerRanger
rangerRojo = ("rojo",["superCuida"],5)

rangers :: [PowerRanger]
rangers = [rangerRojo,rangerRosa] 

-- Punto 2

convertirEnPowerRanger :: Color -> Persona -> PowerRanger
convertirEnPowerRanger color (habilidades, _) = (color, potenciarHabilidades habilidades, nivelPeleaSegun habilidades)

potenciarHabilidades :: [Habilidad] -> [Habilidad]
potenciarHabilidades = map (("super" ++) . capitalizar)

capitalizar :: String -> String
capitalizar (primeraLetra:restoLetras) = toUpper primeraLetra : restoLetras

nivelPeleaSegun :: [Habilidad] -> Int
nivelPeleaSegun = length . concat


-- Punto 3
formarEquipoRanger :: [Color] -> [Persona] -> [PowerRanger]
formarEquipoRanger colores = zipWith convertirEnPowerRanger colores . filter esBueno


-- Punto 4
findOrElse :: (a -> Bool) -> a -> [a] -> a
findOrElse condicion valor lista
	| any condicion lista = find condicion lista
	| otherwise           = valor

findOrElse' :: (a -> Bool) -> a -> [a] -> a
findOrElse' condicion valor lista = head ( filter condicion lista ++ [valor] )

find :: (a -> Bool) -> [a] -> a	
find condicion = head . filter condicion

rangerLider :: [PowerRanger] -> PowerRanger
rangerLider rangers = findOrElse ((== "rojo") . colorRanger) (head rangers) rangers


-- Punto 5
maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy funcion lista = foldl1 (maxBy funcion) lista

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy funcion valor1 valor2
    | funcion valor1 > funcion valor2 = valor1
    | otherwise                       = valor2

rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso = maximumBy nivelPeleaRanger


-- Punto 6
rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso = (>5) . length . habilidadesRanger


-- Punto 7
alfa5 :: PowerRanger
alfa5 = ("metalico", ["repararCosas", "decir" ++ cycle "ay"], 0)

-- Consulta que funciona
--   *Parcial.Power.Rangers> rangerHabilidoso alfa5
--   False
--
--
-- Consulta que no funciona (no tenemos funciones que no anden con alfa5, pero podemos inventarla)
--   *Parcial.Power.Rangers> maximumBy (nivelPeleaSegun . habilidadesRanger) [alfa5, rangerRosa]
--   ....


-- Punto 8
type CantidadPelo = Int
type ChicaSuperPoderosa = (Color, CantidadPelo)

colorChicaSuperPoderosa = fst
cantidadPeloChicaSuperPoderosa = snd

burbuja :: ChicaSuperPoderosa
burbuja = ("celeste", 25000)

liderSegun :: (a -> Bool) -> [a] -> a
liderSegun condicion equipo = findOrElse condicion (head equipo) equipo

chicaSuperPoderosaLider :: [ChicaSuperPoderosa] -> ChicaSuperPoderosa
chicaSuperPoderosaLider = liderSegun ((== "rojo") . colorChicaSuperPoderosa)
-- Redefinimos rangerLider en funcion de liderSegun para no repetir la logica del findOrElse
-- rangerLider = liderSegun ((== "rojo") . colorRanger)
