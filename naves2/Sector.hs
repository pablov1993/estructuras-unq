module Sector(
    Sector,
    Componente,
    Barril,
    crearS,
    sectorId,
    componentesS,
    tripulantesS,
    agregarC,
    agregarT
)  where

import Tripulante
import SectorId
import Set1

data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

crearS :: SectorId -> Sector
crearS id = S id [] []

sectorId :: Sector -> SectorId
sectorId (S id cs ts) = id

componentesS :: Sector -> [Componente]
componentesS (S id cs ts) = cs

tripulantesS :: Sector -> Set Nombre
tripulantesS (S id cs ts) = armarSet ts

armarSet :: [Tripulante] -> Set Nombre
armarSet [] = emptyS
armarSet (x:xs) = addS (nombre x) (armarSet xs)


agregarC :: Componente -> Sector -> Sector
agregarC c (S id cs ts) = S id (c:cs) ts

agregarT :: Nombre -> Sector -> Sector
agregarT nom (S id cs ts) = 
    S id cs ((crearT nom 0):ts)







