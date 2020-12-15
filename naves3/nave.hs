module Nave(
    Nave,
    Tripulante,
    Sector
)where

import Heap
import Map3
import Set1

type Rango = Int
type Nombre = String

data Tripulante = T Nombre Rango Sector
    deriving (Show)

instance Eq Tripulante where
	(T nom rang set) == (T nom' rang' set') = rang == rang'

instance Ord Tripulante where
    (T nom rang set)<= (T nom' rang' set') = rang <= rang'

data Sector = S String [Tripulante]
    deriving (Show,Eq)

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
    deriving Show


-- Propósito: Crea una nave con todos esos sectores sin tripulantes.

-- O (S log S)
naveVacia :: [Sector] -> Nave
naveVacia ss = MkN (agregarSectores ss) emptyH ((head ss),0)


-- O (S log S)
agregarSectores :: [Sector] -> Map Sector (Set Tripulante)
agregarSectores [] = emptyM
agregarSectores (s:ss) =  assocM s emptyS (agregarSectores ss)



-- Propósito: Obtiene los tripulantes de un sector.

tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN map hep sec) = 
    case lookupM s map of 
        (Just ts) -> ts
        Nothing -> error "No existe el sector"

-- Propósito: Denota los sectores de la nave
sectores :: Nave -> [Sector]
sectores (MkN map heap sec) = keys map


-- Propósito: Denota el tripulante con mayor rango.

conMayorRango :: Nave -> Tripulante
conMayorRango (MkN map hep sec) = findMin hep


-- Propósito: Denota el sector de la nave con más tripulantes.
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN map hep sec) =  fst sec

-- Propósito: Denota el conjunto de tripulantes con dicho rango.
-- (T log T)
conRango :: Rango -> Nave -> Set Tripulante
conRango rango (MkN map hep sec) = todosLosT rango hep

-- (T log T)
todosLosT :: Rango -> Heap Tripulante -> Set Tripulante
todosLosT rang hep = emptyS
todosLosT rang hep = 
    if (rango(findMin hep)) == rang
        then addS (findMin hep) (todosLosT rang (deleteMin hep))
        else todosLosT rang (deleteMin hep)




-- Propósito: Devuelve el sector en el que se encuentra un tripulante. 

sectorDe :: Tripulante -> Nave -> Sector
sectorDe  t (MkN map hep sec) = buscarSector t (keys map) map


-- (s log s log t)
buscarSector :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Sector
--buscarSector t [] map = Nothing
buscarSector t (x:xs) map = 
    if triuplanteEnSector t x map
        then x
        else buscarSector t xs map

-- (log S log T)
triuplanteEnSector :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Bool
triuplanteEnSector t s map =
    belongs t (valor(lookupM s map))


-- Propósito: Agrega un tripulante a ese sector de la nave.

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t s (MkN map hep sec) = 
    MkN (agregarAlMap t s map) (insertH t hep) (actualizarSec s sec (MkN map hep sec))


agregarAlMap :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarAlMap t s map = 
    assocM s (addS t (valor(lookupM s map))) map

actualizarSec :: Sector -> (Sector, Int)  -> Nave -> (Sector, Int)
actualizarSec s sec (MkN map hep sec') = 
    if (sizeS (valor(lookupM s map)) + 1) > (sizeS (valor(lookupM (fst sec) map)))
        then (s,(sizeS (valor(lookupM s map)) + 1))
        else sec'
-----------------------------------------
-- Extras para que funcione


rango :: Tripulante -> Rango
rango (T n r s) = r

valor :: Maybe a -> a
valor (Just v) = v