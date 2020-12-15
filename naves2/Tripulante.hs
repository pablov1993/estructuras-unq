module Tripulante (

    Tripulante,
    Nombre,
    Rango,
    crearT,
    sectoresT,
    rango,
    nombre,
    asignarS
)where

import SectorId
import Set1
type Nombre = String
type Rango = Int

data Tripulante = T Nombre Rango (Set SectorId)
    deriving Show

    
instance Eq Tripulante where
	(T nom rang set) == (T nom' rang' set') = rang == rang'

instance Ord Tripulante where
    (T nom rang set)<= (T nom' rang' set') = rang <= rang'

crearT :: Nombre -> Rango -> Tripulante
crearT nom rang = T nom rang emptyS


sectoresT :: Tripulante -> Set SectorId
sectoresT (T nom rang set) = set 

asignarS :: SectorId -> Tripulante -> Tripulante
asignarS  id (T nom rang ss) = (T nom rang (addS id ss))

nombre :: Tripulante -> Nombre
nombre (T nom rang ss) = nom

rango :: Tripulante -> Rango
rango (T nom rang ss) = rang

