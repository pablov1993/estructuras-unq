module Nave(

    Nave,
    construir,
    ingresarT,
    sectoresAsignados,
    datosDeSector,
    tripulantesN,
    agregarASector,
    asignarASector

)where 

import MaxHeap
import Map3
import Tripulante 
import Sector
import SectorId
import Set1

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    deriving Show



construir :: [SectorId] -> Nave
construir xs = N (agregarSs xs) emptyM emptyH

agregarSs :: [SectorId] -> Map SectorId Sector
agregarSs [] = emptyM
agregarSs (s:ss) = assocM s (crearS s) (agregarSs ss) 


-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT nom rang (N mapS mapT mhTrip) = 
    (N mapS (assocM nom (crearT nom rang) mapT) (insertH (crearT nom rang) mhTrip))


-- Propósito: Devuelve los sectores asignados a un tripulante.

sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados nom (N mapS mapT mhTrip) = 
    sectoresT (valor (lookupM nom mapT))


valor :: Maybe a -> a
valor (Just valor) = valor


-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector id (N mapS mapT mhTrip) = 
    (tripulantesS (valor(lookupM id mapS)), componentesS (valor(lookupM id mapS)))


-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.

tripulantesN :: Nave -> [Tripulante]
tripulantesN (N mapS mapT mhTrip) = maxHeapToList mhTrip

maxHeapToList :: MaxHeap Tripulante -> [Tripulante]
maxHeapToList mht =
    if isEmptyH mht
        then []
        else (maxH mht): (maxHeapToList (deleteMaxH mht))


-- Propósito: Asigna una lista de componentes a un sector de la nave.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector comps id (N mapS mapT mhTrip) = 
    (N (agregarTodos comps id mapS) mapT mhTrip)

agregarTodos :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarTodos [] id map = emptyM
agregarTodos (c:cs) id map = 
    case lookupM id map of 
        (Just sector) -> assocM id (agregarC c sector) (agregarTodos cs id map)
        Nothing       -> assocM id (agregarC c (crearS id)) (agregarTodos cs id map)


-- Propósito: Asigna un sector a un tripulante.

asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector nom id (N mapS mapT mhTrip) = 
    (N mapS (asignarAlMap nom id mapT) (asignarHM nom id mhTrip))


asignarAlMap :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
asignarAlMap nom id map = assocM nom (asignarS id (valor(lookupM nom map))) map

asignarHM :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
asignarHM nom id mx =
    if nom == (nombre (maxH mx))
        then insertH (asignarS id (maxH mx)) (asignarHM nom id (deleteMaxH mx))
        else insertH (maxH mx) (asignarHM nom id (deleteMaxH mx))