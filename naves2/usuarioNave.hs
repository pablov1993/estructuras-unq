
import Nave
import Tripulante
import Set1
import Map3
import SectorId
import Sector


-- O (N2 log N)
sectores :: Nave -> Set SectorId
sectores nave = calcularSectores nave (tripulantesN nave)



-- unionS O(N log N)
-- sectoreAsignados O(log M)
-- O (N2 log N)
calcularSectores :: Nave -> [Tripulante] -> Set SectorId
calcularSectores nave [] = emptyS
calcularSectores nave (t:ts) = unionS (sectoresAsignados (nombre t) nave)(calcularSectores nave ts)



-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- O (T log T)
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados nave = tsSinSectores nave (tripulantesN nave)


-- sectoresAsignados O(T log T)
-- sizeS 
tsSinSectores :: Nave -> [Tripulante] -> [Tripulante]
tsSinSectores nave [] = []
tsSinSectores nave (t:ts) = 
    if sizeS (sectoresAsignados (nombre t) nave) == 0
        then t : (tsSinSectores nave ts)
        else tsSinSectores nave ts


-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.

barriles :: Nave -> [Barril]
barriles nave = todosLosBarriles (accederASectores nave (tripulantesN nave)) nave

accederASectores :: Nave -> [Tripulante] -> Set SectorId
accederASectores nave [] = emptyS
accederASectores nave (t:ts) = 
    unionS (sectoresAsignados (nombre t) nave) (accederASectores nave ts)

todosLosBarriles :: [SectorId] -> Nave -> [Componente]
todosLosBarriles [] nave = []
todosLosBarriles (s:ss) nave =
    snd (datosDeSector s nave) ++ (todosLosBarriles ss nave)


componentesBarril :: [Componente] -> [Barril]





