module EscuelaDeMagia(

    EscuelaDeMagia,
    fundarEscuela,
    estaVacia,
    registrar,
    magos,
    hechizosDe,
    leFaltanAprender,
    egresarUno,
    ensenar,
    howards,

    -- import as a services all of this things rigth now

) where

import Mago
import Pq1
import Map3
import Set1

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
    deriving Show

mago = crearM "Pablov"
pablov = aprender "Ptronus" (aprender "Filipendo" mago)
howards = EDM (addS "Patronus"(addS "Filipendo" emptyS)) (assocM "Pablov" pablov emptyM) (insertPQ pablov emptyPQ)

fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM hechizos map pq) = isEmptyPQ pq


-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar nom (EDM hechizos map pq) = 
    case lookupM nom map of
        (Just mago) -> (EDM hechizos map pq)
        Nothing     -> (EDM hechizos (assocM nom (crearM nom) map) (insertPQ (crearM nom) pq))



magos :: EscuelaDeMagia -> [Nombre]
magos (EDM hs map pq) = keys map


-- Propósito: Devuelve los hechizos que conoce un mago dado. 
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe nom (EDM hs map pq) = 
    case lookupM nom map of
        (Just mago) -> hechizos mago
        Nothing     -> emptyS

-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender nom (EDM hs map pq) =
    (sizeS hs) - (sizeS(hechizosDe nom  (EDM hs map pq) ))


-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM hs map pq) = 
    (
        findMinPQ pq,
        (EDM hs map (deleteMinPQ pq) )          
        )

-- Propósito: Enseña un hechizo a un mago existente, y si el hechizo no 
-- existe en la escuela es incorporado a la misma.

ensenar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
ensenar hech nom (EDM hs map pq) =
     EDM (addS hech hs) (ensenarM hech nom map) (ensenarP hech nom pq)

ensenarM :: Hechizo -> Nombre -> Map Nombre Mago -> Map Nombre Mago
ensenarM h nom map =
    assocM nom (aprender h (valor(lookupM nom map))) map

ensenarP :: Hechizo -> Nombre -> PriorityQueue Mago -> PriorityQueue Mago   
ensenarP h nom pq =
    if nombre (findMinPQ pq) == nom
        then insertPQ (aprender h (findMinPQ pq)) (deleteMinPQ pq)
        else insertPQ (findMinPQ pq) (deleteMinPQ pq)



valor :: Maybe a -> a
valor  (Just v) = v


