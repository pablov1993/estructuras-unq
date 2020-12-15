module Organizador(

    Organizador,
    Checksum,
    Persona,
    nuevo,
    agregarPrograma,
    todosLosProgramas,
    autoresDe,
    programasDe,
    programaronJuntas,
    nroProgramasDePersona,
    orga
)where

import Set1
import Map3

type Checksum = String
type Persona = String

--type porProg = Map Checksum (Set Persona)
--type porPer = Map Persona (Set Checksum)

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
    deriving Show


orga = 
    MkO
        (assocM "React" (addS "Pablov" (addS "Colo" emptyS))(assocM "Angular" (addS "Pablov" emptyS) emptyM))
        (assocM "Colo" (addS "React" emptyS)(assocM "Pablov" (addS "React" (addS "Angular" emptyS)) emptyM))
--O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM 

-- Propósito: Agrega al organizador un programa con el Checksum indicado; 
-- el conjunto es el conjunto de personas autores de dicho programa

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO porProg porPer) check set = 
    MkO (assocM check set porProg) (agregarAPersona (setToList set) check porPer)

agregarAPersona :: [Checksum] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarAPersona [] check map = map
agregarAPersona (x:xs) check map = 
    assocM x (progDePer x check map) (agregarAPersona xs check map)

progDePer :: Persona -> Checksum -> Map Persona (Set Checksum) -> Set (Checksum)
progDePer p check map =
    case lookupM p map of
        (Just v) -> addS check v
        Nothing  -> addS check emptyS


-- Propósito: denota una lista con todos y cada uno de los códigos 
-- identificadores de programas del organizador.

todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO porProg porPer) = keys porProg

-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.

autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO porProg porPer) check = 
    valor (lookupM check porProg)


valor :: Maybe a -> a
valor (Just v) = v

-- Propósito: denota el conjunto de programas en los que participó una determinada persona.

programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO porProg porPer) persona = 
    valor (lookupM persona porPer)


-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son
-- autores de algún software en común.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas org per1 per2 = not (isEmptyS (intersection (programasDe org per1)(programasDe org per2)))



-- Propósito: dado un organizador y una persona, 
-- denota la cantidad de programas distintos en los que aparece.

nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona org per = sizeS (programasDe orga per)

