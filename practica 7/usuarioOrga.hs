
import Organizador
import Set1
import Map3
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos 
-- programas en las que las personas programaron juntas.

-- O()
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 orga =
    intersection (programasDe orga p1) (programasDe orga p2)


-- Propósito: denota verdadero si la persona indicada aparece como 
-- autor de todos los programas del organizador.

-- O(log P)
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker orga p = programoATodos (todosLosProgramas orga) (programasDe orga p)

-- O(log C)
programoATodos :: [Checksum] -> Set Checksum -> Bool
programoATodos [] set = True
programoATodos (x:xs) set = belongs x set && (programoATodos xs set)