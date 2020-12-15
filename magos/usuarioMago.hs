import EscuelaDeMagia
import Mago
import Set1
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M  (logM + H logH))

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos escuela = todosLosHechizos (magos escuela) escuela

todosLosHechizos :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
todosLosHechizos [] esc = emptyS
todosLosHechizos (m:ms) esc = 
    unionS (hechizosDe m esc) (todosLosHechizos ms esc)

-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.

hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto esc = hayExperto (magos esc) esc

hayExperto :: [Nombre] -> EscuelaDeMagia -> Bool
hayExperto [] esc = False
hayExperto (m:ms) esc = (leFaltanAprender m esc) == 0 || hayExperto ms esc