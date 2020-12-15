


-- Propósito: Denota los tripulantes de la nave

-- triuplantes (S2 log S) 

tripulantes :: Nave -> Set Tripulante
tripulantes nave = todosLosTripulantes nave (sectores nave)


-- tripulantesDe O(log S)
-- union O(N log N)
-- todoLosTripulantes (S2 log S)
todosLosTripulantes :: Nave -> [Sector] -> Set Tripulante
todosLosTripulantes nave [] = emptyS
todosLosTripulantes nave (s:ss) = 
    unionS(tripulantesDe s nave) (todosLosTripulantes nave ss)

-- Propósito: Elimina al tripulante de la nave.
bajaDeTripulante :: Tripulante -> Nave -> Nave
bajaDeTripulante t n = armarNuevaNave t (sectores n) n

armarNuevaNave :: Tripulante -> [Sector] -> Nave -> Nave
armarNuevaNave t [] nave = naveVacia
armarNuevaNave t (s:ss) nave = 
    





