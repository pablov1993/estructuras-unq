module Multiset(

    MultiSet
)where


import Map3

data MultiSet a = MS (Map a Int)
    deriving Show

--Propósito: denota un multiconjunto vacío.

emptyMS :: MultiSet a
emptyMS = MS emptyM

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.

addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS e (MS map) = MS(agregarAparicion e map)

agregarAparicion e map = 
    case lookupM e map of
        (Just v) -> assocM e (v + 1) map
        Nothing -> assocM e 1 map 

-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS map) = 
    case lookupM x map of
        (Just v) -> v
        Nothing -> 0

-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS map1) (MS map2) = MS (unirMS (keys map1) map1 map2)

unirMS [] map1 map2 = map2
unirMS (k:ks) map1 map2 = 
    case lookupM k map2 of
        (Just v) -> assocM k ((valor(lookupM k map2)) + v) (unirMS ks map1 map2)
        Nothing  -> assocM k (valor(lookupM k map1)) (unirMS ks map1 map2)



valor Nothing = error "no hay valor"
valor (Just v) = v


-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común. 

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MS map1) (MS map2) = MS (doIntersection (keys map1) map1 map2)

doIntersection :: Ord k => [k] -> Map k Int -> Map k Int -> Map k Int
doIntersection [] map1 map2 = emptyM
doIntersection (k:ks) map1 map2 = 
    let actualizar = doIntersection ks map1 map2 in
        case lookupM k map2 of
            (Just v) -> assocM k ((valor(lookupM k map1)) + v) actualizar
            Nothing -> actualizar

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.

multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
multiSetToList (MS map) = doList (keys map) map

doList :: Eq k => [k] -> Map k Int -> [(k, Int)]
doList [] map =[]
doList (k:ks) map = (k,(valor(lookupM k map))): (doList ks map)


-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.

ocurrencias :: String -> MultiSet Char
ocurrencias  [] = emptyMS
ocurrencias (s:ss) = addMS s (ocurrencias ss)



map1 :: Map String Int
map1 =
	assocM "fede" 1 $
	assocM "ale" 2 $
	assocM "cristian" 1 $
	emptyM

ms1 = MS map1

ms2 :: MultiSet String
ms2 = 
    addMS "fede"  $
    addMS "fede"  $
    addMS "ale"  $
    addMS "Pablo" $
    addMS "Pablo" $
    addMS "Pablo" $
    addMS "Ro" $
    addMS "Ro" $
    emptyMS