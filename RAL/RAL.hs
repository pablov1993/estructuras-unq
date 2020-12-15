module RAL(

    RAList,
    emptyRAL
)where


import Map3
import Heap

data RAList a = MkR Int (Map Int a) (Heap a)
    deriving Show

ral = 
    MkR 2
    (assocM 1 "Chau"(assocM 0 "Hola" emptyM))
    (insertPQ "Chau"(insertPQ "Hola" emptyPQ ))

emptyRAL :: RAList a
emptyRAL  = MkR 0 emptyM emptyPQ

isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR pos map heap) = isEmptyPQ heap


lengthRAL :: RAList a -> Int
lengthRAL (MkR pos map heap) = length (keys map)

-- Propósito: devuelve el elemento en el índice dado.

get :: Int -> RAList a -> a
get index (MkR pos map heap) = 
   valor (lookupM index map)


valor :: Maybe a -> a
valor (Just valor) = valor

-- Propósito: devuelve el mínimo elemento de la lista.

minRAL :: Ord a => RAList a -> a
minRAL  (MkR pos map heap) = 
    findMinPQ heap 

-- Propósito: agrega un elemento al final de la lista.

add :: Ord a => a -> RAList a -> RAList a
add valor  (MkR pos map heap) = MkR (pos + 1) (assocM pos valor map) (insertPQ valor heap)

-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
elems :: Ord a => RAList a -> [a]
elems (MkR pos map heap) = armarLista heap

armarLista :: Ord a => Heap a -> [a]
armarLista heap = 
    if isEmptyPQ heap
        then []
        else (findMinPQ heap) : (armarLista (deleteMinPQ heap))

-- Propósito: elimina el último elemento de la lista.

remove :: Ord a => RAList a -> RAList a
remove (MkR pos map heap) = 
   MkR (pos - 1) (deleteM (pos - 1) map) (deleteElemPQ (valor(lookupM (pos -1 ) map)) heap)


deleteElemPQ :: Ord a => a -> Heap a -> Heap a
deleteElemPQ x  heap = 
    if findMinPQ heap == x
        then deleteMinPQ heap
        else insertPQ (findMinPQ heap) (deleteElemPQ x (deleteMinPQ heap))

-- Propósito: reemplaza el elemento en la posición dada.

set :: Ord a => Int -> a -> RAList a -> RAList a
set index x (MkR pos map heap) =
    MkR pos (assocM index x map) (insertPQ x (deleteElemPQ (valor (lookupM index map)) heap))


-- Propósito: agrega un elemento en la posición dada.

addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt index x (MkR pos map heap)  =
    MkR (pos + 1) (addToMap index x (keys map) map) (insertPQ x heap)

addToMap ::Ord a => Int -> a -> [Int] -> Map Int a -> Map Int a
addToMap index x [] map = emptyM
addToMap index x (k:ks) map =
    if k == index
        then assocM index x (assocM (index + 1) (valor (lookupM index map)) (addToMap index x ks map))
        else 
            if k > (index + 1)
                then assocM (k + 1) (valor (lookupM k map)) (addToMap index x ks map)
                else assocM k (valor (lookupM k map)) (addToMap index x ks map)


