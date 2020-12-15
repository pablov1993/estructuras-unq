module Pq1 (
    PriorityQueue,
    emptyPQ,
    isEmptyPQ,
    insertPQ,
    findMinPQ,
    deleteMinPQ
)where


data PriorityQueue a = PQ [a]
    deriving Show

-- Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = PQ []


-- Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ pq)  = null pq

-- Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs)  = (PQ (x:xs))


--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.

findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = minimum xs


--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = (PQ (sinMinimo (minimum xs) xs))

sinMinimo :: Eq a => a -> [a] -> [a]
sinMinimo min [] = []
sinMinimo min (x:xs) = 
    if x == min
        then xs
        else x: (sinMinimo min xs)
