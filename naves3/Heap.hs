module Heap (
    Heap,
    emptyH,
    isEmptyH,
    insertH,
    findMin,
    deleteMin
)where


data Heap a = PQ [a]
    deriving Show

-- Propósito: devuelve una priority queue vacía.
emptyH :: Heap a
emptyH = PQ []


-- Propósito: indica si la priority queue está vacía.
isEmptyH :: Heap a -> Bool
isEmptyH (PQ pq)  = null pq

-- Propósito: inserta un elemento en la priority queue.
insertH :: Ord a => a -> Heap a -> Heap a
insertH x (PQ xs)  = (PQ (x:xs))


--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMin :: Ord a => Heap a -> a
findMin (PQ xs) = maximum xs


--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (PQ xs) = (PQ (sinMaximo (maximum xs) xs))

sinMaximo :: Eq a => a -> [a] -> [a]
sinMaximo max [] = []
sinMaximo max (x:xs) = 
    if x == max
        then xs
        else x: (sinMaximo max xs)
