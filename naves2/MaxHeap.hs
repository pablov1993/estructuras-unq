module MaxHeap (
    MaxHeap,
    emptyH,
    isEmptyH,
    insertH,
    maxH,
    deleteMaxH
)where


data MaxHeap a = PQ [a]
    deriving Show

-- Propósito: devuelve una priority queue vacía.
emptyH :: MaxHeap a
emptyH = PQ []


-- Propósito: indica si la priority queue está vacía.
isEmptyH :: MaxHeap a -> Bool
isEmptyH (PQ pq)  = null pq

-- Propósito: inserta un elemento en la priority queue.
insertH :: Ord a => a -> MaxHeap a -> MaxHeap a
insertH x (PQ xs)  = (PQ (x:xs))


--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
maxH :: Ord a => MaxHeap a -> a
maxH (PQ xs) = maximum xs


--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMaxH :: Ord a => MaxHeap a -> MaxHeap a
deleteMaxH (PQ xs) = (PQ (sinMaximo (maximum xs) xs))

sinMaximo :: Eq a => a -> [a] -> [a]
sinMaximo max [] = []
sinMaximo max (x:xs) = 
    if x == max
        then xs
        else x: (sinMaximo max xs)
