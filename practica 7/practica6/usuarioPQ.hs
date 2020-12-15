import Pq1



--que dada una lista la ordena de menor a mayor utilizando una Heap como estructura auxiliar


--emptyPQ :: PriorityQueue a
--isEmptyPQ :: PriorityQueue a -> Bool
--insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--findMinPQ :: Ord a => PriorityQueue a -> a
--deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a


heapSort :: Ord a => [a] -> [a]
heapSort xs = ordenarPQtoList (listToPQ xs)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

ordenarPQtoList :: Ord a => PriorityQueue a -> [a]
ordenarPQtoList pq = 
    if isEmptyPQ pq 
        then []
        else findMinPQ pq:(ordenarPQtoList (deleteMinPQ pq))
