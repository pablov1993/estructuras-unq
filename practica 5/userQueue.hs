import Queue2

--emptyQ :: Queue a
--queue :: a -> Queue a -> Queue a
--isEmptyQ :: Queue a -> Bool
--firstQ :: Queue a -> a
--dequeue :: Queue a -> Queue a

-- Cuenta la cantidad de elementos de la cola.

lengthQ :: Queue a -> Int
lengthQ q = 
    if (isEmptyQ q)
        then 0
        else 1 + (lengthQ (dequeue q))


-- Dada una cola devuelve la lista con los mismos elementos,
-- donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.

queueToList :: Queue a -> [a]
queueToList q = 
    if isEmptyQ q
        then []
        else firstQ q : (queueToList (dequeue q))

-- Inserta todos los elementos de la segunda cola en la primera.

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = 
    if isEmptyQ q2
        then q1
        else queue (firstQ q2) (unionQ q1 (dequeue q2))

cola :: Queue Int
cola = 
    queue 1 $
    queue 2 $
    emptyQ


cola2 :: Queue Int
cola2 = 
    queue 40 $
    queue 100 $
    emptyQ