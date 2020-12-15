module Queue2(

    Queue,
    emptyQ,
    isEmptyQ,
    queue,
    firstQ,
    dequeue

)where


data Queue a = Q [a]
    deriving Show


-- Crea una cola vacía.

emptyQ :: Queue a
emptyQ = Q []


-- Dada una cola indica si la cola está vacía.

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

-- Dados un elemento y una cola, agrega ese elemento a la cola.

queue :: a -> Queue a -> Queue a
queue x (Q xs) = Q(x:xs)

--constante
-- Dada una cola devuelve el primer elemento de la cola.

firstQ :: Queue a -> a
firstQ (Q xs) = head xs

--lineal
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (sinElUltimo xs)

sinElUltimo [x] = []
sinElUltimo (x:xs) = 
    x : sinElUltimo xs


cola = Q [1,2,3,4,5]