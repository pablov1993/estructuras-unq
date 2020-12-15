module Set2(
    Set,
    emptyS,
    addS,
    belongs,
    sizeS,
    removeS,
    unionS,
    setToList,
    set,
    set2
) where

data Set a = Set [a]
    deriving Show

set = Set [1,2,3,4,5]
set2 = Set [50,50,20,1]

-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = Set []

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (Set xs) = if elem x xs
                    then Set xs
                    else Set (x:xs)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (Set xs) = elem x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (Set xs) = length xs

-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS e (Set xs)  = Set(sacarElem e xs)

sacarElem :: Eq a => a -> [a] -> [a]
sacarElem e [] = []
sacarElem e (x:xs) = 
    if e == x
        then xs
        else x: sacarElem e xs

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Set xs) (Set js) = Set(unir xs js)

unir :: [a] -> [a] -> [a]
unir xs js = xs++js

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (Set xs) = sinRepetidos xs


sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
    if pertenece x xs
        then sinRepetidos xs
        else x: sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece b [] = False
pertenece b (x:xs) = x == b || pertenece b xs



