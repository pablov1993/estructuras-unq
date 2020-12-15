


data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
-- Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la
-- diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado :: Tree a -> Bool
balanceado EmptyT = True
balanceado (NodeT x t1 t2) = (sizeT t1 - sizeT t2 ) >= 1
    



sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a t1 t2) = 
    1 + sizeT t1 + sizeT t2


t1 :: Tree Int
t1 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 50
              (EmptyT)
              EmptyT)
          EmptyT)

t2 :: Tree Int
t2 = NodeT 32
        (NodeT 35
          (NodeT 100
              (NodeT 15
                  EmptyT
                  EmptyT)
              EmptyT)
          EmptyT)
        (NodeT 40
          (NodeT 50
              (NodeT 50
                EmptyT
                EmptyT)
              EmptyT)
          EmptyT)


--Dado un árbol binario de enteros devuelve la suma entre sus elementos.


pad :: [[Char]] -> [[Char]]
pad = zipWith (++) (repeat "   ")

arreglarT :: Show a => Tree a -> IO()
arreglarT EmptyT = putStrLn "-"

arreglarT tree = printRecursive (prettyprint_helper tree)
  where 
    printRecursive [] = putStrLn ""
    printRecursive (x:xs) = do
      putStrLn x
      printRecursive xs

prettyprint_helper (NodeT x left right)
  = (show x) : (prettyprint_subtree left right)
    where
      prettyprint_subtree left right =
        ((pad "+- " "|  ") (prettyprint_helper right))
          ++ ((pad "`- " "   ") (prettyprint_helper left))
      pad first rest = zipWith (++) (first : repeat rest)

prettyprint_helper EmptyT = [""]