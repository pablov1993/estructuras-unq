module Map3(
    
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
)where

data Map k v = Map [k][v]
    deriving Show


emptyM :: Map k v
emptyM = (Map [] [])

-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (Map ks vs) = armarMapa (actualizar k v ks vs)

armarMapa (vs,js) = Map vs js

actualizar :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
actualizar k v [] [] = ([k],[v])
actualizar k v (x:ks) (j:vs) = 
    if k == x
        then ((k:ks),(v:vs))
        else agregarEnTupla x j (actualizar k v ks vs)

agregarEnTupla x j (ks,vs) = ((x:ks),(j:vs))



-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map x s) = valorDe k x s

valorDe k [] [] = Nothing
valorDe k (l:ks) (v:vs) = 
    if k == l
        then Just v 
        else valorDe k ks vs




-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map xs vs) = armarMapa (borrarKV k xs vs)

borrarKV k [] []        = ([],[])
borrarKV k (x:xs)(v:vs) = 
    if k == x 
        then (xs,vs)
        else agregarEnTupla x v (borrarKV k xs vs)


--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (Map ks vs ) = ks


map1 = Map [1] [2]