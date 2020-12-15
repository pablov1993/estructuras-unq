module Map (
    
    Map,
    emptyM,
    assocM,
    lookupM,
    deleteM,
    keys
)where

data Map k v = Map [(k,v)]
    deriving Show


emptyM :: Map k v
emptyM = (Map [])

-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map xs) = Map (registrarKV (k,v) xs)


registrarKV :: (k,v) ->[(k,v)] -> [(k,v)]
registrarKV kv kvs = kv:kvs

-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map xs) = valorDe k xs

valorDe :: Eq k => k -> [(k,v)] -> Maybe v
valorDe k [] =  Nothing
valorDe k (x:xs) = 
    if k == fst x 
        then Just (snd x)
        else valorDe k xs


-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map kvs) = Map (sinRegistro k kvs)


sinRegistro :: Eq k => k -> [(k,v)] -> [(k,v)]
sinRegistro k [] = []
sinRegistro k (kv:kvs) = 
    if k == fst kv 
        then kvs 
        else kv: sinRegistro k kvs 


--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (Map xs) = clavesDe xs

clavesDe :: [(k,v)] -> [k]
clavesDe [] = []
clavesDe (kv:kvs) = fst kv : clavesDe kvs

