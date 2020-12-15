import Map3

-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Map k v -> [k]


-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valoresDe (keys map) map

valoresDe [] map = []
valoresDe (x:xs) map = lookupM x map : valoresDe xs map 

--Propósito: indica si en el map se encuenrtan todas las claves dadas.

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas ks map = pertenecenTodas ks (keys map)

pertenecenTodas :: Eq k => [k] -> [k] -> Bool
pertenecenTodas [] ks = True
pertenecenTodas (x:xs) ks = elem x ks && pertenecenTodas xs ks 

--Propósito: convierte una lista de pares clave valor en un map.

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv:kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

--Propósito: convierte un map en una lista de pares clave valor.

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = crearListKV map (keys map)

crearListKV :: Eq k => Map k v -> [k] -> [(k,v)]
crearListKV map [] = []
crearListKV map (x:xs) = (x,(valor(lookupM x map))): crearListKV map xs


valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x

-- Propósito: dada una lista de pares clave valor, agrupa los valores de
-- los pares que compartan la misma clave.

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (kv:kvs) =
	agruparA kv (agruparEq kvs)

-- agruparA :: Eq k => (k, v)-> Map k [v] -> Map k [v]
agruparA (k,v) map =
	case lookupM k map of
		(Just vs) -> assocM k (v:vs) map
		Nothing	  -> assocM k [v] map 	

-- Propósito: dada una lista de claves de tipo k y un mapa que va de k a int, le suma uno a 
-- cada número asociado con dichas claves.

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] map = map
incrementar (k:ks) map = 
	let actualizar  = incrementar ks map in
		case lookupM k map of 
			(Just int) -> assocM k (int + 1) actualizar
			Nothing -> actualizar  


-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = agruparMapas (keys map1) map1 map2


agruparMapas :: Eq k => [k] -> Map k v -> Map k v -> Map k v
agruparMapas [] map1 map2 = map2
agruparMapas (k:ks) map1 map2 = 
	let actualizar = agruparMapas ks map1 map2 in
		case lookupM k map2 of
			(Just v) -> assocM k (valor (lookupM k map1)) actualizar
			Nothing  -> assocM k (valor (lookupM k map1)) actualizar


-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar :: [a] -> Map Int a
indexar [] = emptyM
indexar (v:vs) = 
	assocM (sizeDeMap (indexar vs)) v (indexar vs)

sizeDeMap :: Map Int a -> Int
sizeDeMap map = length (keys map) 



-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias [] = emptyM
ocurrencias (x:xs) = 
	assocM x (aparaciones x (x:xs)) (ocurrencias xs)

aparaciones x [] = 0 
aparaciones x (x':xs) = 
	if x == x' 
		then 1 + aparaciones x xs
		else 0 + aparaciones x xs 



map1 :: Map String Int
map1 =
	assocM "fede" 200 $
	assocM "ale" 456 $
	assocM "cristian" 1 $
	emptyM

map2 :: Map String Int
map2 =
	assocM "fede" 123 $
	assocM "Roberto" 456 $
	assocM "cristian" 789 $
	emptyM