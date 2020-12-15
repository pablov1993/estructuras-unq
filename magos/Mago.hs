module Mago(

    Mago,
    Hechizo,
    Nombre,
    crearM,
    nombre,
    aprender,
    hechizos


) where 


import Set1

type Hechizo = String
type Nombre = String

data Mago = Mago Nombre (Set Hechizo)
    deriving Show


instance Eq Mago where
	(Mago n h) == (Mago n' h') = sizeS h == sizeS h'

instance Ord Mago where
	(Mago n h) <= (Mago n' h') = sizeS h <= sizeS h'


crearM :: Nombre -> Mago
crearM n = Mago n emptyS

nombre :: Mago -> Nombre
nombre (Mago n hechizos) = n

aprender :: Hechizo -> Mago -> Mago
aprender hechizo (Mago n hechizos) = Mago n (addS hechizo hechizos)

hechizos :: Mago -> Set Hechizo
hechizos (Mago n hs) = hs