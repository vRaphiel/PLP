data HashSet a = Hash (a -> Integer) (Integer -> [a])

vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (const [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece key (Hash f t) = elem key (t (f key))  

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar key (Hash f t) = if pertenece key (Hash f t) then Hash f t else Hash f (\x -> if x == f key then key : t x else t x) 

interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a
interseccion (Hash f1 t1) (Hash f2 t2) = Hash f1 (\i -> [x | x <- t1 i, elem x (t2 i)])