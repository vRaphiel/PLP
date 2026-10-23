module Functions where 
    
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z []     = z
recr f z (x:xs) = f x xs (recr f z xs)