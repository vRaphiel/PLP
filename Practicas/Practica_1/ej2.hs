v_curry :: ((a, b) -> c) -> a -> b -> c
v_curry f x y = f (x, y)

v_uncurry :: (a -> b -> c) -> (a, b) -> c
v_uncurry f (x, y) = f x y