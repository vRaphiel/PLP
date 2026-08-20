sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

trasponer :: [[Int]] -> [[Int]]
trasponer =  foldr (\r rec -> if null rec 
                                then map (:[]) r
                                else zipWith (:) r rec) []
