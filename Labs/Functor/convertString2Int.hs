convertStringList2NumList :: [String] -> [Integer]
convertStringList2NumList [] = []
convertStringList2NumList [x] = (read x):[]
convertStringList2NumList (x:xs) = (read x):convertStringList2NumList xs


convertS :: [String] -> [Integer]
convertS [] = []
convertS x = map (read ) x

addOne2List :: [Int] -> [Int]
addOne2List [] = []
addOne2List x = map (+1) x

maybeIncrement :: Maybe Int -> Maybe Int
maybeIncrement x = map (+1) x

incrementContents :: Functor f => f Int -> f Int
incrementContents x = map (read ) x
