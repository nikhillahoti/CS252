getMax :: [Int] -> Maybe Int
getMax [] = Nothing
getMax [x] = Just x
getMax (x:xs)
 | Just x > getMax xs = Just x
 | otherwise = getMax xs


reciprocal :: (Eq a, Fractional a) => a -> Maybe a
reciprocal 0 = Nothing
reciprocal a = Just $ 1 / a

rectangleArea :: Int -> Int -> Either String Int
rectangleArea x y 
 | x < 0 = Left $ error "Width is not positive"
 | y < 0 = Left $ error "Height is not positive"
 | otherwise = Right $ x * y