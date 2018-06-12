myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ accum [] = accum
myFoldl f accum (x:xs) = myFoldl f (f accum x) xs

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) [] 

firstThat :: (a -> Bool) -> a -> [a] -> a 
firstThat f = foldr (\acc x -> if f x then x else acc)
--firstThat f = foldr (\x acc -> if f x then x else acc)

lastThat :: (a -> Bool) -> a -> [a] -> a 
lastThat f = foldl (\acc x -> if f x then x else acc)

--argmax :: (Ord b) -> (a -> b) -> [a] -> a
argmax f [x] = x
argmax f (x:xs) = if f x > f (argmax f xs) then x else argmax f xs
