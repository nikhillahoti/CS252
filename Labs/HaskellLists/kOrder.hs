findKthElem :: Ord a => Int -> [a] -> a
findKthElem _ [] = error " Empty List"
findKthElem n x
 | n < 0 = error " Cannnot have negative numbered list."
 | n > (length x) = error " Not enough elements in the list "
 | otherwise = last (take (n+1) x)


findEqual _ [] = []
findEqual a (x:xs)
 | a == x = x: findEqual a xs
 | otherwise = []

findNotEqual _ [] = []
findNotEqual a (x:xs)
 | a == x = findNotEqual a xs
 | otherwise = (x:xs)

finder [] = []
finder [x] = [[x]]
finder x 
 | length x < 0 = [[]] 
 | length (findNotEqual (head x) x) > 0 = findEqual (head x) x : finder (findNotEqual (head x) x)
 | otherwise = [findEqual (head x) x]

{-
packer :: Eq a => [a] -> [[a]]
packer [] = [[]]
packer [x] = [[x]]
packer (x:xs) 
 | x == (head xs) = (x:[(head xs)]):packer (tail xs))
 | otherwise = [x]: (packer xs)
-}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

myPack' [] = []
myPack' (y:ys) = reverse $ impl ys [[y]]
 where
 impl [] packed = packed
 impl (x:xs) (z:zs) 
  | x == (head z) = impl xs ((x:z):zs) 
  | otherwise     = impl xs ([x]:(z:zs))