myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ accum [] = accum
myFoldl f accum (x:xs) = myFoldl f (f accum x) xs

myReverse :: [a] -> [a]
myReverse = foldl (\accum x -> x : accum) [] 

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f accum [] = accum
myFoldr f accum (x:xs) = myFoldr f (f x accum) xs 

-- foldl is slow because of the lazy execution behaviour of Haskell i.e. the expressions are not executed until they are required. The expressions are just 
-- pushed onto the stack. Thus for large numbers, the expressions pushed is tremendous which results in stack overflow as the space inside the RAM is limited.

-- foldl' on the other hand executes the intermediate expressions which makes it much faster than the foldl function. It uses the system function of seq
-- which reduces the inner expressions.

checkNegative num 
 | num < 0 = num * (-1)
 | otherwise = num

listAbs :: [Integer] -> [Integer]
listAbs [] = []
listAbs [x] = checkNegative x:[]
listAbs (x:xs) = checkNegative x : listAbs xs 

sumAbs :: [Integer] -> Integer
sumAbs [] = 0
sumAbs [x] = checkNegative x
sumAbs x = myFoldr (+) 0 (listAbs x)