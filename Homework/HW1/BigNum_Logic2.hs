{-
  Name: Nikhil Lahoti
  Class: CS 252
  Assigment: HW1
  Date: <Date assignment is due>
  Description: 
  
  addition function: it just takes the sum of the numbers and moves if any carry is remaining

  subtract function: it works just as the addition but the when the carry from significant bit is required we pass carry as 1 which is subtracted from the first significant bits

  multiplication function: it uses the bigAdd function. 10 * 2 is same as 10 + 10. So bigAdd continues addition till the second number is not 0

  PowerOf function: it uses the bigMultiply with both the arguments passed as same. It does it until the y value is not 0.

-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y = bigAdd' x y 0

bigAdd' :: BigNum -> BigNum -> Block -> BigNum
bigAdd' [] [] c
 | c > 0 = [c]
 | otherwise = []
bigAdd' (x:xs) [] c 
 | c > 0 = ((x+c) `mod` 1000) : bigAdd' xs [] ((x+c) `quot` 1000)
 | otherwise = (x:xs) 
bigAdd' [] (y:ys) c 
 | c > 0 = ((y+c) `mod` 1000) : bigAdd' [] ys ((y+c) `quot` 1000)
 | otherwise = (y:ys)
bigAdd' (x:xs) (y:ys) c = ((x+y+c) `mod` 1000) : bigAdd' xs ys ((x+y+c) `quot` 1000) 

bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract' x y 0

stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' [] [] _ = []
bigSubtract' (x:xs) [] c
 | (x-c) >= 0 = (x-c):xs
 | otherwise = (x-c + 1000): bigSubtract' xs [] 1
bigSubtract' (x:xs) (y:ys) c
 | xs < ys = error " x is a smaller number"
 | (x-c) >= y = (x - c - y): bigSubtract' xs ys 0
 | otherwise = (x - c - y + 1000): bigSubtract' xs ys 1 

{- 
Would work for Haskell
bigEq :: BigNum -> BigNum -> Bool
bigEq x y 
 | x == y = True
 | otherwise = False
-}

bigEq :: BigNum -> BigNum -> Bool
bigEq [] [] = True
bigEq x [] = False
bigEq [] y = False
bigEq (x:xs) (y:ys) 
 | x == y = bigEq xs ys 
 | otherwise = False 


bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply [] [] = []
bigMultiply _ [0] = [0]
bigMultiply [0] [] = [0]
bigMultiply x y = bigAdd x (bigMultiply x (bigDec y))

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf _ [0] = [1]
bigPowerOf [] [] = []
bigPowerOf x y = bigMultiply x (bigPowerOf x (bigDec y))

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]