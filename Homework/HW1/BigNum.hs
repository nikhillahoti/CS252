{-
  Name: Nikhil Lahoti
  Class: CS 252
  Assigment: HW1
  Date: 2/9/2018
  Description: The program takes a big number which is represented as a list of small Int blocks and performs common 
  mathematical operations.
  
  getString function: takes the list of bigNum and converts it into string.  It handles cases when the middle element 
  of the list is shortened for e.g. 072 will be represented by 72, but the 0 needs to be taken into consideration. So it 
  handles those case.

  partInt function: It takes a single Int as input and converts it back into list of small Int blocks. 

  bigOperation function: It takes two lists as input. It performs following:
  		a) converts the list of block into a single string which is casted back to int
  		b) performs the operation (In addition case +)
  		c) converts back the num to small int blocks again 

  bigEq function: Checks if the string returned for both the list is same.

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

bigAdd [] y = y
bigAdd x [] = x 
bigAdd x y = partInt (read (getString x) + read (getString y))


getString [] = ""
getString (x:xs)
 | x < 0 = error "Negative numbers are not valid."
 | x < 10 = getString xs ++ "00" ++ show x 
 | x < 100 = getString xs ++ "0" ++ show x 
 | otherwise = getString xs ++ show x 

partInt num 
 | num < 0 = error "Negative numbers are not valid"
 | num < 1000 = [num]
 | num >= 1000 = (num `mod` 1000) : (partInt (num `quot` 1000))
 | otherwise = []
 
bigSubtract [] y = partInt (- read (getString y))
bigSubtract x [] = x
bigSubtract x y = partInt (read (getString x) - read (getString y))

bigDec x = bigSubtract x [1]

bigMultiply [] y = [0]
bigMultiply x [] = [0]
bigMultiply x y = partInt (read (getString x) * read (getString y))

bigPowerOf [] y = [0]
bigPowerOf x [] = [1]
bigPowerOf x y = partInt (read (getString x) ^ read (getString y))

bigEq x y = if (getString x) == (getString y) then True else False

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

checkNegative num 
 | num < 0 = -num
 | otherwise = num

lister :: [Integer] -> [Integer]
lister [] = []
lister [x] = checkNegative x:[]
lister (x:xs) = checkNegative x: lister xs