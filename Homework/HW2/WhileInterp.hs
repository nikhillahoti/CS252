{-
  Name: Nikhil Lahoti
  Class: CS 252
  Assigment: HW2
  Date: 02/26/018
  Description: 
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j)
 | j == 0 = error "Divide by zero error." 
 | otherwise = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
applyOp _ _ _ = error "Not as per rules"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Var var) s = case (Map.lookup var s) of
                    (Just v) -> (v,s)
                    _ -> error "Value not in Store" 
evaluate (Val v) s = (v,s)

evaluate (Op o e1 e2) s =
  let (v1,s1) = evaluate e1 s
      (v2,s') = evaluate e2 s1
  in (applyOp o v1 v2, s')

evaluate (If e1 e2 e3) s = case (evaluate e1 s) of 
  ((BoolVal True), s) -> evaluate e2 s
  ((BoolVal False), s) -> evaluate e3 s
  _ -> error "not a valid Value"

evaluate (Assign var (Val v)) s = (v, Map.insert var v s)
evaluate (Assign var exp) s = evaluate (Assign var (Val res)) s1 where (res,s1) = evaluate exp s

evaluate (Sequence (Val v) e2) s = evaluate e2 s
evaluate (Sequence e1 e2) s = evaluate (Sequence (Val res) e2) s1 where (res,s1) = evaluate e1 s  

evaluate (While e1 e2) s = case (evaluate e1 s) of 
  ((BoolVal True), s) -> evaluate (Sequence e2 (While e1 e2)) s
  ((BoolVal False), s) -> ((BoolVal False), s)
  _ -> error "not a valid Value"


-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog   