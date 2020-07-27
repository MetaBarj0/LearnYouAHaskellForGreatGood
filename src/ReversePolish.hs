module ReversePolish
  ( readThenCalculate,
  )
where

import Data.List
import Data.Maybe
import Text.Read

readThenCalculate :: Maybe String -> Maybe Double
readThenCalculate Nothing = Nothing
readThenCalculate (Just string) = Just $ readThenCalculateImpl string

readThenCalculateImpl :: String -> Double
readThenCalculateImpl [] = 0.0
readThenCalculateImpl string = calculateTokens $ words string

isDouble :: String -> Bool
isDouble token = isJust (readMaybe token :: Maybe Double)

asDouble :: String -> Double
asDouble token = fromJust (readMaybe token :: Maybe Double)

calculateTokens :: Tokens -> Double
calculateTokens [token]
  | isDouble token = asDouble token
  | otherwise = error "Cannot interpret provided token as a value"
calculateTokens (token : tokens) = recursiveStackOrCompute token [] tokens

type Token = String

type Tokens = [Token]

type Stack = [Double]

isOperator :: String -> Bool
isOperator t
  | t == "+" = True
  | t == "-" = True
  | t == "*" = True
  | t == "/" = True
  | otherwise = False

asOperator :: String -> (Double -> Double -> Double)
asOperator t
  | t == "+" = (+)
  | t == "-" = (-)
  | t == "*" = (*)
  | t == "/" = (/)

recursiveStackOrCompute :: Token -> Stack -> Tokens -> Double
recursiveStackOrCompute token stack []
  | isDouble token = error "Trailing values in the stack"
  | length stack > 2 = error "Trailing values in the stack"
  | isOperator token = computeStack token stack
  | otherwise = error "Unrecognized token."
recursiveStackOrCompute token stack (t : ts)
  | isDouble token = recursiveStackOrCompute t (stack ++ [asDouble token]) ts
  | isOperator token && length stack < 2 = error "Cannot apply operator on less than 2 operands"
  | isOperator token =
    let remainingStack = init $ init stack
        value = computeStack token stack
     in recursiveStackOrCompute t (remainingStack ++ [value]) ts
  | otherwise = error "Unrecognized token."

computeStack :: Token -> Stack -> Double
computeStack token stack =
  let leftValue = last stack
      rightValue = last $ init stack
      op = asOperator token
   in leftValue `op` rightValue
