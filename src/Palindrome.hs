module Palindrome
  ( isPalindrome,
    isPalindrome',
    filterOutUnneeded'',
    logit,
    logit2,
    doubleWriter,
    foldWriter,
    foldWriter2,
    foldWriter3,
    foldWriter4,
    foldWriter5,
    filterOutUnneeded3,
  )
where

import Control.Monad
import Control.Monad.Writer
import Data.Char

isPalindrome :: String -> Maybe Bool
isPalindrome = verifyPalindromness . normalizeInput
  where
    normalizeInput :: String -> Maybe String
    normalizeInput = filterOutUnneeded . lowercase
    filterOutUnneeded :: String -> Maybe String
    filterOutUnneeded input =
      case filtered of
        "" -> Nothing
        _ -> Just filtered
      where
        filtered = filter isNotUneeded input
    isNotUneeded :: Char -> Bool
    isNotUneeded c =
      case c of
        '.' -> False
        ',' -> False
        _ -> True
    lowercase :: String -> String
    lowercase = map toLower
    verifyPalindromness :: Maybe String -> Maybe Bool
    verifyPalindromness Nothing = Nothing
    verifyPalindromness (Just input) = Just (input == tupni)
      where
        tupni = reverse input

isPalindrome' :: String -> Bool
isPalindrome' = verifyPalindromness' . normalizeInput'
  where
    normalizeInput' :: String -> Maybe String
    normalizeInput' "" = Nothing
    normalizeInput' input = Just (filterOutUnneeded' $ lowercase' input)
    verifyPalindromness' :: Maybe String -> Bool
    verifyPalindromness' Nothing = False
    verifyPalindromness' (Just input) = input == reverse input

lowercase' :: String -> String
lowercase' = map toLower

filterOutUnneeded' :: String -> String
filterOutUnneeded' input = do
  i <- input
  guard $ foldr (\f -> (&& f i)) True predicates
  return i
  where
    predicates = map (/=) ",?;.:! "

filterOutUnneeded'' :: String -> String
filterOutUnneeded'' input = [c | c <- input, eliminatePunctuation c]
  where
    eliminatePunctuation :: Char -> Bool
    eliminatePunctuation c = foldr (\f -> (&& f c)) True predicates
    predicates :: [Char -> Bool]
    predicates = map (/=) ",?;.:! "

filterOutUnneeded2 :: String -> String
filterOutUnneeded2 input = do
  i <- input
  guard $ guardCheck i
  return i
  where
    guardCheck :: Char -> Bool
    guardCheck i = foldr (\f -> (&& f i)) True predicates
    predicates = map (/=) ",?;.:! "

filterOutUnneeded3 :: String -> IO String
filterOutUnneeded3 input =
  foldr
    ( \char acc -> do
        str <- acc
        check <- eliminatePunctuation char
        guard check
        putStrLn $ "=> " ++ show char
        return $ char : str
    )
    init
    input
  where
    init :: IO String
    init = return ""
    eliminatePunctuation :: Char -> IO Bool
    eliminatePunctuation c = return $ foldr (\f -> (&& f c)) True predicates
    predicates :: [Char -> Bool]
    predicates = map (/=) ",?;.:! "

-- WriterT stuff...

logit :: Int -> Writer String Int
logit v = writer (value, "Assigned value " ++ show value ++ ".")
  where
    value = v + 1

logit2 :: [Int] -> Int -> Writer [String] Int
logit2 array value = writer (foldr (\v -> (+ v)) value array, ["Done folding"])

doubleWriter :: Writer [String] String
doubleWriter = writer ("ok1", ["First computation"]) >> writer ("ok2", ["Last computation"])

foldWriter :: [Int] -> Int -> Writer [String] Int
foldWriter array init = foldedValue
  where
    foldedValue = foldr (\val acc -> acc >> writer (fst (runWriter acc) + val, ["Folding " ++ show val ++ "."])) initLog array
    initLog = writer (init, ["Begin."])

foldWriter2 :: [Int] -> Int -> Writer [String] Int
foldWriter2 array init = foldedValue
  where
    foldedValue = foldr (\val acc -> acc >>= (\v -> writer (val + v, ["Folding " ++ show val ++ "."]))) initLog array
    initLog = writer (init, ["Begin."])

foldWriter3 :: [Int] -> Int -> Writer [String] Int
foldWriter3 array init = foldedValue
  where
    foldedValue =
      foldr
        ( \val acc -> do
            v <- acc
            writer (val + v, ["Folding " ++ show val ++ "."])
        )
        initLog
        array
    initLog = writer (init, ["Begin."])

foldWriter4 :: [Int] -> Int -> Writer [String] Int
foldWriter4 array init =
  let folded =
        foldr
          ( \val acc -> do
              v <- acc
              tell ["Folding " ++ show val ++ "."]
              return (val + v)
          )
          initLog
          array
   in do
        tell ["End."]
        folded
  where
    initLog = do
      tell ["Begin."]
      return init

foldWriter5 :: [Int] -> Int -> Writer [String] Int
foldWriter5 array init = do
  foldedValue <-
    foldr
      ( \val acc -> do
          v <- acc
          tell ["Folding " ++ show val ++ "."]
          return (val + v)
      )
      initLog
      array
  tell ["End."]
  return foldedValue
  where
    initLog = do
      tell ["Begin."]
      return init
