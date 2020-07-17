module Randomness
  ( pointlessNCoins,
    randoms',
    randomn,
    ioRandomN,
  )
where

import Control.Monad
import System.Random

data CoinFace = Head | Tail

instance Show CoinFace where
  show Head = "showing head..."
  show Tail = "showing tail..."

instance Random CoinFace where
  random gen =
    let (b, gen') = random gen
     in if b
          then (Head, gen')
          else (Tail, gen')

  randomR (Head, Tail) = random

threeCoins :: StdGen -> (CoinFace, CoinFace, CoinFace)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

pointlessNCoins :: StdGen -> Int -> [CoinFace]
pointlessNCoins = flip take . randoms

randoms' :: (Random a, RandomGen g) => g -> [a]
randoms' g =
  let (val, g') = random g
   in val : randoms' g'

randomn :: (Random a, RandomGen g) => g -> Int -> ([a], g)
randomn g 0 = ([], g)
randomn g n =
  let (val, g') = random g
      (tail, g'') = randomn g' (n -1)
   in (val : tail, g'')

randomn' :: (Random a, RandomGen g) => Int -> g -> ([a], g)
randomn' = flip randomn

ioRandomN :: (Random a) => Int -> IO [a]
ioRandomN n = fst . randomn' n <$> getStdGen

