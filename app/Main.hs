module Main where

import           MRandom
import           System.Random

main :: IO ()
main = showHist 100000 (statRoll 5 3 :: Simulation StdGen Int)
