{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
module MRandom
    ( Simulation, die, execute, showHist, statRoll, runMany, collate, hist
    ) where
import           Control.Monad
import           Data.List
import qualified Data.Map      as M
import qualified Data.Text     as T
import           Fmt
import           System.Random

newtype Simulation g a = Sim (g -> (a, g))

instance Functor (Simulation g) where
  fmap f (Sim sim) = Sim final
    where final rng = (f val, rng')
            where (val, rng') = sim rng

instance Applicative (Simulation g) where
  pure = return
  Sim sim1 <*> Sim sim2 = Sim final
    where final rng = (f val, rng'')
            where (f, rng') = sim1 rng
                  (val, rng'') = sim2 rng'

instance Monad (Simulation g) where
  Sim sim >>= f = Sim final
    where final rng = sim' rng'
            where (val, rng') = sim rng
                  Sim sim' = f val
  return val = Sim (\g -> (val, g))

die n = Sim $ randomR (1, n)

statRoll roll keep = do
  vals <- replicateM roll $ die 6
  return $ sum $ take keep $ sortBy (flip compare) vals

execute (Sim n) = getStdRandom n

collate :: (Foldable l, Ord a) => l a -> M.Map a Int
collate = foldl (flip $ M.alter f) M.empty
  where f (Just i) = Just (i+1)
        f Nothing  = Just 1

runMany :: Ord a => Int -> Simulation StdGen a -> IO (M.Map a Int)
runMany n sim = fmap collate $ replicateM n $ execute sim

hist :: (Show a, Ord a) => Int -> M.Map a Int -> T.Text
hist n m = T.unlines $ zipWith makeLine results counts
  where results = map (\(a, b) -> (T.pack $ show a, b)) $ M.toList m
        maxVal = maximum $ map snd results
        maxKey = maximum $ map (T.length . fst) results
        total = sum $ map snd results
        toCount n = T.snoc (T.pack $ show $ fromIntegral prePercent / 10) '%'
          where prePercent = div (n * 1000) total
        counts = map (toCount . snd) results
        maxCount = maximum $ map T.length counts
        makeLine (key, val) count = T.concat [keyPart, " :", countPart, " ", valPart]
          where keyPart = T.justifyLeft maxKey ' ' key
                countPart = T.justifyLeft maxCount ' ' count
                valPart = T.justifyLeft n ' ' $ T.replicate (div (val * n) maxVal) $ T.singleton '#'

showHist :: (Show a, Ord a) => Int -> Simulation StdGen a -> IO ()
showHist n sim = do
  results <- runMany n sim
  putStrLn $ T.unpack $ hist 20 results
