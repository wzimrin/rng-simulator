import           Control.Monad
import qualified Data.Map      as M
import qualified Data.Text     as T
import           MRandom
import           Test.HUnit

main = runTestTT $ TestList
  ["Collate" ~: testCollate
  ,"Die" ~: testDie
  ,"Hist" ~: testHist]

testCollate = TestCase $ do
  assertEqual "collate works on ints" expected $ collate val
  assertEqual "collate works on strings" expected' $ collate val'
  where val = [1, 2, 4, 1, 4, 1]
        expected = M.fromList [(1, 3), (2, 1), (4, 2)]
        val' = ['a', 'b', 'a', 'c', 'a', 'a']
        expected' = M.fromList [('a', 4), ('b', 1), ('c', 1)]

testDie = TestCase $ do
  vals <- runMany 1000 $ die (6 :: Int)
  mapM_ (\n -> assertBool ("got a " ++ show n) $ M.member n vals) [1..6]
  assertEqual "1000 runs produces 1000 results" 1000 $ sum $ M.elems vals

testHist = TestCase $
  assertEqual "3 length map produces 3 lines" 3 $ length $ T.lines results
  where results :: T.Text
        results = hist 20 $ M.fromList [(1, 2), (2, 4), (3, 5)]
