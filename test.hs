module TEST where

import FCM
import Test.HUnit
import Data.Vector as V

main :: IO()
main = do
    let test1 = TestCase (assertEqual "Haming distance ([1,2] [3,4])" 4 (haming_distance  [1,2] [3,4]))
        test2 = TestCase (assertEqual "Evklid distance ([3,4] [0,0])" 5 (evklid_distance [3,4] [0,0]))
        test3 = TestCase (assertEqual "Maximum matrix difference ([[1,3],[5,0]] [[2,3],[7,9]])" 9 (maximum_matrix_diff [[1,3],[5,0]] [[2,3],[7,9]]))
        tests = TestList [test1, test2, test3]
    counts <- runTestTT tests
    return ()