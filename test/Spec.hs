import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO()
main = defaultMain allTests

allTests :: TestTree
allTests =  testGroup "Tests" [mainTests]

mainTests :: TestTree
mainTests = testGroup "Main"
  [
  testCase "smoke" $
    True @?= True
  ]
