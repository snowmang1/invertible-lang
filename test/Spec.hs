import Test.Tasty
import Test.Tasty.HUnit

import Data
import BFS

main :: IO()
main = defaultMain allTests

allTests :: TestTree
allTests =  testGroup "Tests" [mainTests, bfsTests]

mainTests :: TestTree
mainTests = testGroup "Main"
  [
  testCase "smoke" $
    True @?= True
  ]

simple_ast :: AstArb
simple_ast = AST_Delta Add [AST_Leaf "", AST_Leaf ""]

built_simple_bfs :: BFSTree
built_simple_bfs = BFS_Delta (Add, "B", 0, Add) [BFS_Nil, BFS_Nil]

depth_2_ast :: AstArb
depth_2_ast = AST_Delta Add [AST_Delta Sub [AST_Leaf "5", AST_Leaf "6"],
              AST_Delta Add [AST_Leaf "1", AST_Leaf "1"]]

depth_2_bfs :: BFSTree
depth_2_bfs = BFS_Delta (Add, "B", 0, Add) [BFS_Delta (Sub, "B", 0, Sub) [BFS_Nil, BFS_Nil],
              BFS_Delta (Add, "B", 0, Add) [BFS_Nil, BFS_Nil]]

depth_3_ast :: AstArb
depth_3_ast = AST_Delta Add [ AST_Delta Sub [AST_Delta Sub [ AST_Leaf "1", AST_Leaf "1"],
                                            AST_Delta Add [ AST_Leaf "1", AST_Leaf "1"]],
                              AST_Delta Add [ AST_Delta Sub [ AST_Leaf "1", AST_Leaf "1"],
                                            AST_Delta Add [ AST_Leaf "1", AST_Leaf "1"]]]

depth_3_bfs :: BFSTree
depth_3_bfs = BFS_Delta (Add, "B", 0, Add) [BFS_Delta (Sub, "B", 0, Sub) [
                                              BFS_Delta (Sub, "B", 0, Sub) [BFS_Nil, BFS_Nil],
                                              BFS_Delta (Add, "B", 0, Add) [BFS_Nil, BFS_Nil]],
                                            BFS_Delta (Add, "B", 0, Add) [
                                              BFS_Delta (Sub, "B", 0, Sub) [BFS_Nil, BFS_Nil],
                                              BFS_Delta (Add, "B", 0, Add) [BFS_Nil, BFS_Nil]]]

bfsTests :: TestTree
bfsTests = testGroup "BFS tests" [conversionTests, searchTests]

conversionTests :: TestTree
conversionTests = testGroup "BFS Structure Conversion Tests"
  [
  testCase "start_ast_parse Nil case" $
    ast_to_bfs (AST_Nil) @?= BFS_Nil,
  testCase "start_ast_parse Leaf case" $
    ast_to_bfs (AST_Leaf "") @?= BFS_Nil,
  testCase "start_ast_parse single Delta" $
    ast_to_bfs simple_ast @?= built_simple_bfs,
  testCase "start_ast_parse depth 2" $
    ast_to_bfs depth_2_ast @?= depth_2_bfs,
  testCase "start_ast_parse depth 3" $
    ast_to_bfs depth_3_ast @?= depth_3_bfs
  ]

depth1 :: BFSTree
depth1 = BFS_Delta (Sub, "B", 0, Sub) [BFS_Delta (Add, "B", 0, Add) [], BFS_Delta (Add, "B", 0, Add) []]

depth2 :: BFSTree
depth2 = BFS_Delta (Sub, "B", 0, Sub) [ BFS_Delta (Add, "B", 0, Add) [BFS_Delta (Add, "B", 0, Add) []],
                                        BFS_Delta (Add, "B", 0, Add) [BFS_Delta (Add, "B", 0, Add) []]]

searched_depth1 :: BFSTree
searched_depth1 = BFS_Delta (Sub, "W", 0, Sub) [BFS_Leaf (Add, "W", 1, Sub),
                                                BFS_Leaf (Add, "W", 1, Sub)]

searched_depth2 :: BFSTree
searched_depth2 = BFS_Delta (Sub, "W", 0, Sub) [BFS_Delta (Add, "W", 1, Sub) [
                                                    BFS_Leaf (Add, "W", 2, Add)],
                                                BFS_Delta (Add, "W", 1, Sub) [
                                                    BFS_Leaf (Add, "W", 2, Add)]]

depth3 :: BFSTree
depth3 = BFS_Delta (Sub, "B", 0, Sub) [ BFS_Delta (Add, "B", 0, Add) [
                                          BFS_Delta (Add, "B", 0, Add) [
                                            BFS_Delta (Sub, "B", 0, Sub) [],
                                            BFS_Delta (Sub, "B", 0, Sub) []]],
                                        BFS_Delta (Add, "B", 0, Add) [
                                          BFS_Delta (Add, "B", 0, Add) [
                                            BFS_Delta (Sub, "B", 0, Sub) [],
                                            BFS_Delta (Add, "B", 0, Add) []]]]

searched_depth3 :: BFSTree
searched_depth3 = BFS_Delta (Sub, "W", 0, Sub) [BFS_Delta (Add, "W", 1, Sub) [
                                                  BFS_Delta (Add, "W", 2, Add) [
                                                    BFS_Leaf (Sub, "W", 3, Add) ,
                                                    BFS_Leaf (Sub, "W", 3, Add) ]],
                                                BFS_Delta (Add, "W", 1, Sub) [
                                                  BFS_Delta (Add, "W", 2, Add) [
                                                    BFS_Leaf (Sub, "W", 3, Add) ,
                                                    BFS_Leaf (Add, "W", 3, Add) ]]]

searchTests :: TestTree
searchTests = testGroup "BFS Search Tests"
  [
  testCase "Empty Structure" $
    bfs_search (BFS_Nil) @?= BFS_Nil,
  testCase "Root=Leaf" $
    bfs_search (BFS_Leaf (Add, "B", 0, Add)) @?= BFS_Nil,
  testCase "Simple Tree, depth=1" $
    bfs_search depth1 @?= searched_depth1,
  testCase "depth=2" $
    bfs_search depth2 @?= searched_depth2,
  testCase "depth=3" $
    bfs_search depth3 @?= searched_depth3
  ]
