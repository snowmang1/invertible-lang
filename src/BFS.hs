module BFS ( module BFS ) where

import Data

-- | Node that allows for current operation, color, distance from root, parent OpNode
type BFSTreeNode = (OpNode, String, Integer, OpNode)

-- | BFS algorithm compliant tree
data BFSTree = BFS_Nil | BFS_Delta BFSTreeNode [BFSTree] | BFS_Leaf BFSTreeNode deriving (Eq, Show)


-- | BFS sturcture conversion, from ast to bfs ready structure
ast_to_bfs :: AstArb -> BFSTree
ast_to_bfs AST_Nil          = BFS_Nil
ast_to_bfs (AST_Leaf _)     = BFS_Nil
ast_to_bfs (AST_Delta op l) = start_ast_parse (BFS_Delta (op, "B", 0, op) []) (AST_Delta op l)
  where
  -- | This is the translation engine for transforming an ASTArb -> BFSTree
  start_ast_parse :: BFSTree -> AstArb -> BFSTree
  -- base cases
    -- This happens when the AST is exhausted of children
  start_ast_parse BFS_Nil         _ = BFS_Nil
  -- translation cases
    -- AST_Nil reached, end of path
  start_ast_parse (BFS_Delta (_, _, _, _) _) (AST_Nil)     = BFS_Nil
    -- AST_Leaf reached thus end of path
  start_ast_parse (BFS_Delta (_, _, _, _) _) (AST_Leaf _)  = BFS_Nil
    -- populated ast children
  start_ast_parse (BFS_Delta (me, "B", dist, par) _) (AST_Delta _ astl) =
    BFS_Delta (me, "B", dist, par) (map ast_to_bfs astl)
  -- failed cases
    -- should there exist a BFS_Leaf the transformation has failed
  start_ast_parse _ _ = undefined

-- | BFS algorithm impl special, beginning with 
bfs_search :: BFSTree -> BFSTree
bfs_search BFS_Nil = BFS_Nil
  -- This case is trivial and does not require BFS
bfs_search (BFS_Leaf _) = BFS_Nil
bfs_search bt@(BFS_Delta _ _) = bfs_engine bt
  where
  -- | bfs_engine is the recursive engine for the BFS alg
  bfs_engine :: BFSTree -> BFSTree
  -- base cases
    -- leaf cases in different forms
  bfs_engine BFS_Nil                = BFS_Nil
  bfs_engine leaf@(BFS_Leaf _)      = leaf
  bfs_engine (BFS_Delta (leafop, _, leafdist, leafpar) [])  =
    BFS_Leaf (leafop, "W", leafdist, leafpar)
  -- populated child list
  bfs_engine (BFS_Delta op@(me, "B", dist, par) l) = BFS_Delta (me, "W", dist, par) (map f l)
    where
    f :: BFSTree -> BFSTree
    f (BFS_Delta node l') = bfs_engine (BFS_Delta (update_node op node) l')
      where
      -- | update_node acts as the search update U+2200
      update_node :: BFSTreeNode -> BFSTreeNode -> BFSTreeNode
      update_node (pop, _, pdist, _) (cop, _, _, _) = (cop, "B", pdist+1, pop)
    -- f should never recieve leaf or Nil nodes
    f _ = undefined
  bfs_engine _ = undefined
