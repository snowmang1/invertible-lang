module BFS ( module BFS ) where

-- | Node that allows for current operation, color, distance from root, parent OpNode
type BFSTreeNode = (OpNode, String, Integer, OpNode)

-- | BFS algorithm compliant tree
data BFSTree = Nil | Delta BFSTreeNode [BFS_tree] | Leaf BFSTreeNode


-- TODO: BFS conversion according to paper

-- | will include the BFS init process and conversion from tree to tree
conversion :: AstArb -> BFSTree

-- TODO: BFS alg setup

-- | BFS algorithm impl
bfs_on_ast :: AstArb -> BFSTree
