module ReArrange ( module ReArrange ) where

import Data
import BFS

-- | This creates the inverse function of f, f^{-1}
-- limited to arithmetic scope, well formed BFSTree and invertable functions
invertBFSTree :: BFSTree -> BFSTree
invertBFSTree BFS_Nil             = BFS_Nil
invertBFSTree (BFS_Leaf node)     = BFS_Leaf (typeInversion node)
invertBFSTree (BFS_Delta node ls) = BFS_Delta (typeInversion node) (map invertBFSTree ls)

-- | function to invert any defined OpNode
typeInversion :: BFSTreeNode -> BFSTreeNode
typeInversion (Add, c, d, p) = (Sub, c, d, p)
typeInversion (Sub, c, d, p) = (Add, c, d, p)
typeInversion op  = undefined
