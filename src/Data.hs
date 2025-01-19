module Data ( module Data ) where

-- | this is the AST structure that will serve as the target for our investigation
data AstArb  = AST_Nil | AST_Delta OpNode [AstArb] | AST_Leaf LeafNode

-- | These are the possible operations that can be discobered through AstArb
data OpNode   = Add | Sub deriving (Eq, Show)

-- | This is here to serve as any const data that could be found,
-- but in the interest of this investigation it is not interesting (Lexeme)
type LeafNode = String

-- | a set of operations that maps to our AST, used to rearange operations without AST curruption
type FxnSeq   = [(OpNode, Integer)]
