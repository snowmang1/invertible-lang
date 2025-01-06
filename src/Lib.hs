module Lib ( module Lib ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- TODO: move all data structures to independant file

-- | this is the AST structure that will serve as the target for our investigation
data AstArb  = Nil | Delta OpNode [AstArb] | Leaf LeafNode

-- | These are the possible operations that can be discobered through AstArb
data OpNode   = Add | Sub

-- | This is here to serve as any const data that could be found,
-- but in the interest of this investigation it is not interesting (Lexeme)
type LeafNode = String

-- | a set of operations that maps to our AST, used to rearange operations without AST curruption
type FxnSeq   = [(OpNode, Integer)]

-- | This function will serve to encode an AST as a function sequence which is the first step in inverse proving
ast_fseq :: AstArb -> FxnSeq
