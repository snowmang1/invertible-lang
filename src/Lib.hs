module Lib ( module Lib ) where

import Data

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | This function will serve to encode an AST as a function sequence which is the first step in inverse proving
ast_fseq :: AstArb -> FxnSeq
ast_fseq ast = undefined
