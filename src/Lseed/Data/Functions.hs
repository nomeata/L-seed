module Lseed.Data.Functions where

import Lseed.Data
import Data.Monoid

-- | Puts the length of the current segment in the additional information field
--   Pieces without length ('Bud', 'Fork') receive a zero.
plantPieceLengths :: Plant a -> Plant Double
plantPieceLengths (Bud _) =
	Bud 0
plantPieceLengths (Stipe _ len p1) =
	Stipe len len (plantPieceLengths p1)
plantPieceLengths (Fork _ angle p1 p2) =
	Fork 0 angle (plantPieceLengths p1) (plantPieceLengths p2)

plantSubpieceLength :: Plant a -> Plant Double
plantSubpieceLength = fmap getSum . subPieceAccumulate . fmap Sum . plantPieceLengths

extractOutmost :: Plant a -> a
extractOutmost (Bud x) = x
extractOutmost (Stipe x _ _) = x
extractOutmost (Fork x _ _ _) = x

subPieceAccumulate :: Monoid m => Plant m -> Plant m
subPieceAccumulate p = go p
  where go (Bud x) = (Bud x)
        go (Stipe x len p1) = let p1' = go p1
                                  x' = x `mappend` extractOutmost p1'
                              in  Stipe x' len p1'
        go (Fork x angle p1 p2) = let p1' = go p1
                                      p2' = go p2
                                      x' = x `mappend`
                                           extractOutmost p1' `mappend`
	                                   extractOutmost p2'
                                  in  Fork x' angle p1' p2'

