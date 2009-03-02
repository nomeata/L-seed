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
plantSubpieceLength = subPieceSum . plantPieceLengths

extractOutmost :: Plant a -> a
extractOutmost (Bud x) = x
extractOutmost (Stipe x _ _) = x
extractOutmost (Fork x _ _ _) = x

subPieceSum :: Plant Double -> Plant Double
subPieceSum = fmap getSum . subPieceAccumulate . fmap Sum 

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

-- | Apply a function to each Planted in a Garden
mapGarden :: (Planted a -> Planted b) -> Garden a -> Garden b
mapGarden = map

-- | Apply a function to each Planted in a Garden, with an extra argument from a list
--   
--   You need to make sure that the list is long enough!
zipWithGarden :: (Planted a -> x -> Planted b) -> Garden a -> [x] -> Garden b
zipWithGarden = zipWith

-- | Apply a function to the Plant in a Planted
mapPlanted :: (Plant a -> Plant b) -> Planted a -> Planted b
mapPlanted f planted = planted { phenotype = f (phenotype planted) }
