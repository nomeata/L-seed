module Lseed.Data.Functions where

import Lseed.Data
import Data.Monoid

-- | Puts the length of the current segment in the additional information field
--   Pieces without length ('Bud', 'Fork') receive a zero.
plantPieceLengths :: Plant a -> Plant Double
plantPieceLengths Bud =
	Bud
plantPieceLengths (Stipe _ len p1) =
	Stipe len len (plantPieceLengths p1)
plantPieceLengths (Fork angle p1 p2) =
	Fork angle (plantPieceLengths p1) (plantPieceLengths p2)

plantLength :: Plant a -> Double
plantLength = plantTotalSum . plantPieceLengths

plantTotalSum :: Plant Double -> Double
plantTotalSum = getSum . extractOutmost . subPieceAccumulate . fmap Sum 

extractOutmost :: Monoid a =>  Plant a -> a
extractOutmost Bud = mempty
extractOutmost (Stipe x _ _) = x
extractOutmost (Fork _ p1 p2) = extractOutmost p1 `mappend` extractOutmost p2

subPieceAccumulate :: Monoid m => Plant m -> Plant m
subPieceAccumulate p = go p
  where go Bud = Bud
        go (Stipe x len p1) = let p1' = go p1
                                  x' = x `mappend` extractOutmost p1'
                              in  Stipe x' len p1'
        go (Fork angle p1 p2) = let p1' = go p1
                                    p2' = go p2
                                in  Fork angle p1' p2'

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
