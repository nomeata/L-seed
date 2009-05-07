module Lseed.Data.Functions where

import Lseed.Data
import Data.Monoid

-- | Puts the length of the current segment in the additional information field
plantPieceLengths :: Plant a -> Plant Double
plantPieceLengths (Stipe _ len ps) =
	Stipe len len (mapSprouts plantPieceLengths ps)

plantLength :: Plant a -> Double
plantLength = plantTotalSum . plantPieceLengths

plantTotalSum :: Plant Double -> Double
plantTotalSum = getSum . extractOutmost . subPieceAccumulate . fmap Sum 

extractOutmost :: Plant a -> a
extractOutmost (Stipe x _ _) = x

subPieceAccumulate :: Monoid m => Plant m -> Plant m
subPieceAccumulate p = go p
  where go (Stipe x len ps) = let ps' = mapSprouts go ps
                                  x' = x `mappend` (mconcat $ map (extractOutmost.snd) ps')
                              in  Stipe x' len ps'

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
