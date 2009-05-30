module Lseed.Data.Functions where

import Lseed.Data
import Data.Monoid

-- | Puts the length of the current segment in the additional information field
plantPieceLengths :: Plant a -> Plant Double
plantPieceLengths (Plant _ len ang ut ps) =
	Plant len len ang ut (map plantPieceLengths ps)

plantLength :: Plant a -> Double
plantLength = plantTotalSum . plantPieceLengths

plantTotalSum :: Plant Double -> Double
plantTotalSum = getSum . pData . subPieceAccumulate . fmap Sum 

subPieceAccumulate :: Monoid m => Plant m -> Plant m
subPieceAccumulate p = go p
  where go (Plant x len ang ut ps) = let ps' = map go ps
                                         x' = x `mappend` (mconcat $ map pData ps')
                                     in  Plant x' len ang ut ps'

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
