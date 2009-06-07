-- | This module is mostly a general dump...
module Lseed.Logic where

import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Parse
import Lseed.LSystem
import Lseed.Constants
import Lseed.Geometry
import Lseed.StipeInfo
import System.Time
import Text.Printf
import System.Random
import Data.List
import qualified Data.Foldable as F

timeSpanFraction :: Double -> ClockTime -> ClockTime -> Double
timeSpanFraction spanLenght (TOD sa pa) (TOD sb pb) = 
	min 1 $ max 0 $
	(fromIntegral $ (sb - sa) * 1000000000000 + (pb-pa)) /
        (spanLenght * 1000000000000 )

formatTimeInfo :: Integer -> Double -> String
formatTimeInfo day frac = let minutes = floor (frac * 12 * 60) :: Integer
			      (hour, minute) = divMod minutes 60
                          in  printf "Day %d %2d:%02d" day (6+hour) minute

-- | Given the fraction of the time passed, returnes the angle of the sunlight
lightAngle :: Double -> Angle
lightAngle diff = pi/100 + diff * (98*pi/100)

-- | Calculates the length to be grown
remainingGrowth :: (a -> GrowthState) -> Planted a -> Double
remainingGrowth getGrowths planted = go (phenotype planted)
  where go p@(Plant { pLength = l1, pBranches = ps }) =
 	   sum (map go ps) + case getGrowths (pData p) of
  		NoGrowth         -> 0
                EnlargingTo l2   -> l2 - l1
                GrowingSeed done -> (1-done) * seedGrowthCost 

growGarden :: (RandomGen g) => Angle -> g -> GrowingGarden -> (Double -> GrowingGarden)
growGarden angle rgen garden = sequence $ zipWith growPlanted garden' lightings
  where lightings = map (plantTotalSum . fmap snd . phenotype) $ lightenGarden angle garden'
	garden' = applyGenome angle rgen garden

-- | For all Growing plants that are done, find out the next step
-- This involves creating new plants if some are done
applyGenome :: (RandomGen g) => Angle -> g -> GrowingGarden -> GrowingGarden 
applyGenome angle rgen garden = concat $ zipWith applyGenome' rgens aGarden
  where rgens = unfoldr (Just . split) rgen
	aGarden = annotateGarden angle garden
	applyGenome' rgen planted =
		if   remainingGrowth siGrowth planted < eps
		then planted { phenotype = applyLSystem rgen
							(genome planted)
							(phenotype planted)
		     -- here, we throw away the last eps of growth. Is that a problem?
			     } :
		     collectSeeds rgen planted
	 	else [fmap siGrowth planted]
	collectSeeds :: (RandomGen g) => g -> AnnotatedPlanted -> GrowingGarden
	collectSeeds rgen planted = snd $ F.foldr go (rgen,[]) planted
	  where go si (rgen,newPlants) = case siGrowth si of
	  		GrowingSeed _ ->
				let spread = ( - siHeight si + siOffset si
				             ,   siHeight si + siOffset si
					     )
				    (posDelta,rgen') = randomR spread rgen
				    p = Planted (plantPosition planted + posDelta)
			                          (plantOwner planted)
						  (genome planted)
						  (fmap (const NoGrowth) inititalPlant)
				in (rgen, p:newPlants)
			_ -> (rgen,newPlants)

-- | Applies an L-System to a Plant, putting the new length in the additional
--   information field
growPlanted :: GrowingPlanted -> Double -> (Double -> GrowingPlanted)
growPlanted planted light = 
	let remainingLength = remainingGrowth id planted
	in  if remainingLength > eps
            then let sizeOfPlant = plantLength (phenotype planted)
                     lightAvailable = light - costPerLength * sizeOfPlant^2
		     lowerBound = if sizeOfPlant < smallPlantBoostSize
		                  then smallPlantBoostLength
				  else 0
                     allowedGrowths = max lowerBound $
                                      (growthPerDayAndLight * lightAvailable) /
                                      (fromIntegral ticksPerDay) 
		     growthThisTick = min remainingLength allowedGrowths
		     growthFraction = growthThisTick / remainingLength 
		 in \tickDiff -> applyGrowth (tickDiff * growthFraction) planted
	    else const planted

-- | Applies Growth at given fraction, leaving the target length in place
applyGrowth :: Double -> GrowingPlanted -> GrowingPlanted
applyGrowth r = mapPlanted (applyGrowth' (\a b -> a * (1-r) + b * r))

applyGrowth' :: (Double -> Double -> Double) -> GrowingPlant -> GrowingPlant
applyGrowth' f = go
  where go (Plant NoGrowth l ang ut ps) = 
  		Plant NoGrowth l ang ut (map go ps)
	go (Plant (EnlargingTo l2) l1 ang ut ps) =
		Plant (EnlargingTo l2) (f l1 l2) ang ut (map go ps)
	go (Plant (GrowingSeed done) l ang ut ps) =
		Plant (GrowingSeed (f (done*seedGrowthCost) seedGrowthCost)) l ang ut (map go ps)
