-- | This module is mostly a general dump...
module Lseed.Logic where

import Lseed.Renderer.Cairo
import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.LSystem
import Lseed.Constants
import Lseed.Geometry
import Lseed.StipeInfo
import System.Time
import Text.Printf
import System.Random
import Data.List
import Control.Concurrent

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
remainingGrowth :: GrowingPlanted -> Double
remainingGrowth planted = go (phenotype planted)
  where go (Stipe Nothing _    ps) = sum (map (go.snd) ps)
	go (Stipe (Just l2) l1 ps) = (l2 - l1) + sum (map (go.snd) ps)

growGarden :: (RandomGen g) => Angle -> g -> GrowingGarden -> (Double -> GrowingGarden)
growGarden angle rgen garden = sequence $ zipWith growPlanted garden' lightings
  where lightings = map (plantTotalSum . phenotype) $ lightenGarden angle garden'
	garden' = applyGenome angle rgen garden

-- | For all Growing plants that are done, find out the next step
applyGenome :: (RandomGen g) => Angle -> g -> GrowingGarden -> GrowingGarden 
applyGenome angle rgen garden = zipWith3 applyGenome' rgens garden lGarden
  where rgens = unfoldr (Just . split) rgen
	lGarden = lightenGarden angle garden
	applyGenome' rgen planted lPlanted =
		if   remainingGrowth planted < eps
		then planted { phenotype = applyLSystem rgen
							(genome planted)
							(annotatePlant (phenotype lPlanted))
		     -- here, we throw away the last eps of growth. Is that a problem?
			     }
	 	else planted

-- | Applies an L-System to a Plant, putting the new length in the additional
--   information field
growPlanted :: GrowingPlanted -> Double -> (Double -> GrowingPlanted)
growPlanted planted light = 
	let remainingLength = remainingGrowth planted
	in  if remainingLength > eps
            then let sizeOfPlant = plantLength (phenotype planted)
                     lightAvailable = light - costPerLength * sizeOfPlant^2
                     allowedGrowths = max 0 $
                                      (growthPerDayAndLight * lightAvailable + growthPerDay) /
                                      (fromIntegral ticksPerDay) 
		     growthThisTick = min remainingLength allowedGrowths
		     growthFraction = growthThisTick / remainingLength 
		 in \tickDiff -> applyGrowth (tickDiff * growthFraction) planted
	    else const planted

-- | Finishes Growth by reading lenght from the additional information field
finishGrowth :: GrowingPlant -> Plant ()
finishGrowth = fmap (const ()) . applyGrowth' (flip const)

-- | Applies Growth at given fraction, leaving the target lenght in place
applyGrowth :: Double -> GrowingPlanted -> GrowingPlanted
applyGrowth r = mapPlanted (applyGrowth' (\a b -> a * (1-r) + b * r))

applyGrowth' :: (Double -> Double -> Double) -> GrowingPlant -> GrowingPlant
applyGrowth' f = go
  where go (Stipe Nothing l ps)    = Stipe Nothing l (mapSprouts go ps)
	go (Stipe (Just l2) l1 ps) = Stipe (Just l2) (f l1 l2) (mapSprouts go ps)

runGarden :: Garden a -> IO ()
runGarden garden = do
	renderGarden <- initRenderer
	let nextDay (tick, garden) = do
		let (day, tickOfDay) = tick `divMod` ticksPerDay

		tickStart <- getClockTime
		rgen <- newStdGen
		let sampleAngle = lightAngle $ (fromIntegral tickOfDay + 0.5) /
                                                fromIntegral ticksPerDay
		let growingGarden = growGarden sampleAngle rgen garden

		renderGarden $ \later -> 
		        let tickDiff = timeSpanFraction tickLength tickStart later
 			    dayDiff = (fromIntegral tickOfDay + tickDiff) /
                                      fromIntegral ticksPerDay
                            timeInfo = formatTimeInfo day dayDiff
		            visualizeAngle = lightAngle dayDiff
			    gardenNow = mapGarden (fmap (const ())) $ growingGarden tickDiff
			in ScreenContent gardenNow visualizeAngle timeInfo

		threadDelay (round (tickLength * 1000 * 1000))
		nextDay (succ tick, growingGarden 1)
	nextDay (0::Integer, mapGarden (fmap (const Nothing)) garden)
