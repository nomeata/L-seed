import Lseed.Renderer.Cairo
import Lseed.Data
import Lseed.Data.Functions
import Lseed.LSystem
import Lseed.Constants
import Lseed.Geometry
import Data.List
import Control.Concurrent
import Control.Monad
import System.Random
import System.Time
import Text.Printf
import Debug.Trace

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


main = do
	renderGarden <- initRenderer
	-- mapM_ (\g -> threadDelay (500*1000) >> renderGarden g) (inits testGarden)
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
	nextDay (0::Integer, mapGarden (fmap (const Nothing)) testGarden)

-- | Calculates the length to be grown
remainingGrowth :: GrowingPlanted -> Double
remainingGrowth planted = go (phenotype planted)
  where go (Bud Nothing) = 0
        go (Stipe Nothing _ p) = go p
        go (Fork Nothing _ p1 p2) = go p1 + go p2
	go (Stipe (Just l2) l1 p) = (l2 - l1) +  go p
	go p                    = error $ "Unexpected data in growing plant: " ++ show p


growGarden :: (RandomGen g) => Angle -> g -> GrowingGarden -> (Double -> GrowingGarden)
growGarden angle rgen garden = sequence $ zipWith3 growPlanted rgens garden lightings
  where lightings = map (extractOutmost . subPieceSum . phenotype) $ lightenGarden angle garden
        rgens = unfoldr (Just . split) rgen

-- | Applies an L-System to a Plant, putting the new length in the additional
--   information field
growPlanted :: (RandomGen g) => g -> GrowingPlanted -> Double -> (Double -> GrowingPlanted)
growPlanted rgen planted light = 
	let planted' = if remainingGrowth planted < eps
                       then planted { phenotype =
				applyLSystem rgen (genome planted)
                                                  (finishGrowth (phenotype planted))
				}
                       else planted
	    remainingLength = remainingGrowth planted'
	in  if remainingLength > eps
            then let allowedGrowths = (growthPerDayAndLight * light + growthPerDay) /
                                      (fromIntegral ticksPerDay) 
		     growthThisTick = min remainingLength allowedGrowths
		     growthFraction = growthThisTick / remainingLength 
		 in \tickDiff -> applyGrowth (tickDiff * growthFraction) planted'
	    else const planted' 

-- | Finishes Growth by reading lenght from the additional information field
finishGrowth :: GrowingPlant -> Plant ()
finishGrowth = fmap (const ()) . applyGrowth' (flip const)

-- | Applies Growth at given fraction, leaving the target lenght in place
applyGrowth :: Double -> GrowingPlanted -> GrowingPlanted
applyGrowth r = mapPlanted (applyGrowth' (\a b -> a * (1-r) + b * r))

applyGrowth' :: (Double -> Double -> Double) -> GrowingPlant -> GrowingPlant
applyGrowth' f = go
  where go (Bud Nothing) = Bud Nothing
        go (Stipe Nothing l p) = Stipe Nothing l (go p)
        go (Fork Nothing a p1 p2) = Fork Nothing a (go p1) (go p2)
	go (Stipe (Just l2) l1 p) = Stipe (Just l2) (f l1 l2) (go p)
	go p                    = error $ "Unexpected data in growing plant: " ++ show p

testGarden =
	[ Planted 0.1 testLSystem1 (Bud ())
	, Planted 0.3 testLSystem2 (Bud ())
	, Planted 0.5 testLSystem3 (Bud ())
	, Planted 0.7 testLSystem2 (Bud ())
	, Planted 0.9 testLSystem1 (Bud ())
	]
testGarden2 =
	[ Planted 0.4 testLSystem1 (Bud ())
	, Planted 0.6 testLSystem1 (Bud ())
	]

testLSystem1 = [
	(1, \x -> case x of Bud () -> Just (Stipe (Just 1) 0 (Bud Nothing)); _ -> Nothing )
	]
testLSystem2 = [
	(3, \x -> case x of Bud () -> Just (Stipe (Just 2) 0 (Bud Nothing)); _ -> Nothing ),
	(2, \x -> case x of Bud () -> Just (Fork Nothing ( pi/4) (Stipe (Just 1) 0 (Bud Nothing)) (Stipe (Just 1) 0 (Bud Nothing))); _ -> Nothing ),
	(1, \x -> case x of Bud () -> Just (Fork Nothing (-pi/4) (Stipe (Just 1) 0 (Bud Nothing)) (Stipe (Just 1) 0 (Bud Nothing))); _ -> Nothing )
	]
testLSystem3 = [
	(1, \x -> case x of Bud () -> Just (Stipe (Just 3) 0 (Bud Nothing)); _ -> Nothing ),
	(1, \x -> case x of Bud () -> Just (
					Fork Nothing (-2*pi/5) (Stipe (Just 1) 0 (Bud Nothing)) $
					Fork Nothing (-1*pi/5) (Stipe (Just 1) 0 (Bud Nothing)) $
					Fork Nothing ( 1*pi/5) (Stipe (Just 1) 0 (Bud Nothing)) $
					Fork Nothing ( 2*pi/5) (Stipe (Just 1) 0 (Bud Nothing)) $
					Stipe (Just 1) 0 (Bud Nothing)); _ -> Nothing )
	]
