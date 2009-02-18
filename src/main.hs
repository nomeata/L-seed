import Lseed.Renderer.Cairo
import Lseed.Data
import Lseed.LSystem
import Data.List
import Control.Concurrent
import Control.Monad
import System.Random
import System.Time
import Text.Printf

-- | Length of one day, in seconds
dayLength = 10 


timeSpanFraction :: ClockTime -> ClockTime -> Double
timeSpanFraction (TOD sa pa) (TOD sb pb) = 
	min 1 $ max 0 $
	(fromIntegral $ (sb - sa) * 1000000000000 + (pb-pa)) /
        (fromIntegral $ dayLength * 1000000000000 )

main = do
	renderGarden <- initRenderer
	-- mapM_ (\g -> threadDelay (500*1000) >> renderGarden g) (inits testGarden)
	let nextDay (day,garden) = do
		now <- getClockTime
		rgen <- newStdGen
		let garden' = growGarden rgen garden

		renderGarden $ \later -> 
			let timeDiff = timeSpanFraction now later
                            timeInfo = printf "Day %d (%2.0f%%)" day (timeDiff*100)
		            angle = pi/100 + timeDiff * (98*pi/100)
			    gardenNow = applyGrowth timeDiff garden'
			in ScreenContent gardenNow angle timeInfo

		threadDelay (dayLength*1000*1000)
		nextDay (succ day,finishGrowth garden')
	nextDay (0::Integer,testGarden)

growGarden :: (RandomGen g) => g -> Garden () -> Garden (Maybe Double)
growGarden rgen = snd . mapAccumL go rgen 
  where go rgen planted = let (rgen1,rgen2) = split rgen in (rgen2, growPlanted rgen1 planted)

-- | Applies an L-System to a Plant, putting the new length in the additional
--   information field
growPlanted :: (RandomGen g) => g -> Planted () -> Planted (Maybe Double)
growPlanted rgen planted =
	planted { phenotype = applyLSystem rgen (genome planted) (phenotype planted) }

-- | Finishes Growth by reading lenght from the additional information field
finishGrowth :: Garden (Maybe Double) -> Garden ()
finishGrowth = applyGrowth' (flip const)

-- | Applies Growth at given fraction
applyGrowth :: Double -> Garden (Maybe Double) -> Garden ()
applyGrowth r = applyGrowth' (\a b -> a * (1-r) + b * r)

applyGrowth' :: (Double -> Double -> Double) -> Garden (Maybe Double) -> Garden ()
applyGrowth' f = map (\planted -> planted { phenotype = go (phenotype planted) })
  where go (Bud Nothing) = Bud ()
        go (Stipe Nothing l p) = Stipe () l (go p)
        go (Fork Nothing a p1 p2) = Fork () a (go p1) (go p2)
	go (Stipe (Just l2) l1 p) = Stipe () (f l1 l2) (go p)
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
