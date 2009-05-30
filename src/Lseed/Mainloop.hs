-- | This module contains a runner for a an Lseed garden. It can be passed an
-- observer that will receive the results.
module Lseed.Mainloop where

import Lseed.Data
import Lseed.Geometry
import Lseed.Data.Functions
import Lseed.Constants
import Lseed.Logic
import Lseed.StipeInfo
import System.Time
import System.Random
import Control.Concurrent
import Control.Monad

-- | Lets a garden grow for the given number of days, while keeping the
-- observer informed about any changes.
lseedMainLoop :: Bool -- ^ Run in real time, e.g. call 'threadDelay'
	-> Observer -- ^ Who to notify about the state of the game
	-> Integer -- ^ Maximum days to run
	-> Garden () -- ^ Initial garden state
	-> IO ()
lseedMainLoop rt obs maxDays garden = do
	obInit obs
	let nextDay (tick, garden) = 
		let (day, tickOfDay) = tick `divMod` ticksPerDay in
		if day > maxDays then
			obFinished obs garden
		else do

		tickStart <- getClockTime
		rgen <- newStdGen
		let sampleAngle = lightAngle $ (fromIntegral tickOfDay + 0.5) /
                                                fromIntegral ticksPerDay
		let growingGarden = growGarden sampleAngle rgen garden

		obState obs tick garden
		when rt $ do
			obGrowingState obs $ \later -> 
				let tickDiff = timeSpanFraction tickLength tickStart later
				    dayDiff = (fromIntegral tickOfDay + tickDiff) /
					      fromIntegral ticksPerDay
				    timeInfo = formatTimeInfo day dayDiff
				    visualizeAngle = lightAngle dayDiff
				    gardenNow = annotateGarden visualizeAngle $ 
				                growingGarden tickDiff
				in ScreenContent gardenNow visualizeAngle timeInfo

			threadDelay (round (tickLength * 1000 * 1000))
		nextDay (succ tick, growingGarden 1)
	nextDay (0::Integer, mapGarden (fmap (const NoGrowth)) garden)
