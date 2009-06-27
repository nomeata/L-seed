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
	-> GardenSource -- ^ Where do get the plant code from
	-> Integer -- ^ Maximum days to run
	-> IO ()
lseedMainLoop rt obs gardenSource maxDays = do
	garden <- getGarden gardenSource
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
		let newGardenWithSeeds = applyGenome sampleAngle rgen garden
		rgen <- newStdGen
		newGarden <- fmap concat $ forM newGardenWithSeeds $
			\(parent,seedPoss) -> fmap (parent:) $ forM seedPoss $ \seedPos -> do
				genome <- getUpdatedCode gardenSource (fmap (const ()) parent)
				return $ Planted (plantPosition parent + seedPos)
				                 (plantOwner parent)
				                 (plantOwnerName parent)
				         	 genome
				         	 (fmap (const NoGrowth) inititalPlant)

		let growingGarden = growGarden sampleAngle rgen newGarden


		obState obs tick sampleAngle garden
		when rt $ do
			obGrowingState obs $ \later -> 
				let tickDiff = timeSpanFraction tickLength tickStart later
				    dayDiff = (fromIntegral tickOfDay + tickDiff) /
					      fromIntegral ticksPerDay
				    timeInfo = formatTimeInfo day dayDiff ++ " -- Connect now! http://lseed.gpn.entropia.de/ -- Changes auf http://entropia.de/wiki/L-seed"
				    visualizeAngle = lightAngle dayDiff
				    gardenNow = annotateGarden visualizeAngle $ 
				                growingGarden tickDiff
				in ScreenContent gardenNow visualizeAngle timeInfo

			threadDelay (round (tickLength * 1000 * 1000))
		nextDay (succ tick, growingGarden 1)
	nextDay (0::Integer, mapGarden (fmap (const NoGrowth)) garden)
