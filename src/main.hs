import Lseed.Renderer.Cairo
import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Logic
import Control.Concurrent
import Control.Monad
import Debug.Trace
import System.Environment
import System.Time
import System.Random

parseFile filename = do
	content <- readFile filename
	let result = parseGrammar filename content
	return $ either (error.show) compileGrammarFile result

readArgs doit = do
	args <- getArgs
	if null args
	  then	do
		putStrLn "L-Seed Demo application."
		putStrLn "Please pass L-Seed files on the command line."
	  else	do
		plants <- mapM parseFile args
		doit (spread plants)
  where	spread gs = zipWith (\g p -> Planted ((p + 0.5) / l) g (Stipe () 0 [])) gs [0..]
	  where l = fromIntegral (length gs)
	      
		
main = readArgs $ \garden -> do
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
