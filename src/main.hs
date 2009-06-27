import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Mainloop
import Control.Monad
import Debug.Trace
import System.Environment
import System.Time
import System.Random
import Lseed.Renderer.Cairo

parseFile filename = do
	content <- readFile filename
	let result = parseGrammar filename content
	return $ either (error.show) id result

readArgs doit = do
	args <- getArgs
	if null args
	  then	do
		putStrLn "L-Seed Demo application."
		putStrLn "Please pass L-Seed files on the command line."
	  else	do
		genomes <- mapM parseFile args
		doit (spread genomes)
  where	spread gs = zipWith (\g p ->
  		Planted ((fromIntegral p + 0.5) / l)
		        p
			("Player " ++ (show p))
			g
			inititalPlant
		) gs [0..]
	  where l = fromIntegral (length gs)
		
main = readArgs $ \garden -> do
	obs <- cairoObserver
	lseedMainLoop True
	              obs
		      ((constGardenSource garden) { getScreenMessage = (return (Just "hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"))})
		      30
	obShutdown obs
