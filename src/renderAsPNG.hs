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
import Data.Maybe

readArgs doit = do
	args <- getArgs
	let name = fromMaybe "Some Plant" $ listToMaybe args

	file <- getContents
	let genome = either (error.show) id $ parseGrammar name file 
	doit $ [Planted 0.5 0 name genome inititalPlant]
		
main = readArgs $ \garden -> do
	obs <- pngObserver
	lseedMainLoop False obs (constGardenSource garden) 10
