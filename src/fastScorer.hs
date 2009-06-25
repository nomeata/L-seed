import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Mainloop
import qualified Data.Map as M
import Control.Monad
import Debug.Trace
import System.Environment
import System.Time
import System.Random
import Text.Printf

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
			(show p)
			g
			inititalPlant
		) gs [0..]
	  where l = fromIntegral (length gs)
	      

scoringObs = nullObserver {
	obFinished = \garden -> do
		forM_ garden $ \planted -> do
			printf "Plant from %s (%d) at %.4f: Total size %.4f\n"
				(plantOwnerName planted)
				(plantOwner planted)
				(plantPosition planted)
				(plantLength (phenotype planted))
		let owernerscore = foldr (\p -> M.insertWith (+) (plantOwner p, plantOwnerName p)(plantLength (phenotype p))) M.empty garden
		forM_ (M.toList owernerscore) $ \((o,n),s) -> 
			printf "Sum for %s (%d): %.4f\n" n o s
	}

main = readArgs $ \garden -> do
	lseedMainLoop False scoringObs (constGardenSource garden) 30
