import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Mainloop
import Control.Monad
import Debug.Trace
import System.Environment
import System.Time
import System.Random
import Text.Printf

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
		genomes <- mapM parseFile args
		doit (spread genomes)
  where	spread gs = zipWith (\g p -> Planted ((fromIntegral p + 0.5) / l) p g inititalPlant) gs [0..]
	  where l = fromIntegral (length gs)
	      

scoringObs = nullObserver {
	obFinished = \garden -> do
		forM_ garden $ \planted -> do
			printf "Plant from %d at %.4f: Total size %.4f\n"
				(plantOwner planted)
				(plantPosition planted)
				(plantLength (phenotype planted))
	}

main = readArgs $ \garden -> do
	lseedMainLoop False scoringObs 100 garden
