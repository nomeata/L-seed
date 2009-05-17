import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Logic
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
	      
		
main = readArgs runGarden
