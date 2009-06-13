import Lseed.Data
import Lseed.Data.Functions
import Lseed.DB
import Lseed.Grammar.Parse
import Lseed.Mainloop
import Control.Applicative
import Control.Monad
import Text.Printf

getGarden = spread <$> map compileDBCode
		   <$> getCodeToRun
  where spread gs = zipWith (\(u,g) p -> Planted ((fromIntegral p + 0.5) / l) u g inititalPlant) gs [0..]
	  where l = fromIntegral (length gs)

compileDBCode dbc =
	case  parseGrammar "" (dbcCode dbc) of
		Left err          -> error (show err)
		Right grammarFile -> (dbcUserID dbc, grammarFile)

scoringObs = nullObserver {
	obFinished = \garden -> do
		forM_ garden $ \planted -> do
			printf "Plant from %d at %.4f: Total size %.4f\n"
				(plantOwner planted)
				(plantPosition planted)
				(plantLength (phenotype planted))
		addFinishedSeasonResults garden
	}

main = do
	garden <- getGarden
	lseedMainLoop False scoringObs 10 garden
