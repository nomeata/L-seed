import Lseed.Data
import Lseed.Data.Functions
import Lseed.DB
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Mainloop
import Control.Applicative
import Control.Monad
import Text.Printf

getGarden = spread <$> map (either (error.show) compileGrammarFile . parseGrammar "" . dbcCode)
		   <$> getCodeToRun
  where spread gs = zipWith (\g p -> Planted ((fromIntegral p + 0.5) / l) p g (Stipe () 0 [])) gs [0..]
	  where l = fromIntegral (length gs)

scoringObs = nullObserver {
	obFinished = \garden -> do
		forM_ garden $ \planted -> do
			printf "Plant from %d at %.4f: Total size %.4f\n"
				(plantOwner planted)
				(plantPosition planted)
				(plantLength (phenotype planted))
	}

main = do
	garden <- getGarden
	lseedMainLoop False scoringObs 10 garden
