import Lseed.Data
import Lseed.DB
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Logic
import Lseed.Renderer.Cairo
import Control.Applicative

getGarden = spread <$> map (either (error.show) compileGrammarFile . parseGrammar "" . dbcCode)
		   <$> getCodeToRun
  where spread gs = zipWith (\g p -> Planted ((p + 0.5) / l) g (Stipe () 0 [])) gs [0..]
	  where l = fromIntegral (length gs)

main = do
	garden <- getGarden
	obs <- cairoObserver
	lseedMainLoop obs 1 garden
