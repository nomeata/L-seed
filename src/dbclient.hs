import Lseed.Data
import Lseed.Data.Functions
import Lseed.DB
import Lseed.Grammar.Compile
import Lseed.Grammar.Parse
import Lseed.Mainloop
import Lseed.Renderer.Cairo
import Control.Applicative
import Control.Monad
import Text.Printf

getGarden = spread <$> map (either (error.show) compileGrammarFile . parseGrammar "" . dbcCode)
		   <$> getCodeToRun
  where spread gs = zipWith (\g p -> Planted ((fromIntegral p + 0.5) / l) p g inititalPlant) gs [0..]
	  where l = fromIntegral (length gs)

main = do
	garden <- getGarden
	obs <- cairoObserver
	lseedMainLoop True obs 1 garden
