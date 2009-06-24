import Lseed.Data
import Lseed.Data.Functions
import Lseed.DB
import Lseed.Grammar.Parse
import Lseed.Mainloop
import Lseed.Renderer.Cairo
import Control.Applicative
import Control.Monad
import Text.Printf

dbc2genome = either (error.show) id . parseGrammar "" . dbcCode

getDBGarden = spread <$> map dbc2genome <$> getCodeToRun
  where spread gs = zipWith (\g p -> Planted ((fromIntegral p + 0.5) / l) p g inititalPlant) gs [0..]
	  where l = fromIntegral (length gs)

getDBUpdate planted = maybe (genome planted) dbc2genome <$>
                      getUpdatedCodeFromDB (plantOwner planted)

main = do
	obs <- cairoObserver
	lseedMainLoop True obs (GardenSource getDBGarden getDBUpdate) 200
