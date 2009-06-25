import Lseed.Data
import Lseed.Data.Functions
import Lseed.DB
import Lseed.Grammar.Parse
import Lseed.Mainloop
import Lseed.Renderer.Cairo
import Control.Applicative
import Control.Monad
import Text.Printf


getDBGarden = spread <$> map compileDBCode <$> getCodeToRun
  where spread gs = zipWith (\(u,n,g) p ->
 		 Planted ((fromIntegral p + 0.5) / l)
			 u
			 n
			 g
			 inititalPlant
		) gs [0..]
	  where l = fromIntegral (length gs)

compileDBCode dbc =
	case  parseGrammar "" (dbcCode dbc) of
		Left err          -> error (show err)
		Right grammarFile -> (dbcUserID dbc, dbcUserName dbc, grammarFile)
dbc2genome = either (error.show) id . parseGrammar "" . dbcCode

getDBUpdate planted = maybe (genome planted) dbc2genome <$>
                      getUpdatedCodeFromDB (plantOwner planted)

main = do
	obs <- cairoObserver
	lseedMainLoop True obs (GardenSource getDBGarden getDBUpdate) 200
