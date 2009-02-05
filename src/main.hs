import Lseed.Renderer.Cairo
import Lseed.Data
import Lseed.LSystem
import Data.List
import Control.Concurrent
import Control.Monad
import System.Random

main = do
	renderGarden <- initRenderer
	-- mapM_ (\g -> threadDelay (500*1000) >> renderGarden g) (inits testGarden)
	let nextStep garden = do
		renderGarden garden
		garden' <- forM garden $ \planted ->  do
			rgen <- newStdGen
			return $ growPlanted rgen planted
		threadDelay (500*1000)
		nextStep garden'
	nextStep testGarden

growPlanted rgen planted =
	planted { phenotype = applyLSystem rgen (genome planted) (phenotype planted) }

testGarden =
	[ Planted 0.3 testLSystem1 Bud
	, Planted 0.7 testLSystem2 Bud
	, Planted 0.5 testLSystem2 Bud
	, Planted 0.9 testLSystem2 Bud
	]

testLSystem1 = [
	(1, \x -> case x of Bud -> Just (Stipe Bud); _ -> Nothing )
	]
testLSystem2 = [
	(3, \x -> case x of Bud -> Just (Stipe Bud); _ -> Nothing ),
	(2, \x -> case x of Bud -> Just (Fork (Stipe Bud ) (Stipe Bud)); _ -> Nothing )
	]
