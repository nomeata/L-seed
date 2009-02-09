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
		threadDelay (2*1000*1000)
		nextStep garden'
	nextStep testGarden

growPlanted rgen planted =
	planted { phenotype = applyLSystem rgen (genome planted) (phenotype planted) }

testGarden =
	[ Planted 0.1 testLSystem1 (Bud ())
	, Planted 0.3 testLSystem2 (Bud ())
	, Planted 0.5 testLSystem3 (Bud ())
	, Planted 0.7 testLSystem2 (Bud ())
	, Planted 0.9 testLSystem1 (Bud ())
	]
testGarden2 =
	[ Planted 0.4 testLSystem1 (Bud ())
	, Planted 0.6 testLSystem1 (Bud ())
	]

testLSystem1 = [
	(1, \x -> case x of Bud () -> Just (Stipe () 1 (Bud ())); _ -> Nothing )
	]
testLSystem2 = [
	(3, \x -> case x of Bud () -> Just (Stipe () 2 (Bud ())); _ -> Nothing ),
	(2, \x -> case x of Bud () -> Just (Fork () ( pi/4) (Stipe () 1 (Bud ())) (Stipe () 1 (Bud ()))); _ -> Nothing ),
	(1, \x -> case x of Bud () -> Just (Fork () (-pi/4) (Stipe () 1 (Bud ())) (Stipe () 1 (Bud ()))); _ -> Nothing )
	]
testLSystem3 = [
	(1, \x -> case x of Bud () -> Just (Stipe () 3 (Bud ())); _ -> Nothing ),
	(1, \x -> case x of Bud () -> Just (
					Fork () (-2*pi/5) (Stipe () 1 (Bud ())) $
					Fork () (-1*pi/5) (Stipe () 1 (Bud ())) $
					Fork () ( 1*pi/5) (Stipe () 1 (Bud ())) $
					Fork () ( 2*pi/5) (Stipe () 1 (Bud ())) $
					Stipe () 1 (Bud ())); _ -> Nothing )
	]
