import Lseed.Data
import Lseed.Data.Functions
import Lseed.Grammar.Parse
import Lseed.Constants
import Lseed.Mainloop
import Control.Monad
import Debug.Trace
import System.Environment
import System.Time
import System.Random
import Lseed.Renderer.Cairo
import Data.Maybe
import Graphics.Rendering.Cairo

main = do
	args <- getArgs
	let name = fromMaybe "Some Plant" $ listToMaybe args

	file <- getContents
	case parseGrammar name file of
	 Left _ -> do 
		let (w,h) = (300,300)
                withImageSurface FormatRGB24 w h $ \sur -> do
                        renderWith sur $ do
				setSourceRGB 1 1 1
				paint

                                translate 0 (0.5* fromIntegral h)
				setFontSize (0.1* fromIntegral h)
				setSourceRGB 0 0 0
				showText "Syntax Error"
                        surfaceWriteToPNG sur "/dev/fd/1"
	 Right genome -> do
		let garden = [Planted 0.5 0 name genome inititalPlant]
		obs <- pngObserver
		lseedMainLoop False obs (constGardenSource garden) 10
