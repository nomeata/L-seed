module Lseed.Renderer.Cairo where

import Graphics.UI.Gtk hiding (fill)
import Graphics.Rendering.Cairo
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Maybe
import Lseed.Data
import Lseed.Data.Functions
import Lseed.Constants
import Lseed.Geometry
import Text.Printf
import System.Time

initRenderer :: IO (Garden a -> IO ())
initRenderer = do
	initGUI

	-- global renderer state
	currentGardenRef <- newIORef []

	-- widgets
	canvas <- drawingAreaNew

	window <- windowNew
	set window [windowDefaultWidth := 800, windowDefaultHeight := 600,
	      containerChild := canvas, containerBorderWidth := 0]
	widgetShowAll window

	-- Make gtk and haskell threading compatible
	timeoutAdd (yield >> return True) 50
	
	-- a thread for our GUI
	forkIO $ mainGUI

	-- The actual drawing function
	onExpose canvas$ \e -> do garden <- readIORef currentGardenRef
				  dwin <- widgetGetDrawWindow canvas
				  (w,h) <- drawableGetSize dwin
				  -- Sun rotation based on time for now
				  TOD s p <- getClockTime
				  let angle = fromIntegral (s * 1000*1000*1000*1000 + p `mod` (30*1000*1000*1000*1000)) * pi/(30*1000*1000*1000*1000)
				  renderWithDrawable dwin $ do
					-- Set up coordinates
					translate 0 (fromIntegral h)
					scale 1 (-1)
					scale (fromIntegral w) (fromIntegral (w))
					translate 0 groundLevel
					setLineWidth stipeWidth

					render angle garden
		                  return (eventSent e)

	timeoutAdd (widgetQueueDraw canvas >> return True) 20

	return $ \garden -> do
		writeIORef currentGardenRef garden
		widgetQueueDraw canvas

render :: Double -> Garden a -> Render ()
render angle garden = do
	renderGround
	mapM_ renderLightedPoly (lightPolygons angle (gardenToLines garden))
	--mapM_ renderLightedLine (lightenLines angle (gardenToLines garden))
	-- mapM_ renderLine (gardenToLines garden)
	mapM_ renderLightedPlanted (lightenGarden angle garden)
	mapM_ renderPlanted garden

	renderInfo angle garden

renderPlanted :: Planted a -> Render ()
renderPlanted planted = preserve $ do
	translate (plantPosition planted) 0
	setSourceRGB 0 0.8 0
	renderPlant (phenotype planted)

renderPlant :: Plant a -> Render ()	
renderPlant (Bud _) = do
	arc 0 0 budSize 0 (2*pi)
	fill
renderPlant (Stipe _ len p) = do
	moveTo 0 0
	lineTo 0 (len * stipeLength)
	stroke
	translate 0 (len * stipeLength)
	renderPlant p
renderPlant (Fork _ angle p1 p2) = do
	preserve $ rotate angle >> renderPlant p1
	renderPlant p2
		
renderLightedPlanted :: Planted Double -> Render ()
renderLightedPlanted planted = preserve $ do
	translate (plantPosition planted) 0
	renderLightedPlant (phenotype planted)

renderLightedPlant :: Plant Double -> Render ()	
renderLightedPlant (Bud _) = return ()
renderLightedPlant (Stipe intensity len p) = do
	moveTo 0 0
	lineTo 0 (len * stipeLength)
	let normalized = intensity / (len * stipeLength)
	when (normalized > 0) $ do
		liftIO $ print normalized
		setLineWidth (2*stipeWidth)
		setSourceRGBA 1 1 0 normalized
		stroke
	translate 0 (len * stipeLength)
	renderPlant p
renderLightedPlant (Fork _ angle p1 p2) = do
	preserve $ rotate angle >> renderLightedPlant p1
	renderLightedPlant p2
		
{- Line based rendering deprecated

renderLine (l@((x1,y1),(x2,y2)), _) = do
	setSourceRGB 0 1 0 
	setLineWidth (0.5*stipeWidth)
	moveTo x1 y1
	lineTo x2 y2
	stroke
	
renderLightedLine (l@((x1,y1),(x2,y2)), _, intensity) = do
	moveTo x1 y1
	lineTo x2 y2
	let normalized = intensity / lineLength l
	when (normalized > 0) $ do
		setLineWidth (1.5*stipeWidth)
		setSourceRGBA 1 1 0 normalized
		strokePreserve
	setSourceRGB 0 1 0 
	setLineWidth (0.5*stipeWidth)
	stroke
-}
	
renderLightedPoly ((x1,y1),(x2,y2),(x3,y3),(x4,y4), intensity) = do
	when (intensity > 0) $ do
		moveTo x1 y1
		lineTo x2 y2
		lineTo x3 y3
		lineTo x4 y4
		closePath
		setSourceRGB 0 0 intensity
		fill

renderInfo angle garden = do
	let gardenWithLight = lightenGarden angle garden
	forM_ gardenWithLight $ \planted -> do
		let x = plantPosition planted
		let text1 = printf "Light: %.2f" $
				extractOutmost (subPieceSum (phenotype planted))
		let text2 = printf "Size: %.2f" $
				extractOutmost $ plantSubpieceLength (phenotype planted)
		preserve $ do
			scale 1 (-1)
			setSourceRGB 0 0 0
			setFontSize (groundLevel/2)
			moveTo x (0.9*groundLevel)
			showText text1
			moveTo x (0.5*groundLevel)
			showText text2

renderGround :: Render ()
renderGround = do
	-- Clear Background
	rectangle 0 0 1 100
	setSourceRGB  0 0 1
	fill
	setSourceRGB (140/255) (80/255) (21/255)
	rectangle 0 0 1 (-groundLevel)
        fill

-- | Wrapper that calls 'save' and 'restore' before and after the argument
preserve :: Render () -> Render ()
preserve r = save >> r >> restore
