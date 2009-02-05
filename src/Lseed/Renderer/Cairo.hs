module Lseed.Renderer.Cairo where

import Graphics.UI.Gtk hiding (fill)
import Graphics.Rendering.Cairo
import Control.Concurrent
import Data.IORef
import Lseed.Data

-- All relative to the screen width
groundLevel = 0.03
budSize     = 0.01
stipeLength = 0.05
stipeWidth  = 0.01


initRenderer :: IO (Garden -> IO ())
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
	onExpose canvas (\e -> do garden <- readIORef currentGardenRef
				  dwin <- widgetGetDrawWindow canvas
				  (w,h) <- drawableGetSize dwin
				  renderWithDrawable dwin $ do
					-- Set up coordinates
					translate 0 (fromIntegral h)
					scale 1 (-1)
					scale (fromIntegral w) (fromIntegral (w))
					
					setLineWidth stipeWidth
					render garden
		                  return (eventSent e))

	return $ \garden -> do
		writeIORef currentGardenRef garden
		widgetQueueDraw canvas

render :: Garden -> Render ()
render garden = do
	renderGround
	mapM_ (renderPlanted) garden

renderPlanted :: Planted -> Render ()
renderPlanted planted = preserve $ do
	translate (plantPosition planted) groundLevel
	setSourceRGB 0 1 0
	renderPlant (phenotype planted)

renderPlant :: Plant -> Render ()	
renderPlant Bud = do
	arc 0 0 budSize 0 (2*pi)
	fill
renderPlant (Stipe p) = do
	moveTo 0 0
	lineTo 0 stipeLength
	stroke
	translate 0 stipeLength
	renderPlant p
renderPlant (Fork p1 p2) = do
	preserve $ rotate (-pi/4) >> renderPlant p1
	preserve $ rotate (pi/4) >> renderPlant p2
		

renderGround :: Render ()
renderGround = do
	-- Clear Background
	setSourceRGB  0 1 1
	fill
	setSourceRGB  0.6 0.3 0.3
	rectangle 0 0 1 groundLevel
        fill

-- | Wrapper that calls 'save' and 'restore' before and after the argument
preserve :: Render () -> Render ()
preserve r = save >> r >> restore
