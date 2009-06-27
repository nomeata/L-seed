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
import Lseed.StipeInfo
import Text.Printf
import System.Time
import qualified Data.Map as M
import Data.List
import Data.Ord
import System.Time

colors :: [ (Double, Double, Double) ]
colors = cycle $ [ (r,g,b) | r <- [0.0,0.4], b <- [0.0, 0.4], g <- [1.0,0.6,0.8]]

pngDailyObserver :: FilePath -> Observer
pngDailyObserver filename = nullObserver {
	obGrowingState = \scGen -> do
		ScreenContent garden angle timeInfo mbMessage <-
			scGen `fmap` getClockTime 
		let (w,h) = (800,600)
	  	let h' = fromIntegral h / fromIntegral w
		withImageSurface FormatRGB24 w h $ \sur -> do
			renderWith sur $ do
				-- Set up coordinates
				translate 0 (fromIntegral h)
				scale 1 (-1)
				scale (fromIntegral w) (fromIntegral w)
				translate 0 groundLevel
				setLineWidth stipeWidth

				render angle garden

				maybe (return ()) (renderMessage angle h') mbMessage
				renderTimeInfo timeInfo
				renderStats h' garden
			surfaceWriteToPNG sur filename
	}

pngObserver :: IO Observer
pngObserver = return $ nullObserver {
	obFinished = \garden -> do
		let (w,h) = (400,400)
		withImageSurface FormatRGB24 w h $ \sur -> do
			renderWith sur $ do
				-- Set up coordinates
				translate 0 (fromIntegral h)
				scale 1 (-1)
				scale (fromIntegral w) (fromIntegral w)
				translate (-0.5) 0
				scale 2 2
				translate 0 groundLevel
				setLineWidth stipeWidth

				render (pi/3) (annotateGarden (pi/3) garden)
			surfaceWriteToPNG sur "/dev/fd/1"
	}

cairoObserver :: IO Observer
cairoObserver = do
	initGUI

	-- global renderer state
	currentGardenRef <- newIORef (const (ScreenContent [] (pi/2) "No time yet" Nothing))

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
	onExpose canvas$ \e -> do scGen <- readIORef currentGardenRef
				  ScreenContent garden angle timeInfo mbMessage <-
						scGen `fmap` getClockTime 
				  s <- clockTimeToDouble `fmap` getClockTime
				  dwin <- widgetGetDrawWindow canvas
				  (w,h) <- drawableGetSize dwin
				  let h' = fromIntegral h / fromIntegral w


				  let (xLeft,xRight,xHeight) = gardenOffset garden
				      scaleY = 1/xHeight
				      shiftX = if xRight-xLeft-xHeight > 0
				        then scaleY * (xLeft + (1+sin (s/4))/2 * max 0 (xRight-xLeft-xHeight))
					else 0

				  renderWithDrawable dwin $ do
					-- Set up coordinates
					translate 0 (fromIntegral h)
					scale 1 (-1)
					scale (fromIntegral w) (fromIntegral w)
					translate 0 groundLevel

					preserve $ do
						translate (-shiftX) 0
						scale scaleY scaleY
						render angle (windy s garden)
					maybe (return ()) (renderMessage angle h') mbMessage
					renderTimeInfo timeInfo
					renderStats h' garden
		                  return True

	timeoutAdd (widgetQueueDraw canvas >> return True) 20

	return $ nullObserver
		{ obGrowingState = \scGen -> do
			writeIORef currentGardenRef scGen
			widgetQueueDraw canvas
		, obShutdown = mainQuit
		}

render :: Double -> AnnotatedGarden -> Render ()
render angle garden = do
	-- TODO the following can be optimized to run allKindsOfStuffWithAngle only once.
	-- by running it here. This needs modification to lightenGarden and mapLine
	renderSky angle
	--mapM_ renderLightedPoly (lightPolygons angle (gardenToLines garden))

	--mapM_ renderLightedLine (lightenLines angle (gardenToLines garden))
	--mapM_ renderLine (gardenToLines garden)
	--mapM_ renderLightedPlanted garden

	mapM_ renderPlanted garden

	renderGround

	--renderInfo garden

renderPlanted :: AnnotatedPlanted -> Render ()
renderPlanted planted = preserve $ do
	translate (plantPosition planted) 0
	setLineCap LineCapRound
	let c = colors !! fromIntegral (plantOwner planted)
	renderPlant (Just (renderFlag (take 10 (plantOwnerName planted))))
	            c (phenotype planted)

renderFlag :: String -> Render ()
renderFlag text = preserve $ do
	scale 1 (-1)
	setFontSize (groundLevel/2)
	ext <- textExtents text

	preserve $ do
		translate (stipeWidth) (groundLevel/2)
		rectangle 0
			  (textExtentsYbearing ext + groundLevel/2)
			  (textExtentsXadvance ext)
			  (-textExtentsYbearing ext - groundLevel/2 - groundLevel/2)
		setSourceRGB 1 1 1
		fill

		setSourceRGB 0 0 0
		showText text

	setLineWidth (groundLevel/10)
	setSourceRGB 0 0 0
	moveTo 0 0
	lineTo (stipeWidth + textExtentsXadvance ext) 0
	stroke


-- | Renders a plant, or part of a plant, with a given colour. If the Render
-- argument is given, it is drawn at the end of the plant, if there are no
-- branches, or passed to exactly one branch.
renderPlant :: (Maybe (Render ())) -> (Double,Double,Double) -> AnnotatedPlant -> Render ()	
renderPlant leaveR color@(r,g,b) (Plant si len ang ut ps) = preserve $ do
	rotate ang
	withLinearPattern 0 0 0 (len * stipeLength) $ \pat -> do
		let darkenByBegin = 1/(1 + (siSubLength si)/15)
		let darkenByEnd = 1/(1 + (siSubLength si - siLength si)/15)
		patternAddColorStopRGB pat 0
			(darkenByBegin*r) (darkenByBegin*g) (darkenByBegin*b) 
		patternAddColorStopRGB pat 1
			(darkenByEnd*r) (darkenByEnd*g) (darkenByEnd*b) 
		setSource pat
		--setLineWidth (stipeWidth*(0.5 + 0.5 * sqrt (siSubLength si)))
		setLineWidth stipeWidth
		moveTo 0 0
		lineTo 0 (len * stipeLength)
		stroke
	translate 0 (len * stipeLength)
	if null ps
	 then fromMaybe (return ()) leaveR
	 else sequence_ $ zipWith (\r p -> renderPlant r color p)
	                         (leaveR : repeat Nothing)
				 ps
	case siGrowth si of
	  GrowingSeed done -> do
	  	setSourceRGB 1 1 0
	  	arc 0 0 (done * blossomSize/2) 0 (2*pi)
		fill
	  _ -> return ()
		
renderLightedPlanted :: AnnotatedPlanted -> Render ()
renderLightedPlanted planted = preserve $ do
	translate (plantPosition planted) 0
	renderLightedPlant (phenotype planted)

renderLightedPlant :: AnnotatedPlant -> Render ()	
renderLightedPlant (Plant si len ang ut ps) = preserve $ do
	rotate ang
	moveTo 0 0
	lineTo 0 (len * stipeLength)
	let normalized = siLight si / (len * stipeLength)
	when (normalized > 0) $ do
		--liftIO $ print normalized
		setLineWidth (2*stipeWidth)
		setSourceRGBA 1 1 0 normalized
		stroke
	translate 0 (len * stipeLength)
	mapM_ renderLightedPlant ps
		
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

renderInfo garden = do
	forM_ garden $ \planted -> do
		let x = plantPosition planted
		{-
		let text1 = printf "Light: %.2f" $
				siSubLight . pData . phenotype $ planted
		-}
		let text1 = plantOwnerName planted
		let text2 = printf "Size: %.2f" $
				siSubLength . pData . phenotype $ planted
		preserve $ do
			scale 1 (-1)
			setSourceRGB 0 0 0
			setFontSize (groundLevel/2)
			moveTo x (0.9*groundLevel)
			showText text2
			moveTo x (0.5*groundLevel)
			showText text1

renderTimeInfo timeStr = preserve $ do
		scale 1 (-1)
		setSourceRGB 0 0 0
		setFontSize (groundLevel/2)
		moveTo 0 (0.5*groundLevel)
		showText timeStr

renderMessage angle h text = preserve $ do
		scale 1 (-1)
		setSourceRGB 0 0 0
		translate (0.5) (2.5*groundLevel - h) 
		setFontSize (groundLevel)

		let bullet = " * "
		ext <- textExtents (text ++ bullet)

		rectangle (-0.25)
			  (textExtentsYbearing ext + groundLevel)
			  (0.5)
			  (-textExtentsYbearing ext - groundLevel - groundLevel)
		setSourceRGB 1 1 1
		fillPreserve
		clip

		let textWidth = textExtentsXbearing ext + textExtentsXadvance ext
		    textCount = ceiling $ 0.5/textWidth
		    scroll = 3 * (angle + pi/2)/(2*pi)
		    scroll' = scroll - fromIntegral (floor scroll)
		    scrollDist = fromIntegral textCount * textWidth
		translate (-0.25 - scroll' * scrollDist) 0

		setSourceRGB 0 0 0
		showText $ intercalate bullet $ replicate (2*textCount) text

renderStats h garden = do
	let owernerscore = foldr (\p -> M.insertWith (+) (plantOwnerName p) (plantLength (phenotype p))) M.empty garden

	let texts = map (\(n,s) -> printf "%s: %.1f" (take 20 n) s) $
			reverse $
			sortBy (comparing snd) $
		        (M.toList owernerscore)
	preserve $ do
		scale 1 (-1)
		setSourceRGB 0 0 0
		translate 0 (1.5*groundLevel - h) 

		setFontSize (groundLevel/2)

		forM_ texts $ \text ->  do
			ext <- textExtents text
			rectangle 0
				  (textExtentsYbearing ext + groundLevel/2)
				  (textExtentsXbearing ext + textExtentsXadvance ext)
				  (-textExtentsYbearing ext - groundLevel/2 - groundLevel/2)
			setSourceRGB 1 1 1
			fill

			setSourceRGB 0 0 0
			showText text

			translate 0 (groundLevel/2)


renderSky :: Angle -> Render ()
renderSky angle = do
	-- Clear Background
	setSourceRGB  0 0 (sin angle)
	paint

renderGround :: Render ()
renderGround = do
	setSourceRGB (140/255) (80/255) (21/255)
	rectangle (-1) 0 3 (-groundLevel)
        fill

-- | Wrapper that calls 'save' and 'restore' before and after the argument
preserve :: Render () -> Render ()
preserve r = save >> r >> restore

clockTimeToDouble (TOD s p) = fromIntegral s + fromIntegral p/(1000*1000*1000*1000)
