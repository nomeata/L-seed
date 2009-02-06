{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Helper module providing a monad that collects lines
module Lseed.Geometry.Generator
	( GeometryGenerator
	, translated
	, rotated
	, runGeometryGenerator
	, addLine
	)
	where

import Control.Monad.Reader
import Control.Monad.Writer

type Point = (Double, Double)
type Line  = (Point, Point)


newtype GeometryGenerator a = GeometryGenerator (ReaderT (Point, Double) (Writer [Line]) a)
 deriving (Monad)

transformed :: Point -> GeometryGenerator Point
transformed (x,y) = GeometryGenerator $ do
	((bx,by),r) <- ask
	let (x', y') = (cos r * x + sin r *y, -sin r * x + cos r *y)
	return (bx + x', by + y')

translated :: Point -> GeometryGenerator a -> GeometryGenerator a
translated p (GeometryGenerator act) = do
	(x',y') <- transformed p
	GeometryGenerator $
		local (\(_,r) -> ((x',y'),r)) act

rotated :: Double -> GeometryGenerator a -> GeometryGenerator a
rotated r (GeometryGenerator act) = 
	GeometryGenerator $ local (\(p,r') -> (p, r' + r)) act

addLine :: Line -> GeometryGenerator ()
addLine (p1,p2) = do
	p1' <- transformed p1
	p2' <- transformed p2
	GeometryGenerator $ tell [(p1', p2')]

	
runGeometryGenerator :: Point -> Double -> GeometryGenerator () -> [Line]
runGeometryGenerator p r (GeometryGenerator gen) = 
	execWriter (runReaderT gen (p,r))
