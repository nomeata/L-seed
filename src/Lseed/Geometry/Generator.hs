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


newtype GeometryGenerator x a = GeometryGenerator (ReaderT (Point, Double) (Writer [(Line, x)]) a)
 deriving (Monad)

transformed :: Point -> GeometryGenerator x Point
transformed (x,y) = GeometryGenerator $ do
	((bx,by),r) <- ask
	let (x', y') = (cos r * x + sin r *y, -sin r * x + cos r *y)
	return (bx + x', by + y')

translated :: Point -> GeometryGenerator x a -> GeometryGenerator x a
translated p (GeometryGenerator act) = do
	(x',y') <- transformed p
	GeometryGenerator $
		local (\(_,r) -> ((x',y'),r)) act

rotated :: Double -> GeometryGenerator x a -> GeometryGenerator x a
rotated r (GeometryGenerator act) = 
	GeometryGenerator $ local (\(p,r') -> (p, r' - r)) act

addLine :: x -> Line -> GeometryGenerator x ()
addLine x (p1,p2) = do
	p1' <- transformed p1
	p2' <- transformed p2
	GeometryGenerator $ tell [((p1', p2'),x)]

	
runGeometryGenerator :: Point -> Double -> GeometryGenerator x () -> [(Line, x)]
runGeometryGenerator p r (GeometryGenerator gen) = 
	execWriter (runReaderT gen (p,r))
