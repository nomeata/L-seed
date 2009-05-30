{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module Lseed.Geometry where

import Lseed.Data
import Lseed.Data.Functions
import Lseed.Constants
import Lseed.Geometry.Generator
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Control.Monad hiding (mapM,forM)
import Data.Traversable (mapM,forM)
import Prelude hiding (mapM)
import Control.Monad.ST
import Data.STRef

type Point = (Double, Double)
type Line  = (Point, Point)

lightFalloff = 0.7

lineLength ((x1,y1),(x2,y2)) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- | from http://www.pdas.com/lineint.htm
crossPoint :: Line -> Line -> Maybe Point
crossPoint ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
	let a1 = y2-y1
	    b1 = x1-x2
	    c1 = x2*y1 - x1*y2  -- { a1*x + b1*y + c1 = 0 is line 1 }
            a2 = y4-y3
            b2 = x3-x4
            c2 = x4*y3 - x3*y4  -- { a2*x + b2*y + c2 = 0 is line 2 }
	    denom = a1*b2 - a2*b1
	in if abs denom > eps
           then let x = (b1*c2 - b2*c1)/denom
		    y = (a2*c1 - a1*c2)/denom
		in if  x1 <= x && x <= x2 &&
		       y1 <= y && y <= y2 &&
		       x3 <= x && x <= x4 &&
		       y3 <= y && y <= y4
		   then Just (x,y)
                   else Nothing
           else Nothing


plantedToLines :: Planted a -> [(Line, a)]
plantedToLines planted = runGeometryGenerator (plantPosition planted, 0) 0 $
		plantToGeometry (phenotype planted)

plantToGeometry :: Plant a -> GeometryGenerator a ()
plantToGeometry (Plant x len ang _ ps) = rotated ang $ do
		addLine x ((0,0),(0,len * stipeLength))
		translated (0,len * stipeLength) $ mapM_ plantToGeometry ps

-- | Lines are annotated with its plant, identified by the extra data
gardenToLines :: Garden a -> [(Line, a)]
gardenToLines = concatMap (\planted -> plantedToLines planted)

-- | Add lightning from a given angle
lightenLines :: Double -> [(Line, a)] -> [(Line, a, Double)]
lightenLines angle lines = let (lighted,_) = allKindsOfStuffWithAngle angle lines
                           in lighted

lightPolygons :: Double -> [(Line, a)] -> [(Point,Point,Point,Point,Double)]
lightPolygons angle lines = let (_,polygons) = allKindsOfStuffWithAngle angle lines
			    in polygons

allKindsOfStuffWithAngle :: forall a. Double -> [(Line, a)] ->
			    ( [(Line, a, Double)]
	                    , [(Point,Point,Point,Point,Double)] )
allKindsOfStuffWithAngle angle lines = (lighted, polygons)
  where projectLine :: Line -> (Double, Double)
        projectLine (p1, p2) = (projectPoint p1, projectPoint p2)
	projectTan :: Double
	projectTan = 1 / tan (pi-angle)
	projectPoint :: Point -> Double
	projectPoint (x,y) = x + y * projectTan
	
	-- False means Beginning of Line
	sweepPoints :: [(Double, Bool, (Line, a))]
	sweepPoints = sortBy (comparing (\(a,b,_)->(a,b))) $ concatMap (\l@((p1,p2),i) -> 
			if abs (projectPoint p1 - projectPoint p2) < eps
			then []
			else if projectPoint p1 < projectPoint p2
			     then [(projectPoint p1,False,l), (projectPoint p2,True,l)]
			     else [(projectPoint p2,False,l), (projectPoint p1,True,l)]
		) lines

	-- Find all crossing points
	crossings :: [Double]
	crossings = case mapAccumL step [] sweepPoints of
			([],crosses) -> nub (sort (concat crosses))
			_            -> error "Lines left open after sweep"
	  where	-- accumulator is open lines, return is list of cross points
		step :: [Line] -> (Double, Bool, (Line, a)) -> ([Line], [Double])
		step [] (_, True, _)      = error $ "Line ends with no lines open"
		-- Beginning of a new line, mark it as open, and mark it as a cross-point
		step ol (x, False, (l,_)) = (l:ol, [x]) 
		-- End of a line. Calculate crosses with all open line, and remove it from the
		-- list of open lines
		step ol (x, True, (l,_)) = 
			let ol' = filter (/= l) ol
			    crosses = map projectPoint $ mapMaybe (crossPoint l) ol'
			in (ol', x:crosses)

	-- Cross points inverval
	intervals = zip crossings (tail crossings)

	unlighted = map (\(l,i) -> (l,i,0)) lines
	
	unprojectPoint x (p1@(x1,y1),p2@(x2,y2)) = 
		let t = (x - projectPoint p1) /
			(projectPoint p2 - projectPoint p1)
		in (x1 + t * (x2-x1), y1 + t * (y2-y1))

	lineAtRay x l = let (x1',x2') = projectLine l
                      in abs (x1' - x2') > eps && -- avoid lines that parallel to the rays
		         (x1' <= x && x <= x2' || x2' <= x && x <= x1')

	aboveFirst x l1 l2 =
		let (_,y1) = unprojectPoint x l1
		    (_,y2) = unprojectPoint x l2
		in y2 `compare` y1

	lighted :: [(Line, a, Double)]
	lighted = foldl go unlighted intervals
	  where go llines (x1,x2) = curlines' ++ otherlines
		  where -- Calculation based on the ray at the mid point
			mid = (x1 + x2) / 2
			-- Light intensity
			width = abs ((x2 - x1) * sin angle)
			(curlines, otherlines) = partition (\(l,_,_) -> lineAtRay mid l)
							   llines
			sorted = sortBy (\(l1,_,_) (l2,_,_) -> aboveFirst mid l1 l2)
                                        curlines
			curlines' = snd $ mapAccumL shine 1 sorted
			shine intensity (l,i,amount) = ( intensity * lightFalloff
						       , (l,i,amount + intensity * width))

	polygons = concatMap go intervals
	  where go (x1,x2) = if null sorted then [nothingPoly] else lightedPolys
		  where mid = (x1 + x2) / 2
			curlines = filter (lineAtRay mid) (map fst lines)
			sorted = sortBy (aboveFirst mid) curlines
			ceiling = ((0,10),(1,10))
			floor = ((0,0),(1,0))
			nothingPoly = let p1 = unprojectPoint x1 ceiling
                                          p2 = unprojectPoint x1 floor
                                          p3 = unprojectPoint x2 floor
                                          p4 = unprojectPoint x2 ceiling
                                      in (p1,p2,p3,p4,1)
			firstPoly = let p1 = unprojectPoint x1 ceiling
                                        p2 = unprojectPoint x1 (head sorted)
                                        p3 = unprojectPoint x2 (head sorted)
                                        p4 = unprojectPoint x2 ceiling
                                    in (p1,p2,p3,p4)
			lastPoly =  let p1 = unprojectPoint x1 (last sorted)
                                        p2 = unprojectPoint x1 floor
                                        p3 = unprojectPoint x2 floor
                                        p4 = unprojectPoint x2 (last sorted)
                                    in (p1,p2,p3,p4)
			polys = zipWith (\l1 l2 ->
                                         let p1 = unprojectPoint x1 l1
                                             p2 = unprojectPoint x1 l2
                                             p3 = unprojectPoint x2 l2
                                             p4 = unprojectPoint x2 l1
					 in (p1,p2,p3,p4)) sorted (tail sorted)
			polys' = [firstPoly] ++ polys ++ [lastPoly]
			lightedPolys = snd $ mapAccumL shine 1 polys'
			shine intensity (p1,p2,p3,p4) = ( intensity * lightFalloff
							, (p1,p2,p3,p4,intensity))

-- | Annotates each piece of the garden with the amount of line it attacts
lightenGarden :: Double -> Garden a -> Garden Double
lightenGarden angle = mapLine (lightenLines angle) 0 (+) 


-- | Helper to apply a function that works on lines to a garden
mapLine :: (forall b. [(Line, b)] -> [(Line, b, c)]) ->
           c -> (c -> c -> c) -> Garden a -> Garden c
mapLine process init combine garden = runST $ do
	gardenWithPointers <- mapM (mapM (const (newSTRef init))) garden
	let linesWithPointers = gardenToLines gardenWithPointers
	let processedLines = process linesWithPointers
	-- Update values via the STRef
	forM_ processedLines $ \(_,stRef,result) -> modifySTRef stRef (combine result)
	-- Undo the STRefs
	mapM (mapM readSTRef) gardenWithPointers

