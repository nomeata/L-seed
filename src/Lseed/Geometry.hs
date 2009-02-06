{-# LANGUAGE ScopedTypeVariables #-}
module Lseed.Geometry where

import Lseed.Data
import Lseed.Constants
import Lseed.Geometry.Generator
import Data.List
import Data.Maybe

import Debug.Trace

type Point = (Double, Double)
type Line  = (Point, Point)

lightFalloff = 0.7

eps = 1e-9

lineLength ((x1,y1),(x2,y2)) = sqrt (x1*x2 + y1*y2)

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


plantedToLines :: Planted -> [Line]
plantedToLines planted = runGeometryGenerator (plantPosition planted, 0) 0 $
		plantToGeometry (phenotype planted)

plantToGeometry :: Plant -> GeometryGenerator ()
plantToGeometry Bud = return ()
plantToGeometry (Stipe p) = addLine ((0,0),(0,stipeLength)) >>
			    translated (0,stipeLength) (plantToGeometry p)
plantToGeometry (Fork p1 p2) = rotated (-pi/4) (plantToGeometry p1) >>
                               rotated ( pi/4) (plantToGeometry p2)

-- | Lines are annotated with its plant, identified by the position
gardenToLines :: Garden -> [(Line, Double)]
gardenToLines = concatMap (\planted -> map (\line -> (line, plantPosition planted)) (plantedToLines planted))

-- | Add lightning from a given angle
lightenLines :: forall a. (Ord a, Show a) => Double -> [(Line, a)] -> [(Line, a, Double)]
lightenLines angle lines = lighted
  where projectLine :: Line -> (Double, Double)
        projectLine (p1, p2) = (projectPoint p1, projectPoint p2)
	projectPoint :: Point -> Double
	projectPoint (x,y) = x + y * 1 / tan (pi-angle)
	
	-- False means Beginning of Line
	sweepPoints :: [(Double, Bool, (Line, a))]
	sweepPoints = sort $ concatMap (\l@((p1,p2),i) -> 
			if projectPoint p1 == projectPoint p2
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

	lighted :: [(Line, a, Double)]
	lighted = foldl go unlighted intervals
	  where go llines (x1,x2) = curlines' ++ otherlines
		  where -- Calculation based on the ray at the mid point
			mid = (x1 + x2) / 2
			-- Light intensity
			width = (x2 - x1) * sin angle
			(curlines, otherlines) = partition (\(l,_,_) ->
					let (x1',x2') = projectLine l
                                        in x1' <= mid && mid <= x2' ||
                                           x2' <= mid && mid <= x1'
				        ) llines
			sorted = sortBy aboveFirst curlines
			crossPoint (p1@(x1,y1),p2@(x2,y2)) = 
				let t = (mid - projectPoint p1) /
                                        (projectPoint p2 - projectPoint p1)
				in (x1 + t * (x2-x1), y1 + t * (y2-y1))
			aboveFirst (l1,_,_) (l2,_,_) =
				let (_,y1) = crossPoint l1
				    (_,y2) = crossPoint l2
				in y2 `compare` y1
			curlines' = snd $ mapAccumL shine 1 sorted
			shine intensity (l,i,amount) = ( intensity * lightFalloff
						       , (l,i,amount + intensity * width))
{-
-- Yay, this is a sweep-line-algorithm from Kognitive Systeme, whe would have guessed
mergeLines lines = catMaybes $ snd $ mapAccumL step (Nothing, 0) points
 where points = sort $ concatMap (
                \(p1,p2) -> if p1 < p2 then [(p1,False),(p2,True)] else [(p2,False),(p1,True)]
                ) lines
       step (Nothing, 0) (p, False) = ((Just p, 1),    Nothing)
       step (Nothing, 0) (p, True)  = error $ "End before start, point " ++ show p
       step (Just p1, 1) (p2,True)  = ((Nothing, 0),   Just (p1,p2))
       step (Just p1, n) (_, False) = ((Just p1, n+1), Nothing)
       step (Just p1, n) (_, True)  = ((Just p1, n-1), Nothing)


-}
