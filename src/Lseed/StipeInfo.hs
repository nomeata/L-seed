module Lseed.StipeInfo where

import Lseed.Data
import Lseed.Data.Functions
import Lseed.Geometry

annotateGarden :: Angle -> GrowingGarden -> AnnotatedGarden
annotateGarden angle  = map (mapPlanted annotatePlant) . lightenGarden angle

annotatePlant :: Plant (GrowthState, Double) -> AnnotatedPlant
annotatePlant = go 0
  where go d (Plant (gs, light) len ang ut ps) = Plant (StipeInfo
		{ siLength    = len
		, siSubLength = len + sum (map (siSubLength . pData) ps')
		, siLight     = light
		, siSubLight  = light + sum (map (siSubLight . pData) ps')
		, siAngle     = ang
		, siDirection = normAngle d'
		, siGrowth    = gs
		}) len ang ut ps'
	  where ps' = map (go d') ps
	  	d' = (d+ang)

normAngle a = a - fromIntegral (truncate ((a+pi) / (2*pi))) * 2*pi
