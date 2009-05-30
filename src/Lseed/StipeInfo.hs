module Lseed.StipeInfo where

import Lseed.Data
import Lseed.Data.Functions
import Lseed.Geometry

annotatePlant :: Plant Double -> AnnotatedPlant
annotatePlant = go 0
  where go d (Plant light len ang ps) = Plant (StipeInfo
		{ siLength    = len
		, siSubLength = len + sum (map (siSubLength . pData) ps')
		, siLight     = light
		, siSubLight  = light + sum (map (siSubLight . pData) ps')
		, siAngle     = ang
		, siDirection = normAngle d'
		}) len ang ps'
	  where ps' = map (go d') ps
	  	d' = (d+ang)

normAngle a = a - fromIntegral (truncate ((a+pi) / (2*pi))) * 2*pi
