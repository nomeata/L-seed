module Lseed.StipeInfo where

import Lseed.Data
import Lseed.Data.Functions
import Lseed.Geometry

annotatePlant :: Plant Double -> AnnotatedPlant
annotatePlant = go 0 0
  where go a d (Stipe light len ps) = Stipe (StipeInfo
		{ siLength    = len
		, siSubLength = len + sum (map (siSubLength . extractOutmost . snd) ps')
		, siLight     = light
		, siSubLight  = light + sum (map (siSubLight . extractOutmost . snd) ps')
		, siAngle     = a
		, siDirection = normAngle d
		}) len ps'
	  where ps' = map (\(a',p) -> (a', go a' (d+a') p)) ps

normAngle a = a - fromIntegral (truncate ((a+pi) / (2*pi))) * 2*pi
