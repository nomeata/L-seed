module Lseed.Geometry where

import Lseed.Data
import Lseed.Constants
import Lseed.Geometry.Generator

type Point = (Double, Double)
type Line  = (Point, Point)

plantedToLines :: Planted -> [Line]
plantedToLines planted = runGeometryGenerator (plantPosition planted, 0) (pi/2) $
		plantToGeometry (phenotype planted)

plantToGeometry :: Plant -> GeometryGenerator ()
plantToGeometry Bud = return ()
plantToGeometry (Stipe p) = addLine ((0,0),(0,stipeLength)) >>
			    translated (0,stipeLength) (plantToGeometry p)
plantToGeometry (Fork p1 p2) = rotated (-pi/4) (plantToGeometry p1) >>
                               rotated ( pi/4) (plantToGeometry p2)
