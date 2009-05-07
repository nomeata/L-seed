module Lseed.Constants where 

-- All relative to the screen width
groundLevel = 0.03
budSize     = 0.01
stipeLength = 0.05
stipeWidth  = 0.01

-- | Light and growths interpolation frequency
ticksPerDay = 9

-- | Plant length growth per Day and Light
--
-- 1 means: Can grow one stipeLength during one day, when catching the sunlight with one branch of (projected) length screenwidth
growthPerDayAndLight = 15.0

-- | Default growth (for plants without light)
--growthPerDay = 0.5
growthPerDay = 3.0

-- | Cost (in light units) per (length for maintaining the plant)^2, to limit the growth of the plants
costPerLength = 0.002

-- | Length of one day, in seconds
dayLength = 10 

-- | ε
eps = 1e-9

-- | Minimum radial angular distance between two branches
minAngle = pi/20

-- | Derived constants
tickLength = fromIntegral dayLength / fromIntegral ticksPerDay
