module Lseed.Constants where 

-- All relative to the screen width
groundLevel = 0.03
budSize     = 0.01
stipeLength = 0.05
blossomSize = 0.03
stipeWidth  = 0.01

-- | Light and growths interpolation frequency
ticksPerDay = 9

-- | Plant length growth per Day and Light
--
-- 1 means: Can grow one stipeLength during one day, when catching the sunlight with one branch of (projected) length screenwidth
growthPerDayAndLight = 15.0

-- | Plants up to this size get an boost in growths
smallPlantBoostSize = 0.5

-- | Minimum growths for plants of size less then smallPlantBoostSize
smallPlantBoostLength = 0.2

-- | Cost (in light units) per (length for maintaining the plant)^2, to limit the growth of the plants
costPerLength = 0.001

-- | Cost (in length growths equivalent) per seed to be grown
seedGrowthCost = 1.0

-- | Length of one day, in seconds
dayLength = 10

-- | ε
eps = 1e-9

-- | Minimum radial angular distance between two branches
minAngle = pi/20

-- | Derived constants
tickLength = fromIntegral dayLength / fromIntegral ticksPerDay
