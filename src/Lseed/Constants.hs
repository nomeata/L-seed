-- |
-- This module contians the main nobs to control the game play.
--
-- All length data is relative to the screen width.
module Lseed.Constants where 

groundLevel :: Double
groundLevel = 0.03
budSize :: Double
budSize     = 0.01
stipeLength :: Double
stipeLength = 0.05
blossomSize :: Double
blossomSize = 0.03
stipeWidth :: Double
stipeWidth  = 0.01

-- | Light and growths interpolation frequency
ticksPerDay :: Integer
ticksPerDay = 9

-- | Plant length growth per Day and Light
--
-- 1 means: Can grow one stipeLength during one day, when catching the sunlight
-- with one branch of (projected) length screenwidth
growthPerDayAndLight :: Double
growthPerDayAndLight = 25.0

-- | Plants up to this size get an boost in growths
smallPlantBoostSize :: Double
smallPlantBoostSize = 0.5

-- | Minimum growths for plants of size less then smallPlantBoostSize
smallPlantBoostLength :: Double
smallPlantBoostLength = 0.2

-- | Cost (in light units) per (sum for all branches (length * distance), to limit the growth of the plants
costPerLength :: Double
costPerLength = 0.001

-- | Cost (in length growths equivalent) per seed to be grown
seedGrowthCost :: Double
seedGrowthCost = 1.0

-- | Branch translucency. Proportion of light that is let through by a plant
lightFalloff :: Double
lightFalloff = 0.6

-- | Length of one day, in seconds
dayLength :: Double
dayLength = 15

-- | ε
eps = 1e-9

-- | Minimum radial angular distance between two branches
minAngle :: Double
minAngle = pi/20

-- | Derived constants
tickLength = dayLength / fromIntegral ticksPerDay
