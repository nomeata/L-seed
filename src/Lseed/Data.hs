-- | Data definitions for L-seed
module Lseed.Data where 

-- | A list of plants, together with their position in the garden, in the interval [0,1]
type Garden = [ Planted ]

-- | A plant with metainformatoin
data Planted = Planted
	{ plantPosition :: Double -- ^ Position in the garden, interval [0,1]
	, genome :: LSystem  -- ^ Lsystem in use
	, phenotype :: Plant -- ^ Actual current form of the plant
	}

-- | A plant, which is
data Plant 
	= Bud -- ^ a bud, i.e. the end of a sprout
	| Stipe Plant -- ^ a stipe with more of the plant next
	| Fork Plant Plant -- ^ a fork with two successing pieces of a plant
	deriving (Show)

-- | A (compiled) rule of an L-system, with a matching function and a weight
type LRule = (Int, Plant -> Maybe Plant)

-- | An complete LSystem 
type LSystem = [LRule]
