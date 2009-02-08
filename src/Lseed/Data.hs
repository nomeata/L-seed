-- | Data definitions for L-seed
module Lseed.Data where 

-- | A list of plants, together with their position in the garden, in the interval [0,1]
type Garden a = [ Planted a ]

-- | A plant with metainformatoin
data Planted a = Planted
	{ plantPosition :: Double -- ^ Position in the garden, interval [0,1]
	, genome :: LSystem  -- ^ Lsystem in use
	, phenotype :: Plant a -- ^ Actual current form of the plant
	}

-- | A plant, which is
data Plant a 
	-- | a bud, i.e. the end of a sprout
	= Bud a
	-- | a stipe with a length (factor of stipeLength)
	--   and more of the plant next
	| Stipe a Double (Plant a)
	-- ^ a fork with a sidewise offspring at a radial angle,
	--   and a straight continuation 
	| Fork a Double (Plant a) (Plant a)
	deriving (Show)

-- | A (compiled) rule of an L-system, with a matching function and a weight
type LRule = (Int, Plant () -> Maybe (Plant ()))

-- | An complete LSystem 
type LSystem = [LRule]
