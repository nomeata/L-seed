-- | Data definitions for L-seed
module Lseed.Data where 

import Data.Foldable (Foldable, fold)
import Data.Monoid

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

-- Instances
instance Functor Plant where
	fmap f (Bud x) = Bud (f x)
	fmap f (Stipe x len p1) = Stipe (f x) len (fmap f p1)
	fmap f (Fork x angle p1 p2) = Fork (f x) angle (fmap f p1) (fmap f p2)

instance Foldable Plant where
	fold (Bud x) = x
	fold (Stipe x len p1) = x `mappend` fold p1
	fold (Fork x angle p1 p2) = x `mappend` fold p1 `mappend` fold p2

instance Functor Planted where
	fmap f planted = planted { phenotype = fmap f (phenotype planted) }
