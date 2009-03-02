-- | Data definitions for L-seed
module Lseed.Data where 

import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative ((<$>),(<*>),pure)
import Data.Monoid

-- | A list of plants, together with their position in the garden, in the interval [0,1]
type Garden a = [ Planted a ]

-- | Named variants of a garden, for more expressive type signatures
type GrowingGarden = Garden (Maybe Double)

-- | A plant with metainformatoin
data Planted a = Planted
	{ plantPosition :: Double -- ^ Position in the garden, interval [0,1]
	, genome        :: LSystem  -- ^ Lsystem in use
	, phenotype     :: Plant a -- ^ Actual current form of the plant
	}

-- | Named variants of a Planted, for more expressive type signatures
type GrowingPlanted = Planted (Maybe Double)

-- | A plant, which is
data Plant a 
	-- | a bud, i.e. the end of a sprout
	= Bud
	-- | a stipe with a length (factor of stipeLength)
	--   and more of the plant next
	| Stipe a Double (Plant a)
	-- ^ a fork with a sidewise offspring at a radial angle,
	--   and a straight continuation 
	| Fork Double (Plant a) (Plant a)
	deriving (Show)

-- | Named variants of a Plant, for more expressive type signatures
type GrowingPlant = Plant (Maybe Double)

-- | Possible action to run on a Stipe in a Rule
data LRuleAction
	= EnlargeStipe Double -- ^ Extend this Stipe to the given length
        | ForkStipe Double [(Angle, Double)] -- ^ Branch this stipe at the given fraction and angle and let it grow to the given lengths

-- | A (compiled) rule of an L-system, with a matching function returning an action and weight
type LRule = (Plant () -> Maybe (Int, LRuleAction))

-- | An complete LSystem 
type LSystem = [LRule]

-- | Representation of what is on screen
data ScreenContent = ScreenContent
	{ scGarden     :: Garden ()
	, scLightAngle :: Double
	, scTime       :: String
	}

-- | Light angle
type Angle = Double

-- Instances
instance Functor Plant where
	fmap f Bud = Bud
	fmap f (Stipe x len p1) = Stipe (f x) len (fmap f p1)
	fmap f (Fork angle p1 p2) = Fork angle (fmap f p1) (fmap f p2)

instance Foldable Plant where
	fold Bud  = mempty
	fold (Stipe x len p1) = x `mappend` fold p1
	fold (Fork angle p1 p2) = fold p1 `mappend` fold p2

instance Traversable Plant where
	sequenceA Bud =
		pure Bud
	sequenceA (Stipe x len p1) =
		Stipe <$> x <*> pure len <*> sequenceA p1
	sequenceA (Fork angle p1 p2) =
		Fork <$> pure angle <*> sequenceA p1 <*> sequenceA p2
	

instance Functor Planted where
	fmap f planted = planted { phenotype = fmap f (phenotype planted) }

instance Foldable Planted where
	fold planted = fold (phenotype planted)

instance Traversable Planted where
	sequenceA planted = (\x -> planted { phenotype = x }) <$> sequenceA (phenotype planted)
