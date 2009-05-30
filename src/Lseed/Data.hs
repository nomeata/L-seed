-- | Data definitions for L-seed
module Lseed.Data where 

import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative ((<$>),(<*>),pure)
import Control.Arrow (second)
import Data.Monoid
import System.Time (ClockTime)

-- | A list of plants, together with their position in the garden, in the interval [0,1]
type Garden a = [ Planted a ]

-- | Named variants of a garden, for more expressive type signatures
type GrowingGarden = Garden GrowthState
type AnnotatedGarden = Garden StipeInfo

-- | A plant with metainformatoin
data Planted a = Planted
	{ plantPosition :: Double -- ^ Position in the garden, interval [0,1]
	, plantOwner    :: Integer -- ^ Id of the user that owns this plant
	, genome        :: LSystem -- ^ Lsystem in use
	, phenotype     :: Plant a -- ^ Actual current form of the plant
	}

-- | Named variants of a Planted, for more expressive type signatures
type GrowingPlanted = Planted GrowthState
type AnnotatedPlanted = Planted StipeInfo

-- | A plant, which is
data Plant a 
	-- | a stipe with a length (factor of stipeLength), an angle relative
	-- to the parent stipe and a list of plants sprouting at the end
	= Plant { pData :: a
		, pLength :: Double
		, pAngle :: Angle
		, pUserTag :: UserTag
		, pBranches :: [ Plant a ]
		}
	deriving (Show)

-- | A straight, untagged plant with length zero and no branches.
inititalPlant = Plant () 0 0 "" []

data StipeInfo = StipeInfo
	{ siLength    :: Double -- ^ a bit redundant, but what shells
	, siSubLength :: Double
	, siLight     :: Double
	, siSubLight  :: Double
	, siAngle     :: Angle
	, siDirection :: Angle
	}
	deriving (Show)

data GrowthState = NoGrowth
		 | EnlargingTo Double -- ^ value indicates the growth target 
		 | GrowingSeed Double -- ^ value indicates the current state [0..1]

-- | Named variants of a Plant, for more expressive type signatures
type GrowingPlant = Plant GrowthState
type AnnotatedPlant = Plant StipeInfo

-- | Possible action to run on a Stipe in a Rule
data LRuleAction
	= EnlargeStipe UserTag Double -- ^ Extend this Stipe to the given length
        | ForkStipe UserTag Double [(Angle, Double, UserTag)] -- ^ Branch this stipe at the given fraction and angles and let it grow to the given lengths
	| DoBlossom -- ^ Start a to grow a new seed
	deriving (Show)

-- | A (compiled) rule of an L-system, with a matching function returning an action and weight
type LRule = (AnnotatedPlant -> Maybe (Int, LRuleAction))

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

-- | User Tag
type UserTag = String

-- | Main loop observers
data Observer = Observer
	-- | Called once, before the main loop starts
	{ obInit :: IO ()
	-- | Called once per tick, with the current tick number and the current
	-- state of the garden
	, obState :: Integer -> GrowingGarden -> IO ()
	-- | Also called once per tick, with a function that calculates the
	-- information that should be displayed given a point in time
	, obGrowingState :: (ClockTime -> ScreenContent) -> IO ()
	-- | Called before the main loop quits, with the last state of the garden
	, obFinished :: GrowingGarden -> IO ()
	}
nullObserver = Observer (return ()) (\_ _ -> return ()) (\_ -> return ()) (\_ -> return ())

-- Instances
instance Functor Plant where
	fmap f p = p { pData = f (pData p)
		     , pBranches = map (fmap f) (pBranches p)
		     }

instance Foldable Plant where
	fold p = pData p `mappend` (mconcat $ map fold (pBranches p))

instance Traversable Plant where
	sequenceA (Plant x len ang ut ps) =
		Plant <$> x <*> pure len <*> pure ang <*> pure ut <*>
			sequenceA (map sequenceA ps)

instance Functor Planted where
	fmap f planted = planted { phenotype = fmap f (phenotype planted) }

instance Foldable Planted where
	fold planted = fold (phenotype planted)

instance Traversable Planted where
	sequenceA planted = (\x -> planted { phenotype = x }) <$> sequenceA (phenotype planted)
