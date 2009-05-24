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
type GrowingGarden = Garden (Maybe Double)
type AnnotatedGarden = Garden StipeInfo

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
	-- | a stipe with a length (factor of stipeLength)
	--   and a list of plants sprouting at the end, at a given radial angle.
	= Stipe a Double [ (Double, Plant a) ]
	deriving (Show)

mapSprouts :: (Plant a -> Plant b) -> [ (Double, Plant a) ] -> [ (Double, Plant b) ]
mapSprouts = map . second

-- | Named variants of a Plant, for more expressive type signatures
type GrowingPlant = Plant (Maybe Double)

data StipeInfo = StipeInfo
	{ siLength    :: Double -- ^ a bit redundant, but what shells
	, siSubLength :: Double
	, siLight     :: Double
	, siSubLight  :: Double
	, siAngle     :: Angle
	, siDirection :: Angle
	}
	deriving (Show)

type AnnotatedPlant = Plant StipeInfo

-- | Possible action to run on a Stipe in a Rule
data LRuleAction
	= EnlargeStipe Double -- ^ Extend this Stipe to the given length
        | ForkStipe Double [(Angle, Double)] -- ^ Branch this stipe at the given fraction and angles and let it grow to the given lengths
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

-- | Main loop observers
data Observer = Observer
	{ obInit :: IO ()
	, obState :: Integer -> GrowingGarden -> IO ()
	, obGrowingState :: (ClockTime -> ScreenContent) -> IO ()
	, obFinished :: GrowingGarden -> IO ()
	}
nullObserver = Observer (return ()) (\_ _ -> return ()) (\_ -> return ()) (\_ -> return ())

-- Instances
instance Functor Plant where
	fmap f (Stipe x len ps) = Stipe (f x) len (map (second (fmap f)) ps)

instance Foldable Plant where
	fold (Stipe x len ps) = x `mappend` (mconcat $ map (fold.snd) ps)

instance Traversable Plant where
	sequenceA (Stipe x len ps) =
		Stipe <$> x <*> pure len <*> sequenceA (map (\(a,p) -> (,) a <$> sequenceA p) ps)

instance Functor Planted where
	fmap f planted = planted { phenotype = fmap f (phenotype planted) }

instance Foldable Planted where
	fold planted = fold (phenotype planted)

instance Traversable Planted where
	sequenceA planted = (\x -> planted { phenotype = x }) <$> sequenceA (phenotype planted)
