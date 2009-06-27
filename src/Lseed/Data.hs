-- | Data definitions for L-seed
module Lseed.Data where 

import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, sequenceA)
import Control.Applicative ((<$>),(<*>),pure)
import Control.Arrow (second)
import Data.Monoid
import System.Time (ClockTime)
import Data.Monoid

-- | User Tag
type UserTag = String

-- | Light angle
type Angle = Double

-- | A list of plants, together with their position in the garden, in the interval [0,1]
type Garden a = [ Planted a ]

-- | Named variants of a garden, for more expressive type signatures
type GrowingGarden = Garden GrowthState
type AnnotatedGarden = Garden StipeInfo

-- | A plant with metainformatoin
data Planted a = Planted
	{ plantPosition :: Double -- ^ Position in the garden, interval [0,1]
	, plantOwner    :: Integer -- ^ Id of the user that owns this plant
	, plantOwnerName:: String -- ^ Name of the owner of the plant
	, genome        :: GrammarFile -- ^ Lsystem in use
	, phenotype     :: Plant a -- ^ Actual current form of the plant
	}
	deriving (Show)

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
	, siOffset    :: Double -- ^ Sideways position, relative to Plant origin
	, siHeight    :: Double -- ^ Vertical distance from bottom
	, siDistance  :: Double -- ^ Distance from root
	, siGrowth    :: GrowthState
	}
	deriving (Show)

-- | A GrowingPlant can be growing in one of these three ways:
data GrowthState = NoGrowth
		 | EnlargingTo Double -- ^ value indicates the growth target 
		 | GrowingSeed Double -- ^ value indicates the current state [0..1]
	deriving (Show)

-- | Named variants of a Plant, for more expressive type signatures
type GrowingPlant = Plant GrowthState
type AnnotatedPlant = Plant StipeInfo

-- | Representation of what is on screen
data ScreenContent = ScreenContent
	{ scGarden     :: AnnotatedGarden
	, scLightAngle :: Double
	, scTime       :: String
	, scMessage    :: Maybe String
	}

-- | Main loop observers
data Observer = Observer {
	-- | Called once per season, before the main loop starts
	  obInit :: IO ()
	-- | Called once per tick, with the current tick number corresponding
	-- light angle and the current state of the garden
	, obState :: Integer -> Angle -> GrowingGarden -> IO ()
	-- | Also called once per tick, with a function that calculates the
	-- information that should be displayed given a point in time
	, obGrowingState :: (ClockTime -> ScreenContent) -> IO ()
	-- | Called before the main loop quits, with the last state of the garden
	, obFinished :: GrowingGarden -> IO ()
	-- | Called once before program termination
	, obShutdown :: IO ()
	}
nullObserver = Observer (return ()) (\_ _ _ -> return ()) (\_ -> return ()) (\_ -> return ()) (return ())

-- | Methods to get the initial garden and the updated code when a plant multiplies
data GardenSource = GardenSource {
	-- | Called at the beginning of a season, to aquire the garden
	  getGarden :: IO (Garden ())
	-- | Given a plant, returns the genome to be used for a seedling.
	, getUpdatedCode :: Planted () -> IO GrammarFile
	-- | Text to display on the screen
	, getScreenMessage :: IO (Maybe String)
	}
constGardenSource :: Garden () -> GardenSource
constGardenSource garden = GardenSource (return garden) (return . genome) (return Nothing)

-- | A complete grammar file
type GrammarFile = [ GrammarRule ]

type Priority = Int
type Weight = Int

defaultPriority :: Priority
defaultPriority = 0

defaultWeight :: Weight
defaultWeight = 1

-- | A single Rule. For now, only single branches
--   can be matched, not whole subtree structures
data GrammarRule = GrammarRule
	{ grName :: String
	, grPriority :: Priority
	, grWeight :: Weight
	, grCondition :: Condition
	, grAction :: GrammarAction
	}
	deriving (Read,Show)

data Matchable
	= MatchLight
	| MatchSubLight
	| MatchLength
	| MatchSubLength
	| MatchDirection
	| MatchAngle
	| MatchDistance
	deriving (Read,Show)

data Cmp
	= LE
	| Less
	| Equals
	| Greater
	| GE 
	deriving (Read,Show)

data Condition
	= Always Bool -- constant conditions
	| Condition `And` Condition
	| Condition `Or` Condition
	| UserTagIs String
	| NumCond Matchable Cmp Double
	deriving (Read,Show)
	 
data GrammarAction
	= SetLength (Maybe UserTag) LengthDescr
	| AddBranches (Maybe UserTag) Double [(Angle, Double, Maybe UserTag)]
	| Blossom (Maybe UserTag)
	deriving (Read,Show)

data LengthDescr = Absolute Double
	         | Additional Double
                 | AdditionalRelative Double -- ^ in Percent
	deriving (Read,Show)


-- Instances
instance Functor Plant where
	fmap f p = p { pData = f (pData p)
		     , pBranches = map (fmap f) (pBranches p)
		     }

instance Foldable Plant where
	foldMap f p = mconcat $ f (pData p) : map (foldMap f) (pBranches p)

instance Traversable Plant where
	sequenceA (Plant x len ang ut ps) =
		Plant <$> x <*> pure len <*> pure ang <*> pure ut <*>
			sequenceA (map sequenceA ps)

instance Functor Planted where
	fmap f planted = planted { phenotype = fmap f (phenotype planted) }

instance Foldable Planted where
	foldMap f planted = foldMap f (phenotype planted)

instance Traversable Planted where
	sequenceA planted = (\x -> planted { phenotype = x }) <$> sequenceA (phenotype planted)

instance Monoid Observer where
	mempty = nullObserver
	obs1 `mappend` obs2 = nullObserver {
		obInit = obInit obs1 >> obInit obs2,
		obState = \d g -> obState obs1 d g >> obState obs2 d g,
		obGrowingState = \f -> obGrowingState obs1 f >> obGrowingState obs2 f,
		obFinished = \g -> obFinished obs1 g >> obFinished obs2 g,
		obShutdown = obShutdown obs1 >> obShutdown obs2
		}
	
