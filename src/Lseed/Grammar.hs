-- | Grammer-like representation for a plant genome
module Lseed.Grammar where

import Lseed.Data

-- | A complete grammar file
type GrammarFile = [ GrammarRule ]

type Priority = Int
type Weight = Int

type UserTag = String

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
	| MatchTotalLight
	| MatchLength
	| MatchTotalLength
	| MatchDirection
	| MatchAngle
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
	= SetLength LengthDescr (Maybe UserTag)
	| AddBranch Double Angle Double (Maybe UserTag)
	deriving (Read,Show)

data LengthDescr = Absolute Double
	         | Additional Double
                 | AdditionalRelative Double -- ^ in Percent
	deriving (Read,Show)
