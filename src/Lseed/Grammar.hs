-- | Grammer-like representation for a plant genome
module Lseed.Grammar where

import Lseed.Data
import Data.List

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
	, grActions :: [GrammarAction]
	}
	deriving (Read,Show)

data Matchable
	= MatchLight
	| MatchSubLight
	| MatchLength
	| MatchSubLength
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

actionsAreInvalid :: [GrammarAction] -> Maybe String
actionsAreInvalid [_] = Nothing
actionsAreInvalid acts
	= if all isAddBranch acts 
          then case nub (map addBranchAngle acts) of
	    [frac] -> Nothing
	    _      -> Just "Can not branch at different points at the same time."
	  else        Just "Can not grow and branch at the same time."

isAddBranch (AddBranch _ _ _ _) = True
isAddBranch _ = False

addBranchAngle (AddBranch angle _ _ _) = angle

