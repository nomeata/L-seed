-- | Code to turn a 'Lseed.Grammar' into a 'LSystem'
module Lseed.Grammar.Compile where

import Lseed.Data
import Lseed.Grammar
import Data.List (nub)
import Data.Maybe(fromMaybe)

compileGrammarFile :: GrammarFile -> LSystem
compileGrammarFile = map compileGrammarRule

compileGrammarRule :: GrammarRule -> LRule
compileGrammarRule rule plant = 
	if   plant `conformsTo` grCondition rule
	then Just ({- grPriority rule, -}grWeight rule, grToLAction (grAction rule) plant)
	else Nothing


conformsTo :: AnnotatedPlant -> Condition -> Bool
conformsTo (Plant {pData = si, pUserTag = ut}) = go
  where go (Always b)     = b
	go (c1 `And` c2)  = go c1 && go c2
	go (c1 `Or` c2)   = go c1 || go c2
	go (UserTagIs ut') = ut' == ut
	go (NumCond what how val) = doCompare how (getMatchable what) val
	
	getMatchable MatchLength    = siLength si
	getMatchable MatchSubLength = siSubLength si
	getMatchable MatchLight     = siLight si
	getMatchable MatchSubLight  = siSubLight si
	getMatchable MatchDirection = siDirection si
	getMatchable MatchAngle     = siAngle si

	doCompare LE = (<=)
	doCompare Less = (<)
	doCompare Equals = (==)
	doCompare Greater = (>)
	doCompare GE = (>=)

grToLAction :: GrammarAction -> AnnotatedPlant -> LRuleAction
grToLAction (SetLength mut ld) (Plant { pLength = l, pUserTag = oldUt })
	= EnlargeStipe (fromMaybe oldUt mut) (calcLengthDescr ld l)
grToLAction (AddBranches mut frac branches) (Plant { pLength = l, pUserTag = oldUt })
	= ForkStipe (fromMaybe oldUt mut) frac $
		map (\(a,b,c) -> (a,b,fromMaybe oldUt c)) branches
grToLAction Blossom _ 
	= DoBlossom

-- | Length reductions are silenty turned into no-ops
calcLengthDescr :: LengthDescr -> Double -> Double
calcLengthDescr (Absolute val) l  = max l val
calcLengthDescr (Additional val) l = max l (l + val)
calcLengthDescr (AdditionalRelative val) l = max l (l + l * (val/100))

