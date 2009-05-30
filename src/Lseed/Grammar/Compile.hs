-- | Code to turn a 'Lseed.Grammar' into a 'LSystem'
module Lseed.Grammar.Compile where

import Lseed.Data
import Lseed.Grammar
import Data.List (nub)

compileGrammarFile :: GrammarFile -> LSystem
compileGrammarFile = map compileGrammarRule

compileGrammarRule :: GrammarRule -> LRule
compileGrammarRule rule plant = 
	if   plant `conformsTo` grCondition rule
	then Just ({- grPriority rule, -}grWeight rule, grToLAction (grAction rule) plant)
	else Nothing


conformsTo :: AnnotatedPlant -> Condition -> Bool
conformsTo (Plant {pData = si}) = go
  where go (Always b)     = b
	go (c1 `And` c2)  = go c1 && go c2
	go (c1 `Or` c2)   = go c1 || go c2
	go (UserTagIs ut) = error "UserTags are not supported yet"
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
grToLAction (SetLength _ ld) (Plant _ l _ _)
	= EnlargeStipe (calcLengthDescr ld l)
grToLAction (AddBranches _ frac branches) (Plant _ l _ _)
	= ForkStipe frac $ map (\(angle,length,_) -> (angle, length)) branches

-- | Length reductions are silenty turned into no-ops
calcLengthDescr :: LengthDescr -> Double -> Double
calcLengthDescr (Absolute val) l  = max l val
calcLengthDescr (Additional val) l = max l (l + val)
calcLengthDescr (AdditionalRelative val) l = max l (l + l * (val/100))

