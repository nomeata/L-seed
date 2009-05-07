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
	then Just ({- grPriority rule, -}grWeight rule, grToLAction (grActions rule) plant)
	else Nothing


conformsTo :: Plant () -> Condition -> Bool
conformsTo (Stipe () l _) = go
  where go (Always b)     = b
	go (c1 `And` c2)  = go c1 && go c2
	go (c1 `Or` c2)   = go c1 || go c2
	go (UserTagIs ut) = error "UserTags are not supported yet"
	go (NumCond what how val) = doCompare how (getMatchable what) val
	
	getMatchable MatchLength = l
	getMatchable m		 = error $ "Matchable " ++ show m ++ " not supported yet"

	doCompare LE = (<=)
	doCompare Less = (<)
	doCompare Equals = (==)
	doCompare Greater = (>)
	doCompare GE = (>=)

grToLAction :: [GrammarAction] -> Plant () -> LRuleAction
grToLAction [SetLength ld _] (Stipe () l _)
	= EnlargeStipe (calcLengthDescr ld l)
grToLAction acts  (Stipe () l _)
	| all isAddBranch acts
	= case nub (map addBranchAngle acts) of
	    [frac] -> ForkStipe frac $ map (\(AddBranch _ angle length _) -> (angle, length)) acts
	    _ -> error "Can not branch at different points at the same time"
	| otherwise
	= error "Can not grow and branch at the same time"

-- | Length reductions are silenty turned into no-ops
calcLengthDescr :: LengthDescr -> Double -> Double
calcLengthDescr (Absolute val) l  = max l val
calcLengthDescr (Additional val) l = max l (l + val)
calcLengthDescr (AdditionalRelative val) l = max l (l + l * (val/100))

