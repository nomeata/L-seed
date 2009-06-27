module Lseed.LSystem where

import Lseed.Constants
import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random
import Control.Arrow (second)
import Data.List

applyLSystem :: RandomGen g => g -> GrammarFile -> AnnotatedPlant -> GrowingPlant
applyLSystem rgen rules plant = let (maxPrio, result) = go rgen maxPrio plant -- great use of lazyness here
                                in  result
  where go :: RandomGen g => g -> Int -> AnnotatedPlant -> (Int, GrowingPlant)
 	go rgen maxPrio p@(Plant { pUserTag = oldUt
		            , pLength = oldSize
		            , pAngle = ang
		            , pBranches = ps
		            })
		= let choices = map applyRule $
			        filter (\r -> p `conformsTo` grCondition r) $
			        rules
		  in ( maximum (0 : subPrios ++ map fst choices)
		     , case filter (isValid . snd) $
		     	    map snd $
		            filter ((>= maxPrio) . fst) $
			    choices
		       of []       -> noAction
		          choices' -> chooseWeighted rgen' choices'
		     )
	  where applyRule :: GrammarRule -> (Int, (Int, GrowingPlant))
	  	applyRule r = (grPriority r, (grWeight r, applyAction (grAction r)))
	  
	  	applyAction :: GrammarAction -> GrowingPlant
	  	applyAction (SetLength mut ld)
			= p { pData    = EnlargingTo (calcLengthDescr ld oldSize)
			    , pUserTag = fromMaybe oldUt mut
			    , pBranches = ps'
			    }
	  	applyAction (AddBranches mut pos branches) 
			| 1-pos < eps -- Fork at the end
			= p { pData = NoGrowth
			    , pUserTag = ut
			    , pBranches = ps' ++ newForks}
			| otherwise -- Fork not at the end
			= Plant NoGrowth (oldSize * pos) ang ut $
			  [ Plant NoGrowth (oldSize * (1-pos)) 0 ut ps' ] ++
			  newForks
		 where	ut = fromMaybe oldUt mut
			newForks = map (\(angle, newSize, ut) -> Plant (EnlargingTo newSize) 0 angle (fromMaybe oldUt ut) []) branches
		applyAction (Blossom mut) 
			= p { pData = GrowingSeed 0
			    , pUserTag = fromMaybe oldUt mut
			    , pBranches = ps'
			    }
	
		noAction = p { pData = NoGrowth, pBranches = ps' }
		(rgen':rgens) = unfoldr (Just . split) rgen
		(subPrios, ps') = unzip $ zipWith (\r -> go r maxPrio) rgens ps

	-- Some general checks to rule out unwanted rules
	isValid :: GrowingPlant -> Bool
	isValid (Plant newSize oldSize ang ut ps) = anglesOk
	  where angles = sort $ map pAngle ps
		-- Are all angles not too close to each other?
                anglesOk = all (> minAngle) (zipWith (flip (-)) angles (tail angles))

chooseWeighted _    []   = error "Can not choose from an empty list"
chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen



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

-- | Length reductions are silenty turned into no-ops
calcLengthDescr :: LengthDescr -> Double -> Double
calcLengthDescr (Absolute val) l  = max l val
calcLengthDescr (Additional val) l = max l (l + val)
calcLengthDescr (AdditionalRelative val) l = max l (l + l * (val/100))

