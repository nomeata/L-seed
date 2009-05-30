module Lseed.LSystem where

import Lseed.Constants
import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random
import Control.Arrow (second)
import Data.List

applyLSystem :: RandomGen g => g -> LSystem -> AnnotatedPlant -> GrowingPlant
applyLSystem rgen rules plant = go plant
  where applyAction :: AnnotatedPlant -> LRuleAction -> GrowingPlant
	applyAction (Plant _ oldSize ang _ ps) (EnlargeStipe ut newSize) 
		= Plant (EnlargingTo newSize) oldSize ang ut $
                  map go ps
	applyAction (Plant _ oldSize ang _ ps) (ForkStipe ut pos [])-- No branches
		= Plant NoGrowth oldSize ang ut $
		  map go ps
	applyAction (Plant _ oldSize ang _ ps) (ForkStipe ut pos branchSpecs)
		| 1-pos < eps -- Fork at the end
		= Plant NoGrowth oldSize ang ut $
			ps' ++
			newForks
		| otherwise -- Fork not at the end
		= Plant NoGrowth (oldSize * pos) ang ut $
			[ Plant NoGrowth (oldSize * (1-pos)) 0 ut ps' ] ++
			newForks
	  where newForks = map (\(angle, newSize, ut) -> Plant (EnlargingTo newSize) 0 angle ut []) branchSpecs
		ps' = map go ps

	noAction (Plant _ oldSize ang ut ps)
		= Plant NoGrowth oldSize ang ut $ map go ps

	go :: AnnotatedPlant -> GrowingPlant
 	go p = case filter (isValid.snd) $ map (second (applyAction p)) $ mapMaybe ($ p) rules of
		[]      -> noAction p
		choices -> chooseWeighted rgen choices

	-- Some general checks to rule out unwanted rules
	isValid :: GrowingPlant -> Bool
	isValid (Plant newSize oldSize ang ut ps) = anglesOk
	  where angles = sort $ map pAngle ps
		-- Are all angles not too close to each other?
                anglesOk = all (> minAngle) (zipWith (flip (-)) angles (tail angles))

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
