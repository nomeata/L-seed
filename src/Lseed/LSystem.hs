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
	applyAction (Plant _ oldSize ang ps) (EnlargeStipe newSize) 
		= Plant (Just newSize) oldSize ang $
                  map go ps
	applyAction (Plant _ oldSize ang ps) (ForkStipe pos [])-- No branches
		= Plant Nothing oldSize ang $
		  map go ps
	applyAction (Plant _ oldSize ang ps) (ForkStipe pos branchSpecs)
		| 1-pos < eps -- Fork at the end
		= Plant Nothing oldSize ang $
			ps' ++
			newForks
		| otherwise -- Fork not at the end
		= Plant Nothing (oldSize * pos) ang $
			[ Plant Nothing (oldSize * (1-pos)) 0 ps' ] ++
			newForks
	  where newForks = map (\(angle, newSize) -> Plant (Just newSize) 0 angle []) branchSpecs
		ps' = map go ps

	noAction (Plant _ oldSize ang ps)
		= Plant Nothing oldSize ang $ map go ps

	go :: AnnotatedPlant -> GrowingPlant
 	go p = case filter (isValid.snd) $ map (second (applyAction p)) $ mapMaybe ($ p) rules of
		[]      -> noAction p
		choices -> chooseWeighted rgen choices

	-- Some general checks to rule out unwanted rules
	isValid :: GrowingPlant -> Bool
	isValid (Plant newSize oldSize ang ps) = anglesOk
	  where angles = sort $ map pAngle ps
		-- Are all angles not too close to each other?
                anglesOk = all (> minAngle) (zipWith (flip (-)) angles (tail angles))

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
