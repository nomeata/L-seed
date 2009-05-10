module Lseed.LSystem where

import Lseed.Constants
import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random
import Control.Arrow (second)
import Data.List

applyLSystem :: RandomGen g => g -> LSystem -> Plant () -> GrowingPlant
applyLSystem rgen rules plant = go plant
  where applyAction :: Plant () -> LRuleAction -> GrowingPlant
	applyAction (Stipe () oldSize ps) (EnlargeStipe newSize) 
		= Stipe (Just newSize) oldSize $
                  mapSprouts go ps
	applyAction (Stipe () oldSize ps) (ForkStipe pos [])-- No branches
		= Stipe Nothing oldSize $
		  mapSprouts go ps
	applyAction (Stipe () oldSize ps) (ForkStipe pos branchSpecs)
		| 1-pos < eps -- Fork at the end
		= Stipe Nothing oldSize $
			ps' ++
			newForks
		| otherwise -- Fork not at the end
		= Stipe Nothing (oldSize * pos) $
			[ (0, Stipe Nothing (oldSize * (1-pos)) ps') ] ++
			newForks
	  where newForks = map (\(angle, newSize) -> (angle, Stipe (Just newSize) 0 [])) branchSpecs
		ps' = mapSprouts go ps

	noAction (Stipe () oldSize ps)
		= Stipe Nothing oldSize $ mapSprouts go ps

	go :: Plant () -> GrowingPlant
 	go p = case filter (isValid.snd) $ map (second (applyAction p)) $ mapMaybe ($ p) rules of
		[]      -> noAction p
		choices -> chooseWeighted rgen choices

	-- Some general checks to rule out unwanted rules
	isValid :: GrowingPlant -> Bool
	isValid (Stipe newSize oldSize ps) = anglesOk
	  where angles = sort $ map fst ps
		-- Are all angles not too close to each other?
                anglesOk = all (> minAngle) (zipWith (flip (-)) angles (tail angles))

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
