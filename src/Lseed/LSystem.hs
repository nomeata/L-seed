module Lseed.LSystem where

import Lseed.Constants
import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random

applyLSystem :: RandomGen g => g -> LSystem -> Plant () -> GrowingPlant
applyLSystem rgen rules plant = go plant
  where applyAction (EnlargeStipe newSize) (Stipe () oldSize ps)
		= Stipe (Just newSize) oldSize $
                  mapSprouts go ps
	applyAction (ForkStipe pos []) (Stipe () oldSize ps) -- No branches
		= Stipe Nothing oldSize $
		  mapSprouts go ps
	applyAction (ForkStipe pos branchSpecs) (Stipe () oldSize ps)
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

 	go p = case mapMaybe (\r -> r p) rules of
		[]      -> noAction p
		choices -> applyAction (chooseWeighted rgen choices) p

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
