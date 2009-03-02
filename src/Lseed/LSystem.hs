module Lseed.LSystem where

import Lseed.Constants
import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random

applyLSystem :: RandomGen g => g -> LSystem -> Plant () -> GrowingPlant
applyLSystem rgen rules plant = go plant
  where applyAction (EnlargeStipe newSize) (Stipe () oldSize p')
		= Stipe (Just newSize) oldSize $
                  go p'
	applyAction (ForkStipe pos []) (Stipe () oldSize p') -- No branches
		= Stipe Nothing oldSize $ go p'
	applyAction (ForkStipe pos branchSpecs) (Stipe () oldSize p')
		= preFork . forks branchSpecs . postFork $ go p'
	  where (preFork, postFork) | pos < eps -- Fork at the beginning
			            = (id, Stipe Nothing oldSize)
                                    | 1-pos < eps -- Fork at the end
				    = (Stipe Nothing oldSize, id)
                                    | otherwise -- Fork in the middle
                                    = (Stipe Nothing (oldSize * pos),
		                       Stipe Nothing (oldSize * (1-pos)))
		forks = flip $ foldr (\(angle, newSize) -> Fork Nothing angle (Stipe (Just newSize) 0 (Bud Nothing)))
	applyAction _ _ = error "Unknown Action or applied to wrong part of a plant"

 	go p = case p of
			Bud () ->
				Bud Nothing
			Stipe () _ _ ->
				let choices = mapMaybe (\r -> r p) rules 
				in  applyAction (chooseWeighted rgen choices) p
			Fork () angle p1 p2 ->
				Fork Nothing angle (go p1) (go p2)

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
