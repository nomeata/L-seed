module Lseed.LSystem where

import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random

applyLSystem :: RandomGen g => g -> LSystem -> Plant () -> GrowingPlant
applyLSystem rgen rules plant = if null choices
		           then unmodified plant
                           else chooseWeighted rgen choices
  where choices = go plant id
        applyLocal p prev = mapMaybe (\(w,r) -> fmap (\p' -> (w,prev p')) (r p)) rules

 	go p prev = applyLocal p prev `mappend`
		    case p of
			Bud () ->
				mempty
			Stipe () len p' ->
				go p' (prev . (Stipe Nothing len))
			Fork () angle p1 p2 ->
				go p1 (prev . (\x -> Fork Nothing angle x (unmodified p2)))
                                `mappend`
				go p2 (prev . (\x -> Fork Nothing angle (unmodified p1) x))
	unmodified = fmap (const Nothing)

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
