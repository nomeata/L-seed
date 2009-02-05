module Lseed.LSystem where

import Lseed.Data
import Data.Maybe
import Data.Monoid
import System.Random

applyLSystem :: RandomGen g => g -> LSystem -> Plant -> Plant
applyLSystem rgen rules plant = if null choices
		           then plant
                           else chooseWeighted rgen choices
  where choices = go plant id
        applyLocal p prev = mapMaybe (\(w,r) -> fmap (\p' -> (w,prev p')) (r p)) rules

 	go p prev = applyLocal p prev `mappend` case p of
				Bud -> mempty
				Stipe p' -> go p' (prev . Stipe)
				Fork p1 p2 -> go p1 (prev . (\x -> Fork x p2)) `mappend`
                                              go p2 (prev . (\x -> Fork p1 x))

chooseWeighted rgen list = replicated !! (c-1)
  where replicated = concatMap (\(w,e) -> replicate w e) list
        (c,_) = randomR (1, length replicated) rgen
