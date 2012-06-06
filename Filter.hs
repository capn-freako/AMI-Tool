{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

module Filter where

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.Automaton

-- Time domain convolution filter (FIR or IIR),
-- expressed in direct form 2
--convT :: (Num b) => Filter b b
convT (!x, !s) =
    let !wk = (x - sum [a * t | (a, t) <- zip (tail $ as s) (taps s)]) / (head $ as s)
        !newTaps = wk : ((reverse . tail . reverse) $ taps s)
        !s' = s {taps = newTaps}
        !y  = sum [b * w | (b, w) <- zip (bs s) (wk : (taps s))]
    in (y, s')

-- tap weights, `as' and `bs', are being made part of the filter state, in
-- order to accomodate adaptive filters (i.e. - DFEs).
data FilterState a = FilterState {
    as   :: ![a] -- transfer function denominator coefficients
  , bs   :: ![a] -- transfer function numerator coefficients
  , taps :: ![a] -- current delay tap stored values
  } deriving (Show)

-- Future proofing the implementation, using the `newtype' trick.
type Filter a = FilterState a -> Automaton (->) a a

-- Turn a filter into an Automaton, in order to use the built in plubming
-- of Arrows to run the filter on an input.
filterAuto :: ((a, FilterState a) -> (a, FilterState a)) -> Filter a
filterAuto !f = \s0 -> proc x -> do
    rec (y, s') <- arr f    -< (x, s)
        s       <- delay s0 -< s'
    returnA -< y

runAuto a             []     = []
runAuto !(Automaton f) !(x:xs) = let
    (y, a) = f x
    in y : runAuto a xs

runFilter :: Filter a -> FilterState a -> [a] -> [a]
runFilter !f !s !xs = runAuto (f s) xs

t2 = let s = FilterState [1, 0, 0] [0.7, 0.2, 0.1] [0, 0, 0]
     in runAuto (filterAuto convT s) [1, 0, 0, 0, 0]

