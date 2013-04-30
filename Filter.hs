module Filter where

-- FILTER STATE/TYPE DEFINITIONS
-- tap weights, `as' and `bs', are being made part of the filter state, in
-- order to accomodate adaptive filters (i.e. - DFEs).
data FilterState a = FilterState {
    as   :: ![a] -- transfer function denominator coefficients
  , bs   :: ![a] -- transfer function numerator coefficients
  , taps :: ![a] -- current delay tap stored values
  } deriving (Show)

type Kernel a = (a, FilterState a) -> (a, FilterState a)

-- FILTER KERNELS
-- Time domain convolution filter (FIR or IIR),
-- expressed in direct form 2
convT :: (Fractional a) => Kernel a
convT (x, s) =
    let wk = (x - sum [a * t | (a, t) <- zip (tail $ as s) (taps s)]) / head (as s)
        newTaps = wk : init (taps s)
        s' = s {taps = newTaps}
        y  = sum [b * w | (b, w) <- zip (bs s) (wk : taps s)]
    in (y, s')

-- FILTER APPLICATION
runFilter :: Kernel a -> FilterState a -> [a] -> [a]
runFilter f s0 [] = []
runFilter f s0 (x:xs) = let
    (y, s') = f (x, s0)
    in (y : runFilter f s' xs)

