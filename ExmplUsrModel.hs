module ExmplUsrModel where

import DSP.Filter.FIR.FIR
import DSP.Filter.FIR.Taps
import AMIParse

usrAmiInit :: AmiToken -> [Double] -> [Double]
usrAmiInit amiTree impulse
    -- Change the line, below, as follows:
    --  - Change "testAMI" to the root name of your AMI parameter tree.
    --  - Change "Mode" to the name you've given to the filter mode selection parameter.
    = case getAmiExp amiTree ["testAMI", "Model_Specific", "Mode", "Default"] of
        -- Change the `Vals' lines, below, to reflect your possible values of filter
        -- mode, and the corresponding action to be taken in that mode, adding/deleting
        -- lines as necessary.
        Just (Vals ["0"]) -> impulse                 -- Bypassed.
        Just (Vals ["1"]) -> fir (lpf 0.5 2) impulse -- 2nd order FIR LPF w/ cutoff at 1/2 Nyquist
        -- That's it; no more changes are required.
        Nothing  -> []
        _        -> []

