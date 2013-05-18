module Main where

import ExmplUsrModel
import Foreign.C.String
import Foreign.StablePtr

the_params = "(testAMI (Mode 1) (Dcgain 0) (Bandwidth 0) (Process 0))"
unit_pulse = [1.0] ++ [0.0 | i <- [1..127]]
sample_interval = 25e-12

doInit :: CString -> IO ([Double], StablePtr AmiModel)
doInit param_str = do
    (output, self) <- usrAmiInit param_str (realToFrac sample_interval) (map realToFrac unit_pulse)
    return (output, self)

main = do
    (output, self) <- withCString the_params doInit
    putStrLn $ show output
    me <- deRefStablePtr self
    usrAmiClose me

