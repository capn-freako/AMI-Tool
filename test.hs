module Main where

import ExmplUsrModel
import Foreign.C.String
import Foreign.StablePtr
import Control.Monad
import System.Random
import System.Environment (getArgs)

sample_interval = 25e-12
maxRand = 255
vect_len = 128

the_params = "(testAMI (Mode 1) (Dcgain 0) (Bandwidth 0) (Process 0))"
unit_pulse = [1.0] ++ [0.0 | i <- [2..vect_len]]

rand :: IO Double
rand = getStdRandom random

doInit :: CString -> IO ([Double], StablePtr AmiModel)
doInit param_str = do
    (output, self) <- usrAmiInit param_str (realToFrac sample_interval) (map realToFrac unit_pulse)
    return (output, self)

main = do
    -- Fetch the arguments.
    args <- getArgs
    let num_get_calls | null args = 10
                      | otherwise = read (head args)
    -- Initialize the model.
    (output, self) <- withCString the_params doInit
    -- Generate all the random values we'll need.
    wave_ins <- forM [1..num_get_calls] $ \y -> do
        forM [1..vect_len] $ \x -> rand
    -- Process all input vectors through GetWave().
    wave_outs <- forM wave_ins $ \wave_in -> usrAmiGetWave self wave_in
    putStrLn $ show $ length $ concat wave_outs
    me <- deRefStablePtr self
    usrAmiClose me

