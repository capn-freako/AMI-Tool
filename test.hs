module Main where

import Foreign.C.String
import Foreign.StablePtr
import Control.Monad
import Foreign

import ExmplUsrModel

foreach = flip mapM_

smplInt         = 25e-12                     -- 40 GHz sampling
channelResponse = 0.0 : (0.1 : (repeat 0.0)) -- infinitely long impulse

main = do
    putStrLn "MAIN"
    theParams               <- newCString $ foldl (++) "" ["(testAMI ",
                                                            "(Process     0) ",
                                                            "(Bandwidth   0) ",
                                                            "(Dcgain      0) ",
                                                            "(Mode        1) ",
                                                          ")"]
    (impulse, theModelPtr) <- usrAmiInit theParams smplInt $ take 128 channelResponse
    theModel               <- deRefStablePtr theModelPtr
    theTokens              <- deRefStablePtr $ amiParams theModel
    foreach [1..100] $ \i -> do
        putStrLn "HERE"
        wave <- usrAmiGetWave theModelPtr theTokens $ take 1024 channelResponse
        putStrLn $ show wave
    usrAmiClose theModel
    free theParams
    freeStablePtr theModelPtr
    return 0

