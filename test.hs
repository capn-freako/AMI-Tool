module Main where

import Foreign.C.String
import Foreign.StablePtr
import Control.Monad

import ExmplUsrModel

myParams       = newCString $ foldl (++) "" ["(testAMI ",
                                                "(Process     0) ",
                                                "(Bandwidth   0) ",
                                                "(Dcgain      0) ",
                                                "(Mode        1) ",
                                              ")"]
smplInt         = 25e-12                     -- 40 GHz sampling
channelResponse = 0.0 : (0.1 : (repeat 0.0)) -- infinitely long impulse

main = do
    theParams              <- myParams
    (impulse, theModelPtr) <- usrAmiInit theParams smplInt $ take 128 channelResponse
    theModel               <- deRefStablePtr theModelPtr
    theTokens              <- deRefStablePtr $ amiParams theModel
    wave                   <- forM [1..100000] $ \i -> do
                                usrAmiGetWave theModelPtr theTokens $ take 1024 channelResponse
    return wave

