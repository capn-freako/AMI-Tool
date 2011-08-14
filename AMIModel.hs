{-# LANGUAGE ForeignFunctionInterface #-}

module AMIModel where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import MarshalArray
import MarshalError
import Text.ParserCombinators.Parsec
import AMIParse

foreign export ccall amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int

data AmiModel = AmiModel {
      paramsOut :: StablePtr CString
    , msgPtr    :: StablePtr CString
    }

amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int
amiInit impulse_matrix row_size aggressors sample_interval bit_time
        ami_parameters_in ami_parameters_out ami_memory_handle msgHndl
    | impulse_matrix == nullPtr = return 0
    | otherwise = do
        impulse    <- peekArray (fromIntegral row_size) impulse_matrix
        amiParams  <- peekCString ami_parameters_in
        putStrLn "amiInit got:"
        putStrLn $ "Impulse: " ++ (show impulse)
        putStrLn $ "Sample Interval: " ++ (show sample_interval)
        putStrLn $ "Bit Time: " ++ (show bit_time)
        putStrLn $ "AMI Parameters: "
        msg <- case parse amiToken "ami_parameters_in" amiParams of
            Left e -> do putStrLn "Error parsing input:"
                         print e
                         newCString "Error parsing AMI parameters!" 
            Right r -> do print r
                          newCString "AMI parameters parsed successfully." 
        tmpMsgPtr <- newStablePtr msg
        poke msgHndl msg
        prms <- newCString "No output params, yet."
        tmpParamsOut <- newStablePtr prms
        poke ami_parameters_out prms
        self <- newStablePtr $ AmiModel {
                                  paramsOut = tmpParamsOut
                                , msgPtr    = tmpMsgPtr
                               }
        poke ami_memory_handle self
        return 1

amiClose :: StablePtr AmiModel -> IO Int
amiClose selfPtr = do
    self <- deRefStablePtr selfPtr
    freeStablePtr (paramsOut self)
    freeStablePtr (msgPtr self)
    freeStablePtr selfPtr
    return 1

