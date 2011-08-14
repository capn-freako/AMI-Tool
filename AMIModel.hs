{-# LANGUAGE ForeignFunctionInterface #-}

module AMIModel where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import MarshalArray
import MarshalError
import Text.ParserCombinators.Parsec
import AMIParse

foreign export ccall amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (Ptr ()) -> Ptr CString -> IO Int

amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (Ptr ()) -> Ptr CString -> IO Int
amiInit impulse_matrix row_size aggressors sample_interval bit_time
        ami_parameters_in ami_parameters_out ami_memory_handle msg
    | impulse_matrix == nullPtr = return 0
    | otherwise = do
        putStrLn "Inside amiInit."
        impulse    <- peekArray (fromIntegral row_size) impulse_matrix
        putStrLn "Inside amiInit."
        amiParams  <- peekCString ami_parameters_in
        putStrLn "amiInit got:"
        putStrLn $ "Impulse: " ++ (show impulse)
        putStrLn $ "Sample Interval: " ++ (show sample_interval)
        putStrLn $ "Bit Time: " ++ (show bit_time)
    --    putStrLn $ "AMI Parameters: " ++ (show (parse amiToken amiParams))
        putStrLn $ "AMI Parameters: " ++ amiParams
        return 1

