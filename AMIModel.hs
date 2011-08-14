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
        impulse    <- peekArray (fromIntegral row_size) impulse_matrix
        amiParams  <- peekCString ami_parameters_in
        putStrLn "amiInit got:"
        putStrLn $ "Impulse: " ++ (show impulse)
        putStrLn $ "Sample Interval: " ++ (show sample_interval)
        putStrLn $ "Bit Time: " ++ (show bit_time)
        putStrLn $ "AMI Parameters: "
        case parse amiToken "ami_parameters_in" amiParams of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> print r
        return 1

