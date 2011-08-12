{-# LANGUAGE ForeignFunctionInterface #-}

module AMIModel where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

foreign export ccall amiInit :: Ptr Double -> CInt -> CInt -> Double -> Double -> (Ptr Char) -> Ptr (Ptr Char) -> Ptr (Ptr ()) -> Ptr (Ptr Char) -> IO Int

amiInit :: Ptr Double -> CInt -> CInt -> Double -> Double -> (Ptr Char) -> Ptr (Ptr Char) -> Ptr (Ptr ()) -> Ptr (Ptr Char) -> IO Int
amiInit impulse_matrix row_size aggressors sample_interval bit_time ami_parameters_in ami_parameters_out ami_memory_handle msg = do
    putStrLn "Hello, World."
    return 1

