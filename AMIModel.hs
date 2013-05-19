{-# LANGUAGE ForeignFunctionInterface #-}

module AMIModel where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Text.ParserCombinators.Parsec
import AMIParse
import ExmplUsrModel

-- These make certain of our Haskell functions callable from C.
-- Note) They are duplicates of the function signatures, below, with the
--       exception of the `foreign export ccall' prefix.
-- Note the `C' prefix on the majority of the argument types; this is your
-- clue that we're expecting to be called from C.
foreign export ccall amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int
foreign export ccall amiClose :: StablePtr AmiModel -> IO Int
foreign export ccall amiGetWave :: Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CString -> StablePtr AmiModel -> IO Int

-- Note) Typically, the 3 `amiXxx' functions, below, are called by the
--       equivalent functions from `ami_model.c', which have the standard
--       AMI function names (i.e. - `AMI_Init', `AMI_GetWave', and `AMI_Close')
--       and which form the external, public interface of the shared object
--       library we're building, here. This extra layer of indirection is
--       necessary, because the Haskell run-time system MUST be initialized,
--       before ANY Haskell function is called. That initialization is done
--       by `ami_model.c', before it calls any functions contained herein.

-- Our Haskell implementation of `AMI_Init'.
amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int
amiInit impulse_matrix row_size aggressors sample_interval bit_time
        ami_parameters_in ami_parameters_out ami_memory_handle msgHndl
    = do
        -- C to Haskell variable conversion
        impulse      <- if impulse_matrix == nullPtr then
                            return []
                        else
                            peekArray (fromIntegral row_size) impulse_matrix

        -- Call model specific initialization function.
        (newImpulse, self) <- usrAmiInit ami_parameters_in (realToFrac sample_interval) (map realToFrac impulse)
        poke ami_memory_handle self
        amiModel <- deRefStablePtr self
        poke msgHndl $ msgPtr amiModel
        pokeArray impulse_matrix $ map realToFrac newImpulse
        poke ami_parameters_out $ paramsOut amiModel

        return 1

-- Our Haskell implementation of `AMI_Getwave'.
amiGetWave :: Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CString -> StablePtr AmiModel -> IO Int
amiGetWave wave_in wave_size clock_times ami_parameters_out ami_memory_ptr
    = do
        -- C to Haskell variable conversion
        theWave      <- if wave_in == nullPtr then
                            return []
                        else
                            peekArray (fromIntegral wave_size) wave_in

        -- model specific filtering
        self       <- deRefStablePtr ami_memory_ptr -- The accessor, `amiParams' below, takes the structure itself,
        amiParams  <- deRefStablePtr $ amiParams self -- not a pointer to it.
        y          <- usrAmiGetWave ami_memory_ptr (map realToFrac theWave)
        pokeArray wave_in (map realToFrac y)

        return 1

-- Our Haskell implementation of `AMI_Close'.
-- Note the single argument, which is the pointer to the static structure we
-- allocated, during initialization.
amiClose :: StablePtr AmiModel -> IO Int
amiClose selfPtr = do
    self <- deRefStablePtr selfPtr -- The accessors for type `AmiModel' take the structure itself,
    usrAmiClose self               -- not a pointer to it, as arguments.
    freeStablePtr selfPtr
    return 1

