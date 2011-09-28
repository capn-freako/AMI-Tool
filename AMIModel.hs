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

{- This defines the structure of `global' static data we need to preserve,
in between calls to `amiInit' and `amiClose'.

We'll allocate space for this structure, during initialization (i.e. - while
we're in `amiInit'), and send a pointer to that space back to the calling
program, via the `ami_memory_handle' argument. The calling program will store
that pointer and send it back to us, when it calls `amiClose', so that we may
free the space, only after the World is completely finished with us.

In this way, we avoid a global pointer within our own name space, and remain
reusable.
-}
data AmiModel = AmiModel {
      paramsOut :: StablePtr CString
    , msgPtr    :: StablePtr CString
    , amiTree   :: StablePtr AmiToken
    }

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
        impulse      <- if (impulse_matrix == nullPtr) then
                            return []
                        else
                            peekArray (fromIntegral row_size) impulse_matrix
        -- AMI parameter parsing
        amiParams    <- peekCString ami_parameters_in
        (amiTree, msg) <- case parse amiToken "ami_parameters_in" amiParams of
                            Left e  -> do msg' <- newCString $ "Error parsing input: " ++ show e
                                          return (("ParseError", Tokens []), msg')
                            Right r -> do msg' <- newCString "AMI parameters parsed successfully." 
                                          return (r, msg')
        -- model specific channel convolution
        pokeArray impulse_matrix (map realToFrac $
            usrAmiInit amiTree (realToFrac sample_interval) (map realToFrac impulse))

        -- static memory allocation
        tmpMsgPtr    <- newStablePtr msg -- Protecting `msg' from the garbage collector.
        poke msgHndl msg                 -- Note that we poke `msg', not `tmpMsgPtr', into `msgHndl'.
        prms         <- newCString $ "(" ++ fst amiTree ++ (filter (/= '\n') $ show (snd amiTree)) ++ ")"
        tmpParamsOut <- newStablePtr prms
        poke ami_parameters_out prms
        tmpAmiTree   <- newStablePtr amiTree
        self         <- newStablePtr AmiModel {            -- Storing pointers to these protected entities,
                                  paramsOut = tmpParamsOut -- so that we'll be able to free the space,
                                , msgPtr    = tmpMsgPtr    -- when we're closed.
                                , amiTree   = tmpAmiTree
                               }
        poke ami_memory_handle self

        return 1

-- Our Haskell implementation of `AMI_Getwave'.
amiGetWave :: Ptr CDouble -> CInt -> Ptr CDouble -> Ptr CString -> StablePtr AmiModel -> IO Int
amiGetWave wave_in wave_size clock_times ami_parameters_out ami_memory_ptr
    = do
        -- C to Haskell variable conversion
        theWave      <- if (wave_in == nullPtr) then
                            return []
                        else
                            peekArray (fromIntegral wave_size) wave_in

        -- model specific filtering
        self       <- deRefStablePtr ami_memory_ptr -- The accessor, `amiTree' below, takes the structure itself,
        amiParams  <- deRefStablePtr $ amiTree self -- not a pointer to it.
        y          <- return $ usrAmiGetWave amiParams (map realToFrac theWave)
        pokeArray wave_in (map realToFrac y)

        return 1

-- Our Haskell implementation of `AMI_Close'.
-- Note the single argument, which is the pointer to the static structure we
-- allocated, during initialization.
amiClose :: StablePtr AmiModel -> IO Int
amiClose selfPtr = do
    self <- deRefStablePtr selfPtr -- The accessors, `paramsOut' and `msgPtr', take the structure itself,
    freeStablePtr (paramsOut self) -- not a pointer to it, as arguments.
    freeStablePtr (msgPtr self)
    freeStablePtr selfPtr
    return 1

