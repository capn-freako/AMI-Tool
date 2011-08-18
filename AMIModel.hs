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

-- These make certain of our Haskell functions callable from C.
-- Note) They are duplicates of the function signatures, below, with the
--       exception of the `foreign export ccall' prefix.
-- Note the `C' prefix on the majority of the argument types; this is your
-- clue that we're expecting to be called from C.
foreign export ccall amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int
foreign export ccall amiClose :: StablePtr AmiModel -> IO Int

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
    }

-- Our Haskell implementation of `AMI_Init'.
amiInit :: Ptr CDouble -> CInt -> CInt -> CDouble -> CDouble ->
           CString -> Ptr CString -> Ptr (StablePtr AmiModel) -> Ptr CString -> IO Int
amiInit impulse_matrix row_size aggressors sample_interval bit_time
        ami_parameters_in ami_parameters_out ami_memory_handle msgHndl
    | impulse_matrix == nullPtr = return 0
    | otherwise = do
        putStrLn "I'm here."
        impulse      <- peekArray (fromIntegral row_size) impulse_matrix
        amiParams    <- peekCString ami_parameters_in
        (amiTree, msg) <- case parse amiToken "ami_parameters_in" amiParams of
                            Left e  -> do msg' <- newCString $ "Error parsing input: " ++ (show e)
                                          return (("ParseError", Tokens []), msg')
                            Right r -> do msg' <- newCString "AMI parameters parsed successfully." 
                                          return (r, msg')
        tmpMsgPtr    <- newStablePtr msg -- Protecting `msg' from the garbage collector.
        poke msgHndl msg                 -- Note that we poke `msg', not `tmpMsgPtr', into `msgHndl'.
        prms         <- newCString $ show amiTree
        tmpParamsOut <- newStablePtr prms
        poke ami_parameters_out prms
        self         <- newStablePtr $ AmiModel {          -- Storing pointers to these protected entities,
                                  paramsOut = tmpParamsOut -- so that we'll be able to free the space,
                                , msgPtr    = tmpMsgPtr    -- when we're closed.
                               }
        poke ami_memory_handle self
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

