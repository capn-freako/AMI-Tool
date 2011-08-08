{-# LANGUAGE ExistentialQuantification #-}

module AMITypes where

type AmiTree  = (String, [AmiToken])

type AmiToken = (String, AmiExp)

data AmiExp   = Attribute AmiAttribute
              | TokenList [AmiToken]
    deriving (Show)

data AmiAttribute   = Usage         AmiUseType
                    | Type          AmiParamType
                    | Default       AmiDefVal
                    | Format        AmiFormatType
                    | Description   String
                    | UsrAtt        String String
    deriving (Show)

data AmiUseType     = Info
                    | In
                    | Out
                    | Inout
                    | UsrUsage String
    deriving (Show)

data AmiParamType   = PBool
                    | PInt
                    | PTap
                    | PUI
                    | PUsr
    deriving (Show)

data AmiDefVal      = AmiDefVal Double
    deriving (Show)
{-data AmiDefVal      = forall a. (Num a) => AmiDefVal a
instance Show AmiDefVal where
    showsPrec = undefined
    show x
        | isDouble x = show x::Double
        | isInteger x = show x::Integer
        | isInt x     = show x::Int
        | otherwise   = undefined
    showList  = undefined
-}

data AmiFormatType  = Range Double Double Double
                    | UsrFrmt String
    deriving (Show)

