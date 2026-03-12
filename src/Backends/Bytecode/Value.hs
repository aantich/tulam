{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Value representation for the bytecode VM.
--
-- Phase 1: Tagged union (simple, correct).
-- Phase 2+: NaN-boxed Word64 for zero-allocation numeric code.
module Backends.Bytecode.Value
    ( Val(..)
    , HeapObj(..)
    , valToString
    , valToBool
    , valToInt
    , valToFloat
    , isValTrue
    , unitVal
    , emptyVal
    , boolVal
    , intVal
    , floatVal
    , charVal
    , stringVal
    , conVal
    , arrayVal
    , closureVal
    , papVal
    , refVal
    , mutArrayVal
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Runtime value. Phase 1 uses a tagged union.
-- All fields are strict for GHC performance.
data Val
    = VInt    {-# UNPACK #-} !Int
    | VFloat  {-# UNPACK #-} !Double
    | VBool   !Bool
    | VChar   {-# UNPACK #-} !Char
    | VString !Text
    | VUnit
    | VEmpty
    | VObj    !HeapObj
    deriving (Eq)

instance Show Val where
    show (VInt n)    = show n
    show (VFloat d)  = show d
    show (VBool b)   = if b then "True" else "False"
    show (VChar c)   = show c
    show (VString s) = T.unpack s
    show VUnit       = "()"
    show VEmpty      = "<empty>"
    show (VObj obj)  = show obj

-- | Heap-allocated objects.
data HeapObj
    = HCon   {-# UNPACK #-} !Int   -- constructor tag
             {-# UNPACK #-} !Int   -- arity (redundant but fast)
             !(V.Vector Val)        -- fields
    | HClosure {-# UNPACK #-} !Int -- function index in module
               !(V.Vector Val)      -- captured upvalues (flat)
    | HPAP  {-# UNPACK #-} !Int    -- function index
            {-# UNPACK #-} !Int    -- expected total arity
            !(V.Vector Val)         -- applied args so far
    | HArray !(V.Vector Val)        -- immutable array
    | HRef   !(IORef Val)           -- mutable reference
    | HMutArray !(IOVector Val)     -- mutable array

instance Show HeapObj where
    show (HCon tag arity fields)   = "Con(" ++ show tag ++ ", " ++ show (V.toList fields) ++ ")"
    show (HClosure fi upvals)      = "Closure(" ++ show fi ++ ", " ++ show (V.length upvals) ++ " upvals)"
    show (HPAP fi expected args)   = "PAP(" ++ show fi ++ ", " ++ show (V.length args) ++ "/" ++ show expected ++ ")"
    show (HArray xs)               = "[" ++ show (V.toList xs) ++ "]"
    show (HRef _)                  = "<ref>"
    show (HMutArray _)             = "<mutarray>"

-- IORef and IOVector don't have Eq, so we use referential identity
instance Eq HeapObj where
    HCon t1 a1 f1  == HCon t2 a2 f2  = t1 == t2 && a1 == a2 && f1 == f2
    HClosure f1 u1  == HClosure f2 u2  = f1 == f2 && u1 == u2
    HPAP f1 e1 a1  == HPAP f2 e2 a2  = f1 == f2 && e1 == e2 && a1 == a2
    HArray xs       == HArray ys       = xs == ys
    _               == _               = False

-- Smart constructors
unitVal :: Val
unitVal = VUnit

emptyVal :: Val
emptyVal = VEmpty

boolVal :: Bool -> Val
boolVal = VBool

intVal :: Int -> Val
intVal = VInt

floatVal :: Double -> Val
floatVal = VFloat

charVal :: Char -> Val
charVal = VChar

stringVal :: Text -> Val
stringVal = VString

conVal :: Int -> [Val] -> Val
conVal tag fields = VObj (HCon tag (length fields) (V.fromList fields))

arrayVal :: [Val] -> Val
arrayVal xs = VObj (HArray (V.fromList xs))

closureVal :: Int -> [Val] -> Val
closureVal funcIdx upvals = VObj (HClosure funcIdx (V.fromList upvals))

papVal :: Int -> Int -> [Val] -> Val
papVal funcIdx expectedArity args = VObj (HPAP funcIdx expectedArity (V.fromList args))

refVal :: IORef Val -> Val
refVal = VObj . HRef

mutArrayVal :: IOVector Val -> Val
mutArrayVal = VObj . HMutArray

-- Conversion helpers
valToString :: Val -> Text
valToString (VString s) = s
valToString (VInt n)    = T.pack (show n)
valToString (VFloat d)  = T.pack (show d)
valToString (VBool b)   = if b then "True" else "False"
valToString (VChar c)   = T.singleton c
valToString VUnit       = "()"
valToString VEmpty      = ""
valToString (VObj obj)  = T.pack (show obj)

valToBool :: Val -> Bool
valToBool (VBool b) = b
valToBool VUnit     = False
valToBool VEmpty    = False
valToBool (VInt 0)  = False
valToBool _         = True

valToInt :: Val -> Maybe Int
valToInt (VInt n) = Just n
valToInt _        = Nothing

valToFloat :: Val -> Maybe Double
valToFloat (VFloat d) = Just d
valToFloat (VInt n)   = Just (fromIntegral n)
valToFloat _          = Nothing

isValTrue :: Val -> Bool
isValTrue = valToBool
