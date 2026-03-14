{-# LANGUAGE OverloadedStrings, BangPatterns, GeneralizedNewtypeDeriving #-}
-- | NaN-boxed value representation for the bytecode VM.
--
-- All values are stored as Word64 using NaN-boxing:
-- - Float64: raw IEEE 754 bits (when bits[63:51] != all 1s)
-- - Tagged: bits[63:51] = all 1s (negative quiet NaN space)
--     bits[50:48] = 3-bit type tag
--     bits[47:0]  = 48-bit payload
--
-- Tags: 0=Heap, 1=Int, 2=Bool, 3=Char, 4=Unit, 5=Empty, 6=String
module Backends.Bytecode.Value
    ( Val(..)
    , HeapObj(..)
    -- * NaN-boxing primitives (Word64 level, zero allocation)
    , mkIntW, mkFloatW, mkBoolW, mkCharW, mkUnitW, mkEmptyW
    , mkHeapW, mkStrW
    , getIntW, getFloatW, getBoolW, getCharW, getHeapIdxW, getStrIdxW
    , isIntW, isFloatW, isBoolW, isCharW, isUnitW, isEmptyW
    , isHeapW, isStrW, isTaggedW
    , addIntW, subIntW, mulIntW, divIntW, remIntW, negIntW, absIntW
    , addFloatW, subFloatW, mulFloatW, divFloatW, negFloatW
    , absFloatW, sqrtFloatW
    , intBinCmpW, floatBinCmpW
    , itofW, ftoiW
    , bandW, borW, bxorW, bshlW, bshrW
    , isValTrueW
    , eqW
    -- * Val interface (allocates, for complex dispatch)
    , valToW, wToVal
    , valToString
    , valToBool
    , valToInt
    , valToFloat
    , isValTrue
    , unitVal, emptyVal, boolVal, intVal, floatVal, charVal, stringVal
    , conVal, arrayVal, closureVal, papVal, refVal, mutArrayVal
    -- * Heap/String table operations
    , HeapTable(..)
    , newHeapTable
    , heapAlloc
    , heapRead
    , heapWrite
    , StringTable(..)
    , newStringTable
    , strAlloc
    , strRead
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Bits
import Data.Char (chr, ord)
import Data.IORef
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- ============================================================================
-- HeapObj (unchanged from Phase 1)
-- ============================================================================

-- | Heap-allocated objects.
data HeapObj
    = HCon   {-# UNPACK #-} !Int   -- constructor tag
             {-# UNPACK #-} !Int   -- arity (redundant but fast)
             !(V.Vector Val)        -- fields (as NaN-boxed Word64)
    | HClosure {-# UNPACK #-} !Int -- function index in module
               !(V.Vector Val)      -- captured upvalues (flat)
    | HPAP  {-# UNPACK #-} !Int    -- function index
            {-# UNPACK #-} !Int    -- expected total arity
            !(V.Vector Val)         -- applied args so far
    | HArray !(V.Vector Val)        -- immutable array
    | HRef   !(IORef Val)           -- mutable reference
    | HMutArray !(MV.IOVector Val)  -- mutable array

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

-- ============================================================================
-- Val: NaN-boxed Word64
-- ============================================================================

-- | NaN-boxed runtime value. Stored as raw Word64 in registers.
newtype Val = Val { unVal :: Word64 }
    deriving (Eq, Ord)

instance Show Val where
    show v
        | isFloatW w  = show (getFloatW w)
        | isIntW w    = show (getIntW w)
        | isBoolW w   = if getBoolW w then "True" else "False"
        | isCharW w   = show (getCharW w)
        | isUnitW w   = "()"
        | isEmptyW w  = "<empty>"
        | isHeapW w   = "<heap:" ++ show (getHeapIdxW w) ++ ">"
        | isStrW w    = "<str:" ++ show (getStrIdxW w) ++ ">"
        | otherwise   = "<unknown:" ++ show w ++ ">"
      where w = unVal v

-- ============================================================================
-- NaN-boxing constants
-- ============================================================================

-- | Mask for the "is tagged" check: bits[63:51] all 1s
{-# INLINE tagMask #-}
tagMask :: Word64
tagMask = 0xFFF8000000000000

-- | Type tag base values (bits[63:48])
{-# INLINE tagHeap #-}
tagHeap :: Word64
tagHeap = 0xFFF8000000000000  -- tag 0

{-# INLINE tagInt #-}
tagInt :: Word64
tagInt = 0xFFF9000000000000   -- tag 1

{-# INLINE tagBool #-}
tagBool :: Word64
tagBool = 0xFFFA000000000000  -- tag 2

{-# INLINE tagChar #-}
tagChar :: Word64
tagChar = 0xFFFB000000000000  -- tag 3

{-# INLINE tagUnit #-}
tagUnit :: Word64
tagUnit = 0xFFFC000000000000  -- tag 4

{-# INLINE tagEmpty #-}
tagEmpty :: Word64
tagEmpty = 0xFFFD000000000000 -- tag 5

{-# INLINE tagStr #-}
tagStr :: Word64
tagStr = 0xFFFE000000000000   -- tag 6

-- | Mask for high 16 bits (tag identifier)
{-# INLINE tagIdMask #-}
tagIdMask :: Word64
tagIdMask = 0xFFFF000000000000

-- | Mask for 48-bit payload
{-# INLINE payloadMask #-}
payloadMask :: Word64
payloadMask = 0x0000FFFFFFFFFFFF

-- | Sign bit for 48-bit integers (bit 47)
{-# INLINE signBit48 #-}
signBit48 :: Word64
signBit48 = 0x0000800000000000

-- | Sign extension mask for 48-bit to 64-bit
{-# INLINE signExtMask #-}
signExtMask :: Word64
signExtMask = 0xFFFF000000000000

-- ============================================================================
-- Type checks (Word64 level)
-- ============================================================================

{-# INLINE isTaggedW #-}
isTaggedW :: Word64 -> Bool
isTaggedW w = w .&. tagMask == tagMask

{-# INLINE isFloatW #-}
isFloatW :: Word64 -> Bool
isFloatW w = not (isTaggedW w)

{-# INLINE isIntW #-}
isIntW :: Word64 -> Bool
isIntW w = w .&. tagIdMask == tagInt

{-# INLINE isBoolW #-}
isBoolW :: Word64 -> Bool
isBoolW w = w .&. tagIdMask == tagBool

{-# INLINE isCharW #-}
isCharW :: Word64 -> Bool
isCharW w = w .&. tagIdMask == tagChar

{-# INLINE isUnitW #-}
isUnitW :: Word64 -> Bool
isUnitW w = w .&. tagIdMask == tagUnit

{-# INLINE isEmptyW #-}
isEmptyW :: Word64 -> Bool
isEmptyW w = w .&. tagIdMask == tagEmpty

{-# INLINE isHeapW #-}
isHeapW :: Word64 -> Bool
isHeapW w = w .&. tagIdMask == tagHeap

{-# INLINE isStrW #-}
isStrW :: Word64 -> Bool
isStrW w = w .&. tagIdMask == tagStr

-- ============================================================================
-- Constructors (Word64 level, zero allocation)
-- ============================================================================

{-# INLINE mkIntW #-}
mkIntW :: Int -> Word64
mkIntW n = tagInt .|. (fromIntegral n .&. payloadMask)

{-# INLINE mkFloatW #-}
mkFloatW :: Double -> Word64
mkFloatW d
    | isNaN d   = 0x7FF8000000000000  -- canonical positive quiet NaN
    | otherwise = castDoubleToWord64 d

{-# INLINE mkBoolW #-}
mkBoolW :: Bool -> Word64
mkBoolW True  = tagBool .|. 1
mkBoolW False = tagBool

{-# INLINE mkCharW #-}
mkCharW :: Char -> Word64
mkCharW c = tagChar .|. fromIntegral (ord c)

{-# INLINE mkUnitW #-}
mkUnitW :: Word64
mkUnitW = tagUnit

{-# INLINE mkEmptyW #-}
mkEmptyW :: Word64
mkEmptyW = tagEmpty

{-# INLINE mkHeapW #-}
mkHeapW :: Int -> Word64
mkHeapW idx = tagHeap .|. (fromIntegral idx .&. payloadMask)

{-# INLINE mkStrW #-}
mkStrW :: Int -> Word64
mkStrW idx = tagStr .|. (fromIntegral idx .&. payloadMask)

-- ============================================================================
-- Extractors (Word64 level, zero allocation)
-- ============================================================================

{-# INLINE getIntW #-}
getIntW :: Word64 -> Int
getIntW w =
    let raw = w .&. payloadMask
    in if raw .&. signBit48 /= 0
       then fromIntegral (raw .|. signExtMask)  -- sign extend
       else fromIntegral raw

{-# INLINE getFloatW #-}
getFloatW :: Word64 -> Double
getFloatW = castWord64ToDouble

{-# INLINE getBoolW #-}
getBoolW :: Word64 -> Bool
getBoolW w = (w .&. 1) /= 0

{-# INLINE getCharW #-}
getCharW :: Word64 -> Char
getCharW w = chr (fromIntegral (w .&. 0x1FFFFF))  -- 21-bit Unicode

{-# INLINE getHeapIdxW #-}
getHeapIdxW :: Word64 -> Int
getHeapIdxW w = fromIntegral (w .&. payloadMask)

{-# INLINE getStrIdxW #-}
getStrIdxW :: Word64 -> Int
getStrIdxW w = fromIntegral (w .&. payloadMask)

-- ============================================================================
-- Arithmetic operations (Word64 level, zero allocation)
-- ============================================================================

{-# INLINE addIntW #-}
addIntW :: Word64 -> Word64 -> Word64
addIntW a b
    | isIntW a && isIntW b = mkIntW (getIntW a + getIntW b)
    | otherwise = mkEmptyW

{-# INLINE subIntW #-}
subIntW :: Word64 -> Word64 -> Word64
subIntW a b
    | isIntW a && isIntW b = mkIntW (getIntW a - getIntW b)
    | otherwise = mkEmptyW

{-# INLINE mulIntW #-}
mulIntW :: Word64 -> Word64 -> Word64
mulIntW a b
    | isIntW a && isIntW b = mkIntW (getIntW a * getIntW b)
    | otherwise = mkEmptyW

-- | Integer division. Returns Nothing on division by zero.
{-# INLINE divIntW #-}
divIntW :: Word64 -> Word64 -> Maybe Word64
divIntW a b
    | isIntW a && isIntW b =
        let y = getIntW b
        in if y == 0 then Nothing else Just (mkIntW (getIntW a `div` y))
    | otherwise = Nothing

-- | Integer remainder. Returns Nothing on division by zero.
{-# INLINE remIntW #-}
remIntW :: Word64 -> Word64 -> Maybe Word64
remIntW a b
    | isIntW a && isIntW b =
        let y = getIntW b
        in if y == 0 then Nothing else Just (mkIntW (getIntW a `rem` y))
    | otherwise = Nothing

{-# INLINE negIntW #-}
negIntW :: Word64 -> Word64
negIntW a
    | isIntW a  = mkIntW (negate (getIntW a))
    | otherwise = mkEmptyW

{-# INLINE absIntW #-}
absIntW :: Word64 -> Word64
absIntW a
    | isIntW a  = mkIntW (abs (getIntW a))
    | otherwise = mkEmptyW

{-# INLINE addFloatW #-}
addFloatW :: Word64 -> Word64 -> Word64
addFloatW a b
    | isFloatW a && isFloatW b = mkFloatW (getFloatW a + getFloatW b)
    | otherwise = mkEmptyW

{-# INLINE subFloatW #-}
subFloatW :: Word64 -> Word64 -> Word64
subFloatW a b
    | isFloatW a && isFloatW b = mkFloatW (getFloatW a - getFloatW b)
    | otherwise = mkEmptyW

{-# INLINE mulFloatW #-}
mulFloatW :: Word64 -> Word64 -> Word64
mulFloatW a b
    | isFloatW a && isFloatW b = mkFloatW (getFloatW a * getFloatW b)
    | otherwise = mkEmptyW

{-# INLINE divFloatW #-}
divFloatW :: Word64 -> Word64 -> Maybe Word64
divFloatW a b
    | isFloatW a && isFloatW b =
        let y = getFloatW b
        in if y == 0.0 then Nothing else Just (mkFloatW (getFloatW a / y))
    | otherwise = Nothing

{-# INLINE negFloatW #-}
negFloatW :: Word64 -> Word64
negFloatW a
    | isFloatW a = mkFloatW (negate (getFloatW a))
    | otherwise  = mkEmptyW

{-# INLINE absFloatW #-}
absFloatW :: Word64 -> Word64
absFloatW a
    | isFloatW a = mkFloatW (abs (getFloatW a))
    | otherwise  = mkEmptyW

{-# INLINE sqrtFloatW #-}
sqrtFloatW :: Word64 -> Word64
sqrtFloatW a
    | isFloatW a = mkFloatW (sqrt (getFloatW a))
    | otherwise  = mkEmptyW

-- | Int comparison returning Word64 bool
{-# INLINE intBinCmpW #-}
intBinCmpW :: (Int -> Int -> Bool) -> Word64 -> Word64 -> Word64
intBinCmpW cmp a b
    | isIntW a && isIntW b = mkBoolW (cmp (getIntW a) (getIntW b))
    | otherwise = mkBoolW False

-- | Float comparison returning Word64 bool
{-# INLINE floatBinCmpW #-}
floatBinCmpW :: (Double -> Double -> Bool) -> Word64 -> Word64 -> Word64
floatBinCmpW cmp a b
    | isFloatW a && isFloatW b = mkBoolW (cmp (getFloatW a) (getFloatW b))
    | otherwise = mkBoolW False

-- | Int to Float conversion
{-# INLINE itofW #-}
itofW :: Word64 -> Word64
itofW a
    | isIntW a  = mkFloatW (fromIntegral (getIntW a))
    | otherwise = mkEmptyW

-- | Float to Int conversion
{-# INLINE ftoiW #-}
ftoiW :: Word64 -> Word64
ftoiW a
    | isFloatW a = mkIntW (truncate (getFloatW a))
    | otherwise  = mkEmptyW

-- | Bitwise ops
{-# INLINE bandW #-}
bandW :: Word64 -> Word64 -> Word64
bandW a b | isIntW a && isIntW b = mkIntW (getIntW a .&. getIntW b)
          | otherwise = mkEmptyW

{-# INLINE borW #-}
borW :: Word64 -> Word64 -> Word64
borW a b | isIntW a && isIntW b = mkIntW (getIntW a .|. getIntW b)
         | otherwise = mkEmptyW

{-# INLINE bxorW #-}
bxorW :: Word64 -> Word64 -> Word64
bxorW a b | isIntW a && isIntW b = mkIntW (getIntW a `xor` getIntW b)
          | otherwise = mkEmptyW

{-# INLINE bshlW #-}
bshlW :: Word64 -> Word64 -> Word64
bshlW a b | isIntW a && isIntW b = mkIntW (getIntW a `shiftL` getIntW b)
          | otherwise = mkEmptyW

{-# INLINE bshrW #-}
bshrW :: Word64 -> Word64 -> Word64
bshrW a b | isIntW a && isIntW b = mkIntW (getIntW a `shiftR` getIntW b)
          | otherwise = mkEmptyW

-- | Truthiness check on Word64
{-# INLINE isValTrueW #-}
isValTrueW :: Word64 -> Bool
isValTrueW w
    | isBoolW w  = getBoolW w
    | isUnitW w  = False
    | isEmptyW w = False
    | isIntW w   = getIntW w /= 0
    | otherwise  = True

-- | Word64 equality (bitwise)
{-# INLINE eqW #-}
eqW :: Word64 -> Word64 -> Word64
eqW a b = mkBoolW (a == b)

-- ============================================================================
-- Val ↔ Word64 conversion (allocates Val, for complex dispatch)
-- ============================================================================

-- | Convert Val to Word64 for register storage.
{-# INLINE valToW #-}
valToW :: Val -> Word64
valToW (Val w) = w

-- | Convert Word64 to Val (for pattern matching in complex dispatch).
{-# INLINE wToVal #-}
wToVal :: Word64 -> Val
wToVal = Val

-- ============================================================================
-- Smart constructors (return Val, allocates — for backward compat)
-- ============================================================================

unitVal :: Val
unitVal = Val mkUnitW

emptyVal :: Val
emptyVal = Val mkEmptyW

boolVal :: Bool -> Val
boolVal = Val . mkBoolW

intVal :: Int -> Val
intVal = Val . mkIntW

floatVal :: Double -> Val
floatVal = Val . mkFloatW

charVal :: Char -> Val
charVal = Val . mkCharW

stringVal :: Text -> Val
stringVal _ = Val mkEmptyW  -- DEPRECATED: use strAlloc + mkStrW instead

conVal :: Int -> [Val] -> Val
conVal _ _ = Val mkEmptyW   -- DEPRECATED: use heapAlloc + mkHeapW instead

arrayVal :: [Val] -> Val
arrayVal _ = Val mkEmptyW   -- DEPRECATED: use heapAlloc + mkHeapW instead

closureVal :: Int -> [Val] -> Val
closureVal _ _ = Val mkEmptyW -- DEPRECATED: use heapAlloc + mkHeapW instead

papVal :: Int -> Int -> [Val] -> Val
papVal _ _ _ = Val mkEmptyW   -- DEPRECATED: use heapAlloc + mkHeapW instead

refVal :: IORef Val -> Val
refVal _ = Val mkEmptyW       -- DEPRECATED: use heapAlloc + mkHeapW instead

mutArrayVal :: MV.IOVector Val -> Val
mutArrayVal _ = Val mkEmptyW  -- DEPRECATED: use heapAlloc + mkHeapW instead

-- ============================================================================
-- Conversion helpers (need heap/string context for full conversion)
-- ============================================================================

valToString :: Val -> Text
valToString (Val w)
    | isIntW w    = T.pack (show (getIntW w))
    | isFloatW w  = T.pack (show (getFloatW w))
    | isBoolW w   = if getBoolW w then "True" else "False"
    | isCharW w   = T.singleton (getCharW w)
    | isUnitW w   = "()"
    | isEmptyW w  = ""
    | otherwise   = T.pack (show w)

valToBool :: Val -> Bool
valToBool (Val w) = isValTrueW w

valToInt :: Val -> Maybe Int
valToInt (Val w)
    | isIntW w  = Just (getIntW w)
    | otherwise = Nothing

valToFloat :: Val -> Maybe Double
valToFloat (Val w)
    | isFloatW w = Just (getFloatW w)
    | isIntW w   = Just (fromIntegral (getIntW w))
    | otherwise  = Nothing

isValTrue :: Val -> Bool
isValTrue = valToBool

-- ============================================================================
-- Heap Table: indexed storage for HeapObj values
-- ============================================================================

data HeapTable = HeapTable
    { htObjects :: !(IORef (MV.IOVector HeapObj))
    , htSize    :: !(IORef Int)  -- next free index
    }

newHeapTable :: IO HeapTable
newHeapTable = do
    objs <- MV.new 4096
    objsRef <- newIORef objs
    size <- newIORef 0
    return HeapTable { htObjects = objsRef, htSize = size }

-- | Allocate a HeapObj, return its NaN-boxed index.
heapAlloc :: HeapTable -> HeapObj -> IO Word64
heapAlloc ht obj = do
    idx <- readIORef (htSize ht)
    vec <- readIORef (htObjects ht)
    let cap = MV.length vec
    vec' <- if idx >= cap
            then do
                newVec <- MV.grow vec cap  -- double capacity
                writeIORef (htObjects ht) newVec
                return newVec
            else return vec
    MV.write vec' idx obj
    writeIORef (htSize ht) (idx + 1)
    return $! mkHeapW idx

-- | Read a HeapObj by its NaN-boxed Word64 (extracts index).
heapRead :: HeapTable -> Word64 -> IO HeapObj
heapRead ht w = do
    let idx = getHeapIdxW w
    vec <- readIORef (htObjects ht)
    MV.read vec idx

-- | Write a HeapObj back to the heap (for mutation).
heapWrite :: HeapTable -> Word64 -> HeapObj -> IO ()
heapWrite ht w obj = do
    let idx = getHeapIdxW w
    vec <- readIORef (htObjects ht)
    MV.write vec idx obj

-- ============================================================================
-- String Table: indexed storage for Text values
-- ============================================================================

data StringTable = StringTable
    { stStrings :: !(IORef (MV.IOVector Text))
    , stSize    :: !(IORef Int)
    }

newStringTable :: IO StringTable
newStringTable = do
    strs <- MV.new 1024
    strsRef <- newIORef strs
    size <- newIORef 0
    return StringTable { stStrings = strsRef, stSize = size }

-- | Allocate a string, return its NaN-boxed index.
strAlloc :: StringTable -> Text -> IO Word64
strAlloc st txt = do
    idx <- readIORef (stSize st)
    vec <- readIORef (stStrings st)
    let cap = MV.length vec
    vec' <- if idx >= cap
            then do
                newVec <- MV.grow vec cap
                writeIORef (stStrings st) newVec
                return newVec
            else return vec
    MV.write vec' idx txt
    writeIORef (stSize st) (idx + 1)
    return $! mkStrW idx

-- | Read a string by its NaN-boxed Word64.
strRead :: StringTable -> Word64 -> IO Text
strRead st w = do
    let idx = getStrIdxW w
    vec <- readIORef (stStrings st)
    MV.read vec idx
