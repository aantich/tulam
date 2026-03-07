{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Intrinsics
    ( lookupIntrinsic
    , IntrinsicFn
    , boolToCLM
    ) where

import Surface (Name, Literal(..), ConsTag(..))
import CLM (CLMExpr(..))
import Logs (SourceInfo(..))
import Data.HashMap.Strict as Map
import Data.Bits (Bits, (.&.), (.|.), xor, complement, shiftL, shiftR)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Char (ord, chr, toUpper, toLower, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

-- An intrinsic function takes evaluated CLM args and maybe returns a result
type IntrinsicFn = [CLMExpr] -> Maybe CLMExpr

-- Key format: "funcName\0typeName" (matches mkInstanceKey for single-param)
type IntrinsicKey = String

mkKey :: Name -> Name -> IntrinsicKey
mkKey funcName typeName = funcName ++ "\0" ++ typeName

lookupIntrinsic :: Name -> Name -> Maybe IntrinsicFn
lookupIntrinsic funcNm typNm = Map.lookup (mkKey funcNm typNm) intrinsicRegistry

-- Bool constructors matching base.tl: type Bool = True | False
-- True = tag 0, False = tag 1
clmTrue, clmFalse :: CLMExpr
clmTrue  = CLMCON (ConsTag "True" 0) []
clmFalse = CLMCON (ConsTag "False" 1) []

boolToCLM :: Bool -> CLMExpr
boolToCLM True  = clmTrue
boolToCLM False = clmFalse

-- Ordering constructors matching base.tl: type Ordering = LessThan | Equal | GreaterThan
clmLessThan, clmEqual, clmGreaterThan :: CLMExpr
clmLessThan    = CLMCON (ConsTag "LessThan" 0) []
clmEqual       = CLMCON (ConsTag "Equal" 1) []
clmGreaterThan = CLMCON (ConsTag "GreaterThan" 2) []

orderingToCLM :: Ordering -> CLMExpr
orderingToCLM LT = clmLessThan
orderingToCLM EQ = clmEqual
orderingToCLM GT = clmGreaterThan

-- ============================================================================
-- Generic builders: extract/inject + binary/unary/cmp/compare operations
-- ============================================================================

-- | Generic binary operation builder
mkBinOp :: (Literal -> Maybe a) -> (a -> Literal) -> (a -> a -> a) -> IntrinsicFn
mkBinOp extract inject op [a, b] = do
    x <- extract =<< litOf a
    y <- extract =<< litOf b
    Just $ CLMLIT (inject (op x y))
mkBinOp _ _ _ _ = Nothing

-- | Generic unary operation builder
mkUnaryOp :: (Literal -> Maybe a) -> (a -> Literal) -> (a -> a) -> IntrinsicFn
mkUnaryOp extract inject op [a] = do
    x <- extract =<< litOf a
    Just $ CLMLIT (inject (op x))
mkUnaryOp _ _ _ _ = Nothing

-- | Generic comparison builder (returns Bool)
mkCmpOp :: (Literal -> Maybe a) -> (a -> a -> Bool) -> IntrinsicFn
mkCmpOp extract op [a, b] = do
    x <- extract =<< litOf a
    y <- extract =<< litOf b
    Just $ boolToCLM (op x y)
mkCmpOp _ _ _ = Nothing

-- | Generic compare builder (returns Ordering)
mkCompare :: (Literal -> Maybe a) -> (a -> a -> Ordering) -> IntrinsicFn
mkCompare extract cmp [a, b] = do
    x <- extract =<< litOf a
    y <- extract =<< litOf b
    Just $ orderingToCLM (cmp x y)
mkCompare _ _ _ = Nothing

-- | Generic nullary constant
mkConst :: Literal -> IntrinsicFn
mkConst lit [] = Just $ CLMLIT lit
mkConst _ _ = Nothing

-- Helper: extract literal from CLMExpr
litOf :: CLMExpr -> Maybe Literal
litOf (CLMLIT l) = l `seq` Just l
litOf _ = Nothing

-- ============================================================================
-- Extractor/Injector pairs for each type
-- ============================================================================

extractInt :: Literal -> Maybe Int
extractInt (LInt n) = Just n
extractInt _ = Nothing
injectInt :: Int -> Literal
injectInt = LInt

extractInt8 :: Literal -> Maybe Int8
extractInt8 (LInt8 n) = Just n
extractInt8 _ = Nothing
injectInt8 :: Int8 -> Literal
injectInt8 = LInt8

extractInt16 :: Literal -> Maybe Int16
extractInt16 (LInt16 n) = Just n
extractInt16 _ = Nothing
injectInt16 :: Int16 -> Literal
injectInt16 = LInt16

extractInt32 :: Literal -> Maybe Int32
extractInt32 (LInt32 n) = Just n
extractInt32 _ = Nothing
injectInt32 :: Int32 -> Literal
injectInt32 = LInt32

extractInt64 :: Literal -> Maybe Int64
extractInt64 (LInt64 n) = Just n
extractInt64 _ = Nothing
injectInt64 :: Int64 -> Literal
injectInt64 = LInt64

extractWord8 :: Literal -> Maybe Word8
extractWord8 (LWord8 n) = Just n
extractWord8 _ = Nothing
injectWord8 :: Word8 -> Literal
injectWord8 = LWord8

extractWord16 :: Literal -> Maybe Word16
extractWord16 (LWord16 n) = Just n
extractWord16 _ = Nothing
injectWord16 :: Word16 -> Literal
injectWord16 = LWord16

extractWord32 :: Literal -> Maybe Word32
extractWord32 (LWord32 n) = Just n
extractWord32 _ = Nothing
injectWord32 :: Word32 -> Literal
injectWord32 = LWord32

extractWord64 :: Literal -> Maybe Word64
extractWord64 (LWord64 n) = Just n
extractWord64 _ = Nothing
injectWord64 :: Word64 -> Literal
injectWord64 = LWord64

extractFloat :: Literal -> Maybe Double
extractFloat (LFloat f) = Just f
extractFloat _ = Nothing
injectFloat :: Double -> Literal
injectFloat = LFloat

extractFloat32 :: Literal -> Maybe Float
extractFloat32 (LFloat32 f) = Just f
extractFloat32 _ = Nothing
injectFloat32 :: Float -> Literal
injectFloat32 = LFloat32

-- ============================================================================
-- Registry entry generators per type family
-- ============================================================================

-- | Generate all entries for a signed integer type
signedIntEntries :: (Num a, Integral a, Ord a, Bits a)
                 => Name
                 -> (Literal -> Maybe a) -> (a -> Literal)
                 -> a -> a -> Int
                 -> [(IntrinsicKey, IntrinsicFn)]
signedIntEntries typNm ext inj zero' one' bits =
    [ (mkKey "+" typNm,          mkBinOp ext inj (+))
    , (mkKey "-" typNm,          mkBinOp ext inj (-))
    , (mkKey "*" typNm,          mkBinOp ext inj (*))
    , (mkKey "negate" typNm,     mkUnaryOp ext inj negate)
    , (mkKey "abs" typNm,        mkUnaryOp ext inj abs)
    , (mkKey "signum" typNm,     mkUnaryOp ext inj signum)
    , (mkKey "zero" typNm,       mkConst (inj zero'))
    , (mkKey "one" typNm,        mkConst (inj one'))
    , (mkKey "div" typNm,        mkBinOp ext inj div)
    , (mkKey "mod" typNm,        mkBinOp ext inj mod)
    -- comparison
    , (mkKey "==" typNm,         mkCmpOp ext (==))
    , (mkKey "!=" typNm,         mkCmpOp ext (/=))
    , (mkKey "<" typNm,          mkCmpOp ext (<))
    , (mkKey ">" typNm,          mkCmpOp ext (>))
    , (mkKey "<=" typNm,         mkCmpOp ext (<=))
    , (mkKey ">=" typNm,         mkCmpOp ext (>=))
    , (mkKey "compare" typNm,    mkCompare ext compare)
    -- bitwise
    , (mkKey ".&." typNm,        mkBinOp ext inj (.&.))
    , (mkKey ".|." typNm,        mkBinOp ext inj (.|.))
    , (mkKey "xor" typNm,        mkBinOp ext inj xor)
    , (mkKey "complement" typNm, mkUnaryOp ext inj complement)
    , (mkKey "shiftL" typNm,     shiftLFn)
    , (mkKey "shiftR" typNm,     shiftRFn)
    , (mkKey "bitSize" typNm,    mkConst (LInt bits))
    ]
  where
    -- shiftL/shiftR take Int as second arg (shift amount)
    shiftLFn [a, b] = do
        x <- ext =<< litOf a
        s <- extractInt =<< litOf b
        Just $ CLMLIT (inj (shiftL x s))
    shiftLFn _ = Nothing
    shiftRFn [a, b] = do
        x <- ext =<< litOf a
        s <- extractInt =<< litOf b
        Just $ CLMLIT (inj (shiftR x s))
    shiftRFn _ = Nothing

-- | Generate all entries for an unsigned integer type (same ops, wrapping semantics)
unsignedIntEntries :: (Num a, Integral a, Ord a, Bits a)
                   => Name
                   -> (Literal -> Maybe a) -> (a -> Literal)
                   -> a -> a -> Int
                   -> [(IntrinsicKey, IntrinsicFn)]
unsignedIntEntries = signedIntEntries  -- same operations, Haskell handles wrapping

-- | Generate all entries for a floating-point type
floatEntries :: (Floating a, Ord a, RealFrac a)
             => Name
             -> (Literal -> Maybe a) -> (a -> Literal)
             -> a -> a
             -> [(IntrinsicKey, IntrinsicFn)]
floatEntries typNm ext inj zero' one' =
    [ (mkKey "+" typNm,          mkBinOp ext inj (+))
    , (mkKey "-" typNm,          mkBinOp ext inj (-))
    , (mkKey "*" typNm,          mkBinOp ext inj (*))
    , (mkKey "/" typNm,          mkBinOp ext inj (/))
    , (mkKey "negate" typNm,     mkUnaryOp ext inj negate)
    , (mkKey "abs" typNm,        mkUnaryOp ext inj abs)
    , (mkKey "signum" typNm,     mkUnaryOp ext inj signum)
    , (mkKey "zero" typNm,       mkConst (inj zero'))
    , (mkKey "one" typNm,        mkConst (inj one'))
    , (mkKey "recip" typNm,      mkUnaryOp ext inj recip)
    -- comparison
    , (mkKey "==" typNm,         mkCmpOp ext (==))
    , (mkKey "!=" typNm,         mkCmpOp ext (/=))
    , (mkKey "<" typNm,          mkCmpOp ext (<))
    , (mkKey ">" typNm,          mkCmpOp ext (>))
    , (mkKey "<=" typNm,         mkCmpOp ext (<=))
    , (mkKey ">=" typNm,         mkCmpOp ext (>=))
    , (mkKey "compare" typNm,    mkCompare ext compare)
    -- transcendentals
    , (mkKey "sqrt" typNm,       mkUnaryOp ext inj sqrt)
    , (mkKey "exp" typNm,        mkUnaryOp ext inj exp)
    , (mkKey "log" typNm,        mkUnaryOp ext inj log)
    , (mkKey "sin" typNm,        mkUnaryOp ext inj sin)
    , (mkKey "cos" typNm,        mkUnaryOp ext inj cos)
    , (mkKey "tan" typNm,        mkUnaryOp ext inj tan)
    , (mkKey "asin" typNm,       mkUnaryOp ext inj asin)
    , (mkKey "acos" typNm,       mkUnaryOp ext inj acos)
    , (mkKey "atan" typNm,       mkUnaryOp ext inj atan)
    , (mkKey "pow" typNm,        mkBinOp ext inj (**))
    , (mkKey "pi" typNm,         mkConst (inj pi))
    ]

-- ============================================================================
-- Conversion intrinsics (fromInt, fromFloat, convert)
-- ============================================================================

fromIntEntries :: [(IntrinsicKey, IntrinsicFn)]
fromIntEntries =
    [ (mkKey "fromInt" "Int8",   fromIntTo LInt8 fromIntegral)
    , (mkKey "fromInt" "Int16",  fromIntTo LInt16 fromIntegral)
    , (mkKey "fromInt" "Int32",  fromIntTo LInt32 fromIntegral)
    , (mkKey "fromInt" "Int64",  fromIntTo LInt64 fromIntegral)
    , (mkKey "fromInt" "UInt8",  fromIntTo LWord8 fromIntegral)
    , (mkKey "fromInt" "UInt16", fromIntTo LWord16 fromIntegral)
    , (mkKey "fromInt" "UInt32", fromIntTo LWord32 fromIntegral)
    , (mkKey "fromInt" "UInt64", fromIntTo LWord64 fromIntegral)
    , (mkKey "fromInt" "Float32", fromIntToFloat32)
    , (mkKey "fromInt" "Float64", fromIntToFloat64)
    , (mkKey "fromFloat" "Float32", fromFloatToFloat32)
    ]
  where
    fromIntTo :: (a -> Literal) -> (Int -> a) -> IntrinsicFn
    fromIntTo mkLit conv [CLMLIT (LInt n)] = Just $ CLMLIT (mkLit (conv n))
    fromIntTo _ _ _ = Nothing
    fromIntToFloat32 [CLMLIT (LInt n)] = Just $ CLMLIT (LFloat32 (fromIntegral n))
    fromIntToFloat32 _ = Nothing
    fromIntToFloat64 [CLMLIT (LInt n)] = Just $ CLMLIT (LFloat (fromIntegral n))
    fromIntToFloat64 _ = Nothing
    fromFloatToFloat32 [CLMLIT (LFloat f)] = Just $ CLMLIT (LFloat32 (realToFrac f))
    fromFloatToFloat32 _ = Nothing

-- | Conversion entries between numeric types
conversionEntries :: [(IntrinsicKey, IntrinsicFn)]
conversionEntries =
    -- Widening: signed
    [ mkConv "Int8"  "Int16"  extractInt8   (LInt16 . fromIntegral)
    , mkConv "Int16" "Int32"  extractInt16  (LInt32 . fromIntegral)
    , mkConv "Int32" "Int64"  extractInt32  (LInt64 . fromIntegral)
    , mkConv "Int32" "Int"    extractInt32  (LInt . fromIntegral)
    -- Widening: unsigned
    , mkConv "UInt8"  "UInt16" extractWord8  (LWord16 . fromIntegral)
    , mkConv "UInt16" "UInt32" extractWord16 (LWord32 . fromIntegral)
    , mkConv "UInt32" "UInt64" extractWord32 (LWord64 . fromIntegral)
    -- Widening: float
    , mkConv "Float32" "Float64" extractFloat32 (LFloat . realToFrac)
    , mkConv "Int" "Float64" extractInt (LFloat . fromIntegral)
    -- Narrowing: signed
    , mkConv "Int"   "Int32"  extractInt    (LInt32 . fromIntegral)
    , mkConv "Int32" "Int16"  extractInt32  (LInt16 . fromIntegral)
    , mkConv "Int16" "Int8"   extractInt16  (LInt8 . fromIntegral)
    -- Narrowing: float
    , mkConv "Float64" "Float32" extractFloat (LFloat32 . realToFrac)
    -- Cross-sign
    , mkConv "Int" "UInt64" extractInt (LWord64 . fromIntegral)
    , mkConv "UInt64" "Int" extractWord64 (LInt . fromIntegral)
    -- Legacy compat
    , (mkKey "toFloat" "Int",  legacyToFloat)
    , (mkKey "toInt" "Float64", legacyToInt)
    ]
  where
    -- Multi-param key for Convertible morphism: "convert\0fromType\0toType"
    mkConv :: Name -> Name -> (Literal -> Maybe a) -> (a -> Literal) -> (IntrinsicKey, IntrinsicFn)
    mkConv fromT toT ext inj =
        (mkKey "convert" fromT ++ "\0" ++ toT, convFn)
      where
        convFn [a] = do
            x <- ext =<< litOf a
            Just $ CLMLIT (inj x)
        convFn _ = Nothing

legacyToFloat :: IntrinsicFn
legacyToFloat [CLMLIT (LInt n)] = Just $ CLMLIT (LFloat (fromIntegral n))
legacyToFloat _ = Nothing

legacyToInt :: IntrinsicFn
legacyToInt [CLMLIT (LFloat f)] = Just $ CLMLIT (LInt (truncate f))
legacyToInt _ = Nothing

-- ============================================================================
-- String operations
-- ============================================================================

stringEntries :: [(IntrinsicKey, IntrinsicFn)]
stringEntries =
    [ (mkKey "concat" "String",  strBinOp (++))
    , (mkKey "++" "String",      strBinOp (++))
    , (mkKey "length" "String",  strLength)
    , (mkKey "==" "String",      strCmpOp (==))
    , (mkKey "!=" "String",      strCmpOp (/=))
    , (mkKey "<" "String",       strCmpOp (<))
    , (mkKey ">" "String",       strCmpOp (>))
    , (mkKey "<=" "String",      strCmpOp (<=))
    , (mkKey ">=" "String",      strCmpOp (>=))
    , (mkKey "compare" "String", strCompare)
    ]
  where
    strBinOp op [CLMLIT (LString a), CLMLIT (LString b)] = Just $ CLMLIT (LString (op a b))
    strBinOp _ _ = Nothing
    strCmpOp op [CLMLIT (LString a), CLMLIT (LString b)] = Just $ boolToCLM (op a b)
    strCmpOp _ _ = Nothing
    strCompare [CLMLIT (LString a), CLMLIT (LString b)] = Just $ orderingToCLM (compare a b)
    strCompare _ = Nothing
    strLength [CLMLIT (LString s)] = Just $ CLMLIT (LInt (length s))
    strLength _ = Nothing

-- ============================================================================
-- Array intrinsic operations
-- ============================================================================

arrayEntries :: [(IntrinsicKey, IntrinsicFn)]
arrayEntries =
    [ (mkKey "length" "Array",      arrayLength)
    , (mkKey "index" "Array",       arrayIndex)
    , (mkKey "slice" "Array",       arraySlice)
    , (mkKey "generate" "Array",    arrayGenerate)
    , (mkKey "set" "Array",         arraySet)
    , (mkKey "push" "Array",        arrayPush)
    , (mkKey "arrayConcat" "Array", arrayConcat')
    , (mkKey "reverse" "Array",     arrayReverse)
    , (mkKey "range" "Array",       arrayRange)
    ]
  where
    arrayLength [CLMARRAY xs] = Just $ CLMLIT (LInt (length xs))
    arrayLength _ = Nothing
    arrayIndex [CLMARRAY xs, CLMLIT (LInt i)]
        | i >= 0 && i < length xs = Just (xs !! i)
        | otherwise = Just $ CLMERR ("Array index out of bounds: " ++ show i ++ " (length " ++ show (length xs) ++ ")") SourceInteractive
    arrayIndex _ = Nothing
    arraySlice [CLMARRAY xs, CLMLIT (LInt s), CLMLIT (LInt e)]
        | s >= 0 && e >= s && e <= length xs = Just $ CLMARRAY (take (e - s) (drop s xs))
        | otherwise = Just $ CLMERR "Array slice out of bounds" SourceInteractive
    arraySlice _ = Nothing
    arrayGenerate _ = Nothing  -- requires closure evaluation, handled in interpreter
    arraySet [CLMARRAY xs, CLMLIT (LInt i), val]
        | i >= 0 && i < length xs = Just $ CLMARRAY (take i xs ++ [val] ++ drop (i + 1) xs)
        | otherwise = Just $ CLMERR ("Array set: index " ++ show i ++ " out of bounds") SourceInteractive
    arraySet _ = Nothing
    arrayPush [CLMARRAY xs, val] = Just $ CLMARRAY (xs ++ [val])
    arrayPush _ = Nothing
    arrayConcat' [CLMARRAY xs, CLMARRAY ys] = Just $ CLMARRAY (xs ++ ys)
    arrayConcat' _ = Nothing
    arrayReverse [CLMARRAY xs] = Just $ CLMARRAY (reverse xs)
    arrayReverse _ = Nothing
    arrayRange [CLMLIT (LInt start), CLMLIT (LInt end)]
        | end >= start = Just $ CLMARRAY [CLMLIT (LInt i) | i <- [start..end-1]]
        | otherwise = Just $ CLMARRAY []
    arrayRange _ = Nothing

-- ============================================================================
-- Char intrinsics (Eq, Ord)
-- ============================================================================

extractChar :: Literal -> Maybe Char
extractChar (LChar c) = Just c
extractChar _ = Nothing
injectChar :: Char -> Literal
injectChar = LChar

charEntries :: [(IntrinsicKey, IntrinsicFn)]
charEntries =
    [ (mkKey "==" "Char",      mkCmpOp extractChar (==))
    , (mkKey "!=" "Char",      mkCmpOp extractChar (/=))
    , (mkKey "<" "Char",       mkCmpOp extractChar (<))
    , (mkKey ">" "Char",       mkCmpOp extractChar (>))
    , (mkKey "<=" "Char",      mkCmpOp extractChar (<=))
    , (mkKey ">=" "Char",      mkCmpOp extractChar (>=))
    , (mkKey "compare" "Char", mkCompare extractChar compare)
    ]

-- ============================================================================
-- Show intrinsics
-- ============================================================================

showEntries :: [(IntrinsicKey, IntrinsicFn)]
showEntries =
    [ (mkKey "show" "Int",     showLit show extractInt)
    , (mkKey "show" "Float64", showLit show extractFloat)
    , (mkKey "show" "String",  showString)
    , (mkKey "show" "Char",    showChar')
    , (mkKey "show" "Int8",    showLit show extractInt8)
    , (mkKey "show" "Int16",   showLit show extractInt16)
    , (mkKey "show" "Int32",   showLit show extractInt32)
    , (mkKey "show" "Int64",   showLit show extractInt64)
    , (mkKey "show" "UInt8",   showLit show extractWord8)
    , (mkKey "show" "UInt16",  showLit show extractWord16)
    , (mkKey "show" "UInt32",  showLit show extractWord32)
    , (mkKey "show" "UInt64",  showLit show extractWord64)
    , (mkKey "show" "Float32", showLit show extractFloat32)
    , (mkKey "show" "Byte",    showLit show extractWord8)
    ]
  where
    showLit :: (a -> String) -> (Literal -> Maybe a) -> IntrinsicFn
    showLit f ext [a] = do
        x <- ext =<< litOf a
        Just $ CLMLIT (LString (f x))
    showLit _ _ _ = Nothing
    showString [CLMLIT (LString s)] = Just $ CLMLIT (LString (show s))
    showString _ = Nothing
    showChar' [CLMLIT (LChar c)] = Just $ CLMLIT (LString (show c))
    showChar' _ = Nothing

-- ============================================================================
-- Bounded intrinsics
-- ============================================================================

boundedEntries :: [(IntrinsicKey, IntrinsicFn)]
boundedEntries =
    [ (mkKey "minBound" "Int8",   mkConst (LInt8 minBound))
    , (mkKey "maxBound" "Int8",   mkConst (LInt8 maxBound))
    , (mkKey "minBound" "Int16",  mkConst (LInt16 minBound))
    , (mkKey "maxBound" "Int16",  mkConst (LInt16 maxBound))
    , (mkKey "minBound" "Int32",  mkConst (LInt32 minBound))
    , (mkKey "maxBound" "Int32",  mkConst (LInt32 maxBound))
    , (mkKey "minBound" "Int64",  mkConst (LInt64 minBound))
    , (mkKey "maxBound" "Int64",  mkConst (LInt64 maxBound))
    , (mkKey "minBound" "UInt8",  mkConst (LWord8 minBound))
    , (mkKey "maxBound" "UInt8",  mkConst (LWord8 maxBound))
    , (mkKey "minBound" "UInt16", mkConst (LWord16 minBound))
    , (mkKey "maxBound" "UInt16", mkConst (LWord16 maxBound))
    , (mkKey "minBound" "UInt32", mkConst (LWord32 minBound))
    , (mkKey "maxBound" "UInt32", mkConst (LWord32 maxBound))
    , (mkKey "minBound" "UInt64", mkConst (LWord64 minBound))
    , (mkKey "maxBound" "UInt64", mkConst (LWord64 maxBound))
    , (mkKey "minBound" "Char",   mkConst (LChar minBound))
    , (mkKey "maxBound" "Char",   mkConst (LChar maxBound))
    , (mkKey "minBound" "Byte",   mkConst (LWord8 minBound))
    , (mkKey "maxBound" "Byte",   mkConst (LWord8 maxBound))
    ]

-- ============================================================================
-- Enum intrinsics
-- ============================================================================

enumEntries :: [(IntrinsicKey, IntrinsicFn)]
enumEntries =
    [ (mkKey "succ" "Int",      mkUnaryOp extractInt injectInt (+1))
    , (mkKey "pred" "Int",      mkUnaryOp extractInt injectInt (subtract 1))
    , (mkKey "fromEnum" "Int",  \[a] -> Just a)   -- identity
    , (mkKey "toEnum" "Int",    \[a] -> Just a)   -- identity
    , (mkKey "succ" "Char",     charSucc)
    , (mkKey "pred" "Char",     charPred)
    , (mkKey "fromEnum" "Char", charFromEnum)
    , (mkKey "toEnum" "Char",   charToEnum)
    ]
  where
    charSucc [CLMLIT (LChar c)] = Just $ CLMLIT (LChar (succ c))
    charSucc _ = Nothing
    charPred [CLMLIT (LChar c)] = Just $ CLMLIT (LChar (pred c))
    charPred _ = Nothing
    charFromEnum [CLMLIT (LChar c)] = Just $ CLMLIT (LInt (ord c))
    charFromEnum _ = Nothing
    charToEnum [CLMLIT (LInt n)] = Just $ CLMLIT (LChar (chr n))
    charToEnum _ = Nothing

-- ============================================================================
-- Hashable intrinsics
-- ============================================================================

hashEntries :: [(IntrinsicKey, IntrinsicFn)]
hashEntries =
    [ (mkKey "hash" "Int",    \[a] -> Just a)  -- identity hash
    , (mkKey "hash" "String", hashString)
    , (mkKey "hash" "Char",   hashChar)
    ]
  where
    hashString [CLMLIT (LString s)] = Just $ CLMLIT (LInt (fnv1a s))
    hashString _ = Nothing
    hashChar [CLMLIT (LChar c)] = Just $ CLMLIT (LInt (ord c))
    hashChar _ = Nothing
    fnv1a :: String -> Int
    fnv1a = Prelude.foldl (\h c -> (h `xor` ord c) * 16777619) 2166136261

-- ============================================================================
-- Extended string operations
-- ============================================================================

stringExtEntries :: [(IntrinsicKey, IntrinsicFn)]
stringExtEntries =
    [ (mkKey "charAt" "String",    strCharAt)
    , (mkKey "substring" "String", strSubstring)
    , (mkKey "indexOf" "String",   strIndexOf)
    , (mkKey "trim" "String",      strTrim)
    , (mkKey "toUpper" "String",   strToUpper)
    , (mkKey "toLower" "String",   strToLower)
    , (mkKey "startsWith" "String", strStartsWith)
    , (mkKey "endsWith" "String",  strEndsWith)
    , (mkKey "replace" "String",   strReplace)
    , (mkKey "parseInt" "String",  strParseInt)
    , (mkKey "parseFloat" "String", strParseFloat)
    ]
  where
    strCharAt [CLMLIT (LString s), CLMLIT (LInt i)]
        | i >= 0 && i < length s = Just $ CLMLIT (LChar (s !! i))
        | otherwise = Just $ CLMERR ("charAt: index " ++ show i ++ " out of bounds (length " ++ show (length s) ++ ")") SourceInteractive
    strCharAt _ = Nothing
    strSubstring [CLMLIT (LString s), CLMLIT (LInt start), CLMLIT (LInt end)]
        | start >= 0 && end >= start && end <= length s = Just $ CLMLIT (LString (take (end - start) (drop start s)))
        | otherwise = Just $ CLMERR "substring: index out of bounds" SourceInteractive
    strSubstring _ = Nothing
    strIndexOf [CLMLIT (LString haystack), CLMLIT (LString needle)] = Just $ CLMLIT (LInt (findIndex' needle haystack 0))
    strIndexOf _ = Nothing
    findIndex' :: String -> String -> Int -> Int
    findIndex' _ [] _ = -1
    findIndex' needle hay@(_:rest) i
        | needle `isPrefixOf` hay = i
        | otherwise = findIndex' needle rest (i + 1)
    strTrim [CLMLIT (LString s)] = Just $ CLMLIT (LString (reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))))
    strTrim _ = Nothing
    strToUpper [CLMLIT (LString s)] = Just $ CLMLIT (LString (Prelude.map toUpper s))
    strToUpper _ = Nothing
    strToLower [CLMLIT (LString s)] = Just $ CLMLIT (LString (Prelude.map toLower s))
    strToLower _ = Nothing
    strStartsWith [CLMLIT (LString s), CLMLIT (LString prefix)] = Just $ boolToCLM (prefix `isPrefixOf` s)
    strStartsWith _ = Nothing
    strEndsWith [CLMLIT (LString s), CLMLIT (LString suffix)] = Just $ boolToCLM (suffix `isSuffixOf` s)
    strEndsWith _ = Nothing
    strReplace [CLMLIT (LString s), CLMLIT (LString old), CLMLIT (LString new')] = Just $ CLMLIT (LString (replaceAll old new' s))
    strReplace _ = Nothing
    replaceAll :: String -> String -> String -> String
    replaceAll _ _ [] = []
    replaceAll old new' s@(c:rest)
        | old `isPrefixOf` s = new' ++ replaceAll old new' (drop (length old) s)
        | otherwise = c : replaceAll old new' rest
    strParseInt [CLMLIT (LString s)] = case (readMaybe s :: Maybe Int) of
        Just n  -> Just $ CLMCON (ConsTag "Just" 1) [CLMLIT (LInt n)]
        Nothing -> Just $ CLMCON (ConsTag "Nothing" 0) []
    strParseInt _ = Nothing
    strParseFloat [CLMLIT (LString s)] = case (readMaybe s :: Maybe Double) of
        Just f  -> Just $ CLMCON (ConsTag "Just" 1) [CLMLIT (LFloat f)]
        Nothing -> Just $ CLMCON (ConsTag "Nothing" 0) []
    strParseFloat _ = Nothing

-- ============================================================================
-- SIMD stubs
-- ============================================================================

simdEntries :: [(IntrinsicKey, IntrinsicFn)]
simdEntries = concatMap mkVecStubs ["Vec2", "Vec4", "Vec8", "Vec16"]
  where
    mkVecStubs vecNm =
        [ stub "splat"   vecNm
        , stub "extract" vecNm
        , stub "insert"  vecNm
        , stub "hsum"    vecNm
        , stub "hmin"    vecNm
        , stub "hmax"    vecNm
        , stub "lanes"   vecNm
        ]
    stub fn typNm = (mkKey fn typNm, \_ -> Just $ CLMERR ("SIMD operation '" ++ fn ++ "' requires native compilation") SourceInteractive)

-- ============================================================================
-- The complete registry
-- ============================================================================

intrinsicRegistry :: HashMap IntrinsicKey IntrinsicFn
intrinsicRegistry = Map.fromList $ concat
    [ signedIntEntries   "Int"   extractInt   injectInt   0 1 64
    , signedIntEntries   "Int8"  extractInt8  injectInt8  0 1 8
    , signedIntEntries   "Int16" extractInt16 injectInt16 0 1 16
    , signedIntEntries   "Int32" extractInt32 injectInt32 0 1 32
    , signedIntEntries   "Int64" extractInt64 injectInt64 0 1 64
    , unsignedIntEntries "UInt8"  extractWord8  injectWord8  0 1 8
    , unsignedIntEntries "UInt16" extractWord16 injectWord16 0 1 16
    , unsignedIntEntries "UInt32" extractWord32 injectWord32 0 1 32
    , unsignedIntEntries "UInt64" extractWord64 injectWord64 0 1 64
    , unsignedIntEntries "Byte"   extractWord8  injectWord8  0 1 8
    , floatEntries       "Float64" extractFloat   injectFloat   0.0 1.0
    , floatEntries       "Float32" extractFloat32 injectFloat32 0.0 1.0
    , charEntries
    , stringEntries
    , stringExtEntries
    , arrayEntries
    , fromIntEntries
    , conversionEntries
    , showEntries
    , boundedEntries
    , enumEntries
    , hashEntries
    , simdEntries
    ]
