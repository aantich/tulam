{-# LANGUAGE OverloadedStrings #-}
-- | Unified test result type and backend interface for multi-backend testing.
--
-- All backends (bytecode VM, LLVM native, etc.) convert their native results
-- to TestResult for uniform assertion checking.
module TestBackend
    ( TestResult(..)
    , Backend(..)
    -- * Smart constructors
    , trTrue, trFalse
    , trNat
    , trNothing, trJust
    , trNil, trCons
    , trLeft, trRight
    , trPair
    , trLT, trEQ, trGT
    , trUnit
    -- * Parsing
    , parseTestResult
    ) where

-- | Unified test result that all backends can produce.
data TestResult
    = TRInt Int
    | TRFloat Double
    | TRBool Bool
    | TRString String
    | TRChar Char
    | TRCon String [TestResult]   -- ^ Constructor with fields: Con(field1, field2, ...)
    | TRArray [TestResult]
    | TRUnit
    | TRError String
    deriving (Eq, Show)

-- | A test backend that can evaluate tulam expressions.
data Backend = Backend
    { backendName    :: String
    , backendEval    :: String -> IO TestResult     -- ^ Evaluate expression string
    , backendAvail   :: IO Bool                     -- ^ Is this backend available?
    }

-- ============================================================================
-- Smart constructors
-- ============================================================================

trTrue, trFalse :: TestResult
trTrue  = TRCon "True" []
trFalse = TRCon "False" []

trNat :: Int -> TestResult
trNat 0 = TRCon "Z" []
trNat n = TRCon "Succ" [trNat (n - 1)]

trNothing :: TestResult
trNothing = TRCon "Nothing" []

trJust :: TestResult -> TestResult
trJust x = TRCon "Just" [x]

trNil :: TestResult
trNil = TRCon "Nil" []

trCons :: TestResult -> TestResult -> TestResult
trCons x xs = TRCon "Cons" [x, xs]

trLeft :: TestResult -> TestResult
trLeft x = TRCon "Left" [x]

trRight :: TestResult -> TestResult
trRight x = TRCon "Right" [x]

trPair :: TestResult -> TestResult -> TestResult
trPair a b = TRCon "Pair" [a, b]

trLT, trEQ, trGT :: TestResult
trLT = TRCon "LT" []
trEQ = TRCon "EQ" []
trGT = TRCon "GT" []

trUnit :: TestResult
trUnit = TRUnit

-- ============================================================================
-- Parsing test results from string representation
-- ============================================================================

-- | Parse a test result from its string representation.
-- Handles: integers, floats, booleans, strings, constructors, arrays.
parseTestResult :: String -> Either String TestResult
parseTestResult s = case parse (dropWhile (== ' ') s) of
    Just (r, rest) | all (== ' ') rest -> Right r
    Just (_, rest) -> Left $ "Unexpected trailing: " ++ rest
    Nothing -> Left $ "Cannot parse: " ++ s
  where
    parse [] = Nothing
    -- Booleans
    parse ('T':'r':'u':'e':rest)  = Just (trTrue, rest)
    parse ('F':'a':'l':'s':'e':rest) = Just (trFalse, rest)
    -- Unit
    parse ('(':')':rest) = Just (TRUnit, rest)
    -- String literal
    parse ('"':rest) = parseString rest ""
    -- Char literal
    parse ('\'':c:'\'':rest) = Just (TRChar c, rest)
    -- Array
    parse ('[':rest) = parseArray rest []
    -- Negative number
    parse ('-':rest) = case span isDigitOrDot rest of
        (num, rest') | not (null num) ->
            if '.' `elem` num
            then Just (TRFloat (read ('-':num)), rest')
            else Just (TRInt (read ('-':num)), rest')
        _ -> Nothing
    -- Number
    parse s'@(c:_) | c >= '0' && c <= '9' =
        let (num, rest') = span isDigitOrDot s'
        in if '.' `elem` num
           then Just (TRFloat (read num), rest')
           else Just (TRInt (read num), rest')
    -- Constructor
    parse s'@(c:_) | c >= 'A' && c <= 'Z' =
        let (name, rest') = span isIdentChar s'
        in case rest' of
            ('(':rest'') -> parseTuple rest'' [] >>= \(fields, rest''') ->
                Just (TRCon name fields, rest''')
            _ -> Just (TRCon name [], rest')
    parse _ = Nothing

    parseString [] _ = Nothing
    parseString ('"':rest) acc = Just (TRString (reverse acc), rest)
    parseString ('\\':'n':rest) acc = parseString rest ('\n':acc)
    parseString ('\\':'t':rest) acc = parseString rest ('\t':acc)
    parseString ('\\':'\\':rest) acc = parseString rest ('\\':acc)
    parseString ('\\':'"':rest) acc = parseString rest ('"':acc)
    parseString (c:rest) acc = parseString rest (c:acc)

    parseTuple (')':rest) acc = Just (reverse acc, rest)
    parseTuple s' acc = case parse (dropWhile (== ' ') s') of
        Just (r, rest') -> case dropWhile (== ' ') rest' of
            (',':rest'') -> parseTuple rest'' (r:acc)
            (')':rest'') -> Just (reverse (r:acc), rest'')
            _ -> Nothing
        Nothing -> Nothing

    parseArray (']':rest) acc = Just (TRArray (reverse acc), rest)
    parseArray s' acc = case parse (dropWhile (== ' ') s') of
        Just (r, rest') -> case dropWhile (== ' ') rest' of
            (',':rest'') -> parseArray rest'' (r:acc)
            (']':rest'') -> Just (TRArray (reverse (r:acc)), rest'')
            _ -> Nothing
        Nothing -> Nothing

    isDigitOrDot c = (c >= '0' && c <= '9') || c == '.'
    isIdentChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                    || (c >= '0' && c <= '9') || c == '_'
