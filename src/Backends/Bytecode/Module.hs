{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Bytecode module: the compilation unit for the VM.
--
-- A BytecodeModule contains everything needed to execute a program:
-- constant pool, function table, bytecode, constructor info, and debug info.
module Backends.Bytecode.Module
    ( BytecodeModule(..)
    , FuncInfo(..)
    , Constant(..)
    , JumpTable(..)
    , DebugInfo(..)
    , DebugEntry(..)
    , emptyModule
    , lookupFunction
    , lookupFunctionByName
    , dumpModule
    ) where

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word
import Logs (SourceInfo(..))
import Data.Hashable (Hashable(..))
import Backends.Bytecode.Instruction (decodeInstr, disassembleOne)

-- | A constant in the constant pool.
data Constant
    = KInt    !Int
    | KFloat  !Double
    | KString !Text
    | KName   !Text          -- function/variable name (for dispatch, errors)
    deriving (Show, Eq)

instance Hashable Constant where
    hashWithSalt s (KInt n)    = s `hashWithSalt` (0 :: Int) `hashWithSalt` n
    hashWithSalt s (KFloat d)  = s `hashWithSalt` (1 :: Int) `hashWithSalt` d
    hashWithSalt s (KString t) = s `hashWithSalt` (2 :: Int) `hashWithSalt` t
    hashWithSalt s (KName n)   = s `hashWithSalt` (3 :: Int) `hashWithSalt` n

-- | Jump table for SWITCH instruction (pattern match dispatch).
data JumpTable = JumpTable
    { jtDefault :: !Int              -- offset for default/wildcard case
    , jtEntries :: !(V.Vector (Int, Int))  -- (tag, offset) pairs
    } deriving (Show, Eq)

-- | Debug info entry mapping bytecode offset to source location.
data DebugEntry = DebugEntry
    { deOffset :: !Int
    , deSource :: !SourceInfo
    } deriving (Show, Eq)

-- | Per-function debug information.
data DebugInfo = DebugInfo
    { diSourceMap :: !(V.Vector DebugEntry)   -- bytecode offset -> source location
    , diTypeSig   :: !Text                    -- type signature string
    } deriving (Show, Eq)

-- | Function info in the function table.
data FuncInfo = FuncInfo
    { fiName       :: !Text        -- function name (for debug/dispatch)
    , fiArity      :: !Int         -- number of parameters
    , fiNumRegs    :: !Int         -- registers needed (from compiler)
    , fiEntry      :: !Int         -- entry point (bytecode offset)
    , fiCodeLen    :: !Int         -- bytecode length
    , fiUpvalCount :: !Int         -- number of upvalues (0 for top-level)
    , fiDebug      :: !DebugInfo   -- debug info
    } deriving (Show, Eq)

-- | A compiled bytecode module.
data BytecodeModule = BytecodeModule
    { bmName       :: !Text                    -- module name
    , bmConstants  :: !(V.Vector Constant)     -- constant pool
    , bmFunctions  :: !(V.Vector FuncInfo)     -- function table
    , bmCode       :: !(V.Vector Word32)       -- bytecode instructions
    , bmJumpTables :: !(V.Vector JumpTable)    -- jump tables for SWITCH
    , bmConsNames  :: !(V.Vector (Text, Int))  -- (consName, tag) for debug
    } deriving (Show, Eq)

-- | Empty module (for initialization).
emptyModule :: BytecodeModule
emptyModule = BytecodeModule
    { bmName       = ""
    , bmConstants  = V.empty
    , bmFunctions  = V.empty
    , bmCode       = V.empty
    , bmJumpTables = V.empty
    , bmConsNames  = V.empty
    }

-- | Look up a function by index.
lookupFunction :: BytecodeModule -> Int -> Maybe FuncInfo
lookupFunction bm idx
    | idx >= 0 && idx < V.length (bmFunctions bm) = Just (bmFunctions bm V.! idx)
    | otherwise = Nothing

-- | Look up a function by name.
lookupFunctionByName :: BytecodeModule -> Text -> Maybe (Int, FuncInfo)
lookupFunctionByName bm name =
    V.ifoldl' (\acc i fi -> case acc of
        Just _  -> acc
        Nothing -> if fiName fi == name then Just (i, fi) else Nothing
    ) Nothing (bmFunctions bm)

-- | Dump module contents for debugging.
dumpModule :: BytecodeModule -> String
dumpModule bm = unlines $
    [ "=== Bytecode Module: " ++ T.unpack (bmName bm) ++ " ==="
    , ""
    , "--- Constants (" ++ show (V.length (bmConstants bm)) ++ ") ---"
    ] ++
    [ "  k" ++ show i ++ ": " ++ showConst c
    | (i, c) <- zip [0..] (V.toList (bmConstants bm))
    ] ++
    [ ""
    , "--- Functions (" ++ show (V.length (bmFunctions bm)) ++ ") ---"
    ] ++
    concatMap (\(i, fi) ->
        [ "  f" ++ show i ++ ": " ++ T.unpack (fiName fi)
            ++ " (arity=" ++ show (fiArity fi)
            ++ ", regs=" ++ show (fiNumRegs fi)
            ++ ", entry=" ++ show (fiEntry fi)
            ++ ", len=" ++ show (fiCodeLen fi)
            ++ ")"
        ]
    ) (zip [0..] (V.toList (bmFunctions bm))) ++
    [ ""
    , "--- Bytecode (" ++ show (V.length (bmCode bm)) ++ " instructions) ---"
    ] ++
    [ "  " ++ case decodeInstr w of
        Just instr -> disassembleOne i instr
        Nothing    -> pad 4 (show i) ++ "  ???"
    | (i, w) <- zip [0..] (V.toList (bmCode bm))
    ]
  where
    showConst (KInt n)    = "Int " ++ show n
    showConst (KFloat d)  = "Float " ++ show d
    showConst (KString s) = "String " ++ show s
    showConst (KName n)   = "Name " ++ show n
    pad n s = replicate (n - length s) ' ' ++ s
