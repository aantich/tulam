{-# LANGUAGE OverloadedStrings #-}
-- | Debug utilities for the bytecode VM.
--
-- Provides disassembly, stack trace formatting, and module inspection.
module Backends.Bytecode.Debug
    ( disassembleModule
    , disassembleFunction
    , formatStackTrace
    , showVal
    ) where

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word

import Backends.Bytecode.Value
import Backends.Bytecode.Instruction
import Backends.Bytecode.Module
import Backends.Bytecode.VM (Frame(..))

-- | Disassemble an entire bytecode module.
disassembleModule :: BytecodeModule -> String
disassembleModule bm = unlines $
    [ "=== Module: " ++ T.unpack (bmName bm) ++ " ==="
    , ""
    , "--- Constants (" ++ show (V.length (bmConstants bm)) ++ ") ---"
    ] ++
    [ "  k" ++ show i ++ " = " ++ showConst c
    | (i, c) <- zip [0..] (V.toList (bmConstants bm))
    ] ++
    [ "" ] ++
    concatMap (\(i, fi) -> disassembleFunction bm i fi) (zip [0..] (V.toList (bmFunctions bm)))

-- | Disassemble a single function.
disassembleFunction :: BytecodeModule -> Int -> FuncInfo -> [String]
disassembleFunction bm idx fi =
    [ ""
    , "--- f" ++ show idx ++ ": " ++ T.unpack (fiName fi)
        ++ " (arity=" ++ show (fiArity fi)
        ++ ", regs=" ++ show (fiNumRegs fi)
        ++ ") ---"
    ] ++
    [ let offset = fiEntry fi + i
          w = bmCode bm V.! offset
      in case decodeInstr w of
            Just instr -> disassembleOne offset instr
            Nothing    -> show offset ++ "  ???"
    | i <- [0 .. fiCodeLen fi - 1]
    , fiEntry fi + i < V.length (bmCode bm)
    ]

-- | Format a stack trace from a list of frames.
formatStackTrace :: BytecodeModule -> [Frame] -> String
formatStackTrace bm frames = unlines $
    "Stack trace (most recent call first):" :
    [ "  " ++ show i ++ ": " ++ funcName ++ " (at PC " ++ show (frRetPC f) ++ ")"
    | (i, f) <- zip [0 :: Int ..] frames
    , let funcName = case lookupFunction bm (frFuncIdx f) of
            Just fi -> T.unpack (fiName fi)
            Nothing -> "<unknown>"
    ]

-- | Show a Val for debugging (NaN-boxed representation).
-- Note: cannot display heap objects or strings without table access;
-- shows the tag/index only.
showVal :: Val -> String
showVal (Val w)
    | isIntW w    = show (getIntW w)
    | isFloatW w  = show (getFloatW w)
    | isBoolW w   = if getBoolW w then "True" else "False"
    | isCharW w   = show (getCharW w)
    | isUnitW w   = "()"
    | isEmptyW w  = "<empty>"
    | isStrW w    = "<str:" ++ show (getStrIdxW w) ++ ">"
    | isHeapW w   = "<heap:" ++ show (getHeapIdxW w) ++ ">"
    | otherwise   = "<unknown:" ++ show w ++ ">"

-- Helper
showConst :: Constant -> String
showConst (KInt n)    = "Int " ++ show n
showConst (KFloat d)  = "Float " ++ show d
showConst (KString s) = "String " ++ show s
showConst (KName n)   = "Name " ++ show n
