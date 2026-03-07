{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- error handling

module Logs where

import Data.Text as L hiding (replicate)
import qualified Data.HashMap.Strict as HM
import Util.IOLogger as IOLog
import Util.PrettyPrinting
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Binary (Binary(..), put, get, putWord8, getWord8)
import qualified Data.Binary as Bin
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

-- some info for debugging to attach to initially parsed module expressions
data SourceInfo = SourceInfo {
    lineNum :: !Int, colNum :: !Int, sourceFile :: String, notes :: Text
} | SourceInteractive deriving (Eq, Generic)

-- Manual Binary instance for SourceInfo (Text needs encoding)
instance Binary SourceInfo where
    put (SourceInfo l c f n) = putWord8 0 >> Bin.put l >> Bin.put c >> Bin.put f >> put (TE.encodeUtf8 n)
    put SourceInteractive = putWord8 1
    get = do
        tag <- getWord8
        case tag of
            0 -> SourceInfo <$> Bin.get <*> Bin.get <*> Bin.get <*> (TE.decodeUtf8 <$> get)
            _ -> pure SourceInteractive

-- Binary instance for Text (via UTF-8 ByteString)
putText :: Text -> Bin.Put
putText = Bin.put . TE.encodeUtf8

getText :: Bin.Get Text
getText = TE.decodeUtf8 <$> Bin.get

data LogPayload = LogPayload {
    linePos :: !Int,
    colPos :: !Int,
    filename :: String,
    message :: String
} deriving Show

-- lambda logger monad is a state monad that lies on top of IO
type LambdaLoggerMonad = LoggerMonadIO LogPayload

-- | Show all logs using the current file's source text (legacy, single-file)
showAllLogsWSource :: Text -> LambdaLoggerMonad ()
showAllLogsWSource src = do
    logs <- getAllLogs
    liftIO $ mapM_ (\err -> putStrLn $ showErrorWithSource src err ) logs

-- | Show all logs using a map of filename -> source text (multi-file)
showAllLogsWSourceMap :: HM.HashMap FilePath Text -> Text -> LambdaLoggerMonad ()
showAllLogsWSourceMap srcMap currentSrc = do
    logs <- getAllLogs
    liftIO $ mapM_ (\err ->
        let fname = filename (payload err)
            src = case HM.lookup fname srcMap of
                    Just s  -> s
                    Nothing -> currentSrc
        in putStrLn $ showErrorWithSource src err
        ) logs

-- | GHC-style error display with filename, gutter line numbers, and caret
showErrorWithSource :: Text -> LogMessage LogPayload -> String
showErrorWithSource s err' =
    let err = payload err'
        line = linePos err - 1
        col  = colPos err - 1
        srcLines = L.lines s
        lineContents = if line >= 0 && line < Prelude.length srcLines
                       then srcLines !! line
                       else "<source line unavailable>"
        lineNumStr = show (linePos err)
        gutterW = Prelude.length lineNumStr
        fname = filename err
        lvl = case IOLog.level err' of
                LogError   -> as [bold, red] "Error"
                LogWarning -> as [bold, yellow] "Warning"
                _          -> as [bold, cyan] "Info"
        header = lvl ++ " in " ++ (if Prelude.null fname then "<interactive>" else fname)
                 ++ ":" ++ show (linePos err) ++ ":" ++ show (colPos err)
        gutter = replicate gutterW ' ' ++ " |"
        lineGutter = lineNumStr ++ " | "
        caret = replicate gutterW ' ' ++ " | " ++ replicate (max 0 col) ' ' ++ as [bold, green] "^^^"
        msg = "    " ++ message err
    in Prelude.unlines ["", header, gutter, lineGutter ++ L.unpack lineContents, caret, msg]

-- | Create a LogPayload from a SourceInfo and message string.
-- Eliminates boilerplate of extracting lineNum/colNum/sourceFile manually.
mkLogPayload :: SourceInfo -> String -> LogPayload
mkLogPayload (SourceInfo ln col file _) msg = LogPayload ln col file msg
mkLogPayload SourceInteractive msg = LogPayload 0 0 "<interactive>" msg

-- | Create a LogPayload with a pass prefix, e.g. "[Env] message"
mkLogPayloadP :: String -> SourceInfo -> String -> LogPayload
mkLogPayloadP prefix si msg = mkLogPayload si ("[" ++ prefix ++ "] " ++ msg)

instance Show SourceInfo where
    show (SourceInfo l c f note) = (if Prelude.null f then "" else f ++ ":") ++ "At line " ++ show l ++ ", column " ++ show c ++ ": " ++ L.unpack note
    show SourceInteractive = "GENERATED"

instance PrettyPrint LogPayload where
    ppr (LogPayload lin col fname msg) = (if Prelude.null fname then "" else fname ++ ":") ++ "At line " ++ show lin ++ ", column " ++ show col ++ ": " ++ msg