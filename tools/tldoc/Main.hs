{-# LANGUAGE OverloadedStrings #-}

-- | tldoc: Auto-generate StandardLibrary.md from /// doc comments in .tl source files.
--
-- This is a standalone tool that does NOT depend on the tulam compiler.
-- It uses lightweight line-oriented parsing to extract doc comments and declarations.

module Main where

import           Data.Char           (isSpace)
import           Data.List           (intercalate, isPrefixOf, isSuffixOf,
                                      sortBy, groupBy, stripPrefix, foldl')
import           Data.Maybe          (mapMaybe, fromMaybe, isJust)
import           Data.Ord            (comparing)
import           Data.Time           (getCurrentTime, formatTime, defaultTimeLocale)
import           System.Directory    (doesFileExist)
import           System.FilePath     ((</>), takeDirectory, dropExtension,
                                      makeRelative)
import           System.IO           (hFlush, stdout)

-- ============================================================================
-- Data Types
-- ============================================================================

data DocBlock = DocBlock
  { dbDescription :: [String]        -- ^ Free-text description lines
  , dbParams      :: [(String, String)]  -- ^ @param name description
  , dbReturns     :: Maybe String        -- ^ @returns description
  , dbExamples    :: [String]            -- ^ @example lines
  , dbSee         :: [String]            -- ^ @see references
  , dbNotes       :: [String]            -- ^ @note callouts
  , dbSince       :: Maybe String        -- ^ @since version
  } deriving (Show)

emptyDocBlock :: DocBlock
emptyDocBlock = DocBlock [] [] Nothing [] [] [] Nothing

data DeclKind
  = DModule | DType | DPrimitive | DFunction | DAlgebra
  | DMorphism | DInstance | DEffect | DHandler | DRepr
  | DRecord | DValue | DLaw | DStructure | DImport | DExport
  deriving (Show, Eq)

data Declaration = Declaration
  { declKind      :: DeclKind
  , declName      :: String
  , declSignature :: String      -- ^ Full first line of declaration
  , declInner     :: [DocItem]   -- ^ Inner declarations (for block types)
  } deriving (Show)

data DocItem = DocItem (Maybe DocBlock) Declaration
  deriving (Show)

data Module = Module
  { modPath  :: String           -- ^ e.g. "Algebra.Eq"
  , modDoc   :: Maybe DocBlock   -- ^ Module-level doc comment
  , modItems :: [DocItem]        -- ^ Top-level documented items
  } deriving (Show)

-- ============================================================================
-- Stage 1: File Discovery
-- ============================================================================

-- | Read Base.tl to find the module export order, prepend Prelude
discoverModules :: FilePath -> IO [(String, FilePath)]
discoverModules libDir = do
  let basePath = libDir </> "Base.tl"
  exists <- doesFileExist basePath
  baseModules <- if exists
    then do
      content <- readFile basePath
      let exports = mapMaybe parseExport (lines content)
      return exports
    else return []
  let prelude = ("Prelude", libDir </> "Prelude.tl")
  let modPairs = map (\m -> (m, moduleToPath libDir m)) baseModules
  -- Also include Base.tl itself at the end
  let base = ("Base", basePath)
  return (prelude : modPairs ++ [base])

parseExport :: String -> Maybe String
parseExport line =
  let trimmed = dropWhile isSpace line
  in case stripPrefix "export " trimmed of
    Just rest -> Just $ takeWhile (\c -> c /= ';' && c /= ' ') rest
    Nothing   -> Nothing

moduleToPath :: FilePath -> String -> FilePath
moduleToPath libDir modName =
  libDir </> map dotToSlash modName ++ ".tl"
  where
    dotToSlash '.' = '/'
    dotToSlash c   = c

-- ============================================================================
-- Stage 2: Lightweight Source Parsing
-- ============================================================================

data ParseState = ParseState
  { psDocLines   :: [String]      -- ^ Current accumulating doc block lines
  , psItems      :: [DocItem]     -- ^ Parsed top-level items
  , psModuleDoc  :: Maybe DocBlock  -- ^ Module-level doc
  , psModuleName :: String
  , psBraceDepth :: Int           -- ^ Track { } nesting
  , psInBlock    :: Bool          -- ^ Inside an algebra/effect/instance block
  , psBlockItems :: [DocItem]     -- ^ Items inside current block
  , psBlockDecl  :: Maybe (Maybe DocBlock, Declaration)  -- ^ Current block's parent decl
  } deriving (Show)

initParseState :: ParseState
initParseState = ParseState [] [] Nothing "" 0 False [] Nothing

-- | Parse a single .tl file into a Module
parseFile :: String -> String -> Module
parseFile modPath content =
  let ls = lines content
      finalState = foldl' processLine initParseState ls
      -- Flush any remaining doc block
      finalState' = flushBlock finalState
      items = reverse (psItems finalState')
      mDoc = psModuleDoc finalState'
  in Module modPath mDoc items

processLine :: ParseState -> String -> ParseState
processLine ps line
  | isDocComment trimmed =
      ps { psDocLines = psDocLines ps ++ [extractDocComment trimmed] }
  | isRegularComment trimmed =
      ps  -- Skip regular comments, but DON'T clear doc block
  | isBlankLine trimmed && not (null (psDocLines ps)) =
      -- Blank lines inside doc blocks are preserved as empty description lines
      ps { psDocLines = psDocLines ps ++ [""] }
  | isBlankLine trimmed =
      ps
  | otherwise =
      processDeclaration ps trimmed line
  where
    trimmed = dropWhile isSpace line

isDocComment :: String -> Bool
isDocComment s = "///" `isPrefixOf` s

isRegularComment :: String -> Bool
isRegularComment s = "//" `isPrefixOf` s && not ("///" `isPrefixOf` s)

isBlankLine :: String -> Bool
isBlankLine = all isSpace

extractDocComment :: String -> String
extractDocComment s =
  case stripPrefix "///" s of
    Just rest -> dropWhile (== ' ') rest  -- Drop single leading space
    Nothing   -> s

-- | Identify declaration kind from a line's keyword prefix
identifyDecl :: String -> Maybe DeclKind
identifyDecl s
  | "module "    `isPrefixOf` s = Just DModule
  | "type "      `isPrefixOf` s = Just DType
  | "primitive " `isPrefixOf` s = Just DPrimitive
  | "function "  `isPrefixOf` s = Just DFunction
  | "algebra "   `isPrefixOf` s = Just DAlgebra
  | "morphism "  `isPrefixOf` s = Just DMorphism
  | "instance "  `isPrefixOf` s = Just DInstance
  | "effect "    `isPrefixOf` s = Just DEffect
  | "handler "   `isPrefixOf` s = Just DHandler
  | "repr "      `isPrefixOf` s = Just DRepr
  | "record "    `isPrefixOf` s = Just DRecord
  | "structure " `isPrefixOf` s = Just DStructure
  | "value "     `isPrefixOf` s = Just DValue
  | "law "       `isPrefixOf` s = Just DLaw
  | "import "    `isPrefixOf` s = Just DImport
  | "export "    `isPrefixOf` s = Just DExport
  | otherwise    = Nothing

processDeclaration :: ParseState -> String -> String -> ParseState
processDeclaration ps trimmed fullLine =
  case identifyDecl trimmed of
    Nothing ->
      -- Not a declaration line — track braces, possibly inner content
      let newDepth = psBraceDepth ps + countBraces trimmed
          ps' = ps { psBraceDepth = newDepth, psDocLines = [] }
      in if newDepth <= 0 && psInBlock ps
         then flushBlock ps'  -- Close block
         else ps'
    Just kind ->
      let docBlock = buildDocBlock (psDocLines ps)
          name = extractName kind trimmed
          sig = cleanSignature trimmed
          braceChange = countBraces trimmed
          newDepth = psBraceDepth ps + braceChange
          isBlockDecl = kind `elem` [DAlgebra, DMorphism, DEffect, DHandler, DInstance, DStructure, DRecord]
                        && '{' `elem` trimmed
          decl = Declaration kind name sig []
      in case kind of
        DModule ->
          -- Module declaration: associate doc with module, not as item
          let mDoc = if null (psDocLines ps) then Nothing else Just (buildDocBlock (psDocLines ps))
          in ps { psDocLines = [], psModuleName = name, psModuleDoc = mDoc
                , psBraceDepth = newDepth }
        DImport -> ps { psDocLines = [], psBraceDepth = newDepth }
        DExport -> ps { psDocLines = [], psBraceDepth = newDepth }
        _ | psInBlock ps ->
              -- We're inside a block: this is an inner declaration
              let item = DocItem (if null (psDocLines ps) then Nothing else Just docBlock) decl
              in ps { psDocLines = [], psBlockItems = psBlockItems ps ++ [item]
                    , psBraceDepth = newDepth }
          | isBlockDecl ->
              -- Starting a new block declaration
              let mDoc = if null (psDocLines ps) then Nothing else Just docBlock
              in ps { psDocLines = [], psInBlock = True
                    , psBlockDecl = Just (mDoc, decl)
                    , psBlockItems = [], psBraceDepth = newDepth }
          | otherwise ->
              -- Top-level non-block declaration
              let mDoc = if null (psDocLines ps) then Nothing else Just docBlock
                  item = DocItem mDoc decl
              in ps { psDocLines = [], psItems = item : psItems ps
                    , psBraceDepth = newDepth }

-- | Close current block, adding it as a top-level item with inner items
flushBlock :: ParseState -> ParseState
flushBlock ps =
  case psBlockDecl ps of
    Nothing -> ps { psInBlock = False, psBraceDepth = 0 }
    Just (mDoc, decl) ->
      let decl' = decl { declInner = psBlockItems ps }
          item = DocItem mDoc decl'
      in ps { psInBlock = False, psBlockDecl = Nothing, psBlockItems = []
            , psItems = item : psItems ps, psBraceDepth = 0 }

countBraces :: String -> Int
countBraces = foldl' (\acc c -> case c of '{' -> acc+1; '}' -> acc-1; _ -> acc) 0

extractName :: DeclKind -> String -> String
extractName kind s =
  let afterKeyword = case kind of
        DModule    -> drop 7 s
        DType      -> drop 5 s
        DPrimitive -> drop 10 s
        DFunction  -> drop 9 s
        DAlgebra   -> drop 8 s
        DMorphism  -> drop 10 s
        DInstance  -> drop 9 s
        DEffect    -> drop 7 s
        DHandler   -> drop 8 s
        DRepr      -> drop 5 s
        DRecord    -> drop 7 s
        DStructure -> drop 10 s
        DValue     -> drop 6 s
        DLaw       -> drop 4 s
        DImport    -> drop 7 s
        DExport    -> drop 7 s
      trimmed = dropWhile isSpace afterKeyword
  in extractNameFromTrimmed trimmed

-- | Extract name, handling operator names in parens like (==), (+), etc.
extractNameFromTrimmed :: String -> String
extractNameFromTrimmed ('(':rest) =
  case break (== ')') rest of
    (op, ')':_) -> "(" ++ op ++ ")"
    _           -> takeWhile (\c -> not (isSpace c) && c /= '{' && c /= '=' && c /= ';' && c /= ':') rest
extractNameFromTrimmed s =
  takeWhile (\c -> not (isSpace c) && c /= '(' && c /= '{' && c /= '=' && c /= ';' && c /= ':') s

cleanSignature :: String -> String
cleanSignature s =
  let s' = takeWhile (/= '{') s  -- Remove block body
      afterDef = dropAfterDef s'   -- Remove definition but handle operators
      cleaned = trimRight afterDef
      -- Strip trailing comma and semicolon
      cleaned' = stripTrailing cleaned
      -- Check if this is an intrinsic declaration
      restAfterEq = dropWhile isSpace (dropAfterDef' s)
      isIntrinsic = "= intrinsic" `isPrefixOf` restAfterEq
  in if isIntrinsic
     then stripTrailing (trimRight (dropAfterDef s')) ++ " = intrinsic"
     else cleaned'

stripTrailing :: String -> String
stripTrailing s
  | not (null s) && (last s == ',' || last s == ';') = trimRight (init s)
  | otherwise = s

-- | Drop everything after the definition '=' but skip '=' inside parens (operators)
dropAfterDef :: String -> String
dropAfterDef [] = []
dropAfterDef ('(':rest) =
  let (inside, after) = break (== ')') rest
  in case after of
    (')':more) -> '(' : inside ++ ")" ++ dropAfterDef more
    _          -> '(' : inside  -- unclosed paren, just return what we have
dropAfterDef ('=':_) = []
dropAfterDef (c:cs) = c : dropAfterDef cs

-- | Find the definition '=' skipping operators in parens, return the rest after '='
dropAfterDef' :: String -> String
dropAfterDef' [] = []
dropAfterDef' ('(':rest) =
  case break (== ')') rest of
    (_, ')':more) -> dropAfterDef' more
    _             -> []
dropAfterDef' ('=':rest) = '=' : rest
dropAfterDef' (_:cs) = dropAfterDef' cs

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

-- ============================================================================
-- Stage 3: Doc Block Building
-- ============================================================================

buildDocBlock :: [String] -> DocBlock
buildDocBlock docLines =
  foldl' processDocLine emptyDocBlock docLines

processDocLine :: DocBlock -> String -> DocBlock
processDocLine db line
  | Just rest <- stripPrefix "@param " line =
      let (name, desc) = break isSpace rest
      in db { dbParams = dbParams db ++ [(name, dropWhile isSpace desc)] }
  | Just rest <- stripPrefix "@returns " line =
      db { dbReturns = Just rest }
  | Just rest <- stripPrefix "@example " line =
      db { dbExamples = dbExamples db ++ [rest] }
  | Just rest <- stripPrefix "@see " line =
      db { dbSee = dbSee db ++ [rest] }
  | Just rest <- stripPrefix "@note " line =
      db { dbNotes = dbNotes db ++ [rest] }
  | Just rest <- stripPrefix "@since " line =
      db { dbSince = Just rest }
  | otherwise =
      db { dbDescription = dbDescription db ++ [line] }

-- ============================================================================
-- Stage 4: Markdown Generation
-- ============================================================================

generateMarkdown :: [Module] -> String -> String
generateMarkdown modules timestamp =
  unlines $ concat
    [ header timestamp
    , [""]
    , tableOfContents modules
    , [""]
    , concatMap renderModule modules
    ]

header :: String -> [String]
header timestamp =
  [ "# tulam Standard Library Reference"
  , ""
  , "> Auto-generated by `tldoc` on " ++ timestamp ++ "."
  , ">"
  , "> Do not edit manually. Regenerate with `stack exec tldoc`."
  , ""
  , "---"
  ]

tableOfContents :: [Module] -> [String]
tableOfContents modules =
  [ "## Table of Contents"
  , ""
  ] ++ concatMap renderTocGroup grouped
  where
    grouped = groupByDir modules

groupByDir :: [Module] -> [(String, [Module])]
groupByDir modules =
  let tagged = map (\m -> (modDir m, m)) modules
      groups = groupBy (\a b -> fst a == fst b) tagged
  in map (\g -> (fst (head g), map snd g)) groups

modDir :: Module -> String
modDir m =
  let p = modPath m
  in case break (== '.') p of
    (dir, _:_) -> dir
    _          -> "Root"

renderTocGroup :: (String, [Module]) -> [String]
renderTocGroup (dir, modules) =
  ("### " ++ dir) :
  "" :
  map (\m -> "- [" ++ modPath m ++ "](#" ++ anchor (modPath m) ++ ")") modules ++
  [""]

anchor :: String -> String
anchor = map (\c -> if c == '.' then '-' else toLowerChar c)
  where
    toLowerChar c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

renderModule :: Module -> [String]
renderModule m =
  [ "---"
  , ""
  , "## " ++ modPath m
  , ""
  ] ++
  renderModuleDoc (modDoc m) ++
  concatMap renderDocItem (modItems m) ++
  [""]

renderModuleDoc :: Maybe DocBlock -> [String]
renderModuleDoc Nothing = []
renderModuleDoc (Just db) =
  renderDescription (dbDescription db) ++
  renderSee (dbSee db) ++
  [""]

renderDocItem :: DocItem -> [String]
renderDocItem (DocItem mDoc decl) =
  case declKind decl of
    DType      -> renderTypeDecl mDoc decl
    DPrimitive -> renderPrimitiveDecl mDoc decl
    DFunction  -> renderFunctionDecl mDoc decl
    DAlgebra   -> renderBlockDecl "Algebra" mDoc decl
    DMorphism  -> renderBlockDecl "Morphism" mDoc decl
    DStructure -> renderBlockDecl "Structure" mDoc decl
    DInstance  -> renderInstanceDecl mDoc decl
    DEffect    -> renderBlockDecl "Effect" mDoc decl
    DHandler   -> renderBlockDecl "Handler" mDoc decl
    DRepr      -> renderReprDecl mDoc decl
    DRecord    -> renderBlockDecl "Record" mDoc decl
    DValue     -> renderValueDecl mDoc decl
    _          -> []

renderTypeDecl :: Maybe DocBlock -> Declaration -> [String]
renderTypeDecl mDoc decl =
  [ "### `type " ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++ renderDocBlockBody mDoc

renderPrimitiveDecl :: Maybe DocBlock -> Declaration -> [String]
renderPrimitiveDecl mDoc decl =
  [ "### `primitive " ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++ renderDocBlockBody mDoc

renderFunctionDecl :: Maybe DocBlock -> Declaration -> [String]
renderFunctionDecl mDoc decl =
  [ "#### `" ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++ renderDocBlockBody mDoc

renderValueDecl :: Maybe DocBlock -> Declaration -> [String]
renderValueDecl mDoc decl =
  [ "#### `" ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++ renderDocBlockBody mDoc

renderBlockDecl :: String -> Maybe DocBlock -> Declaration -> [String]
renderBlockDecl kindLabel mDoc decl =
  [ "### " ++ kindLabel ++ ": `" ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++
  renderDocBlockBody mDoc ++
  renderInnerItems (declInner decl)

renderInnerItems :: [DocItem] -> [String]
renderInnerItems [] = []
renderInnerItems items =
  let funcs = [i | i@(DocItem _ d) <- items, declKind d == DFunction]
      vals  = [i | i@(DocItem _ d) <- items, declKind d == DValue]
      laws  = [i | i@(DocItem _ d) <- items, declKind d == DLaw]
  in
  (if null funcs then [] else
    ["**Functions:**", ""] ++ concatMap renderInnerFunction funcs) ++
  (if null vals then [] else
    ["**Values:**", ""] ++ concatMap renderInnerValue vals) ++
  (if null laws then [] else
    ["**Laws:**", ""] ++ concatMap renderInnerLaw laws)

renderInnerFunction :: DocItem -> [String]
renderInnerFunction (DocItem mDoc decl) =
  [ "- `" ++ declSignature decl ++ "`"
  ] ++ indentDocBlock mDoc

renderInnerValue :: DocItem -> [String]
renderInnerValue (DocItem mDoc decl) =
  [ "- `" ++ declSignature decl ++ "`"
  ] ++ indentDocBlock mDoc

renderInnerLaw :: DocItem -> [String]
renderInnerLaw (DocItem mDoc decl) =
  [ "- *" ++ declName decl ++ "*: `" ++ declSignature decl ++ "`"
  ] ++ indentDocBlock mDoc

indentDocBlock :: Maybe DocBlock -> [String]
indentDocBlock Nothing = []
indentDocBlock (Just db) =
  let desc = renderDescriptionInline (dbDescription db)
      params = concatMap (\(n,d) -> ["  - **" ++ n ++ "**: " ++ d]) (dbParams db)
      ret = case dbReturns db of
              Just r  -> ["  - Returns: " ++ r]
              Nothing -> []
      examples = concatMap (\e -> ["  - Example: `" ++ e ++ "`"]) (dbExamples db)
  in desc ++ params ++ ret ++ examples

renderDescriptionInline :: [String] -> [String]
renderDescriptionInline [] = []
renderDescriptionInline ls =
  let text = unwords $ filter (not . null) ls
  in if null text then [] else ["  " ++ text]

renderInstanceDecl :: Maybe DocBlock -> Declaration -> [String]
renderInstanceDecl mDoc decl =
  [ "- **`" ++ declSignature decl ++ "`**"
  ] ++
  (case mDoc of
    Nothing -> []
    Just db -> renderDescriptionInline (dbDescription db)) ++
  (if null (declInner decl) then [] else
    concatMap renderInnerInstanceItem (declInner decl))

renderInnerInstanceItem :: DocItem -> [String]
renderInnerInstanceItem (DocItem _ decl) =
  ["  - `" ++ declSignature decl ++ "`"]

renderReprDecl :: Maybe DocBlock -> Declaration -> [String]
renderReprDecl mDoc decl =
  [ "### Repr: `" ++ declName decl ++ "`"
  , ""
  , "```tulam"
  , declSignature decl
  , "```"
  , ""
  ] ++ renderDocBlockBody mDoc

renderDocBlockBody :: Maybe DocBlock -> [String]
renderDocBlockBody Nothing = []
renderDocBlockBody (Just db) =
  renderDescription (dbDescription db) ++
  renderParams (dbParams db) ++
  renderReturnsSection (dbReturns db) ++
  renderExamples (dbExamples db) ++
  renderNotes (dbNotes db) ++
  renderSee (dbSee db) ++
  renderSince (dbSince db) ++
  [""]

renderDescription :: [String] -> [String]
renderDescription [] = []
renderDescription ls =
  let text = unlines ls
  in if all isSpace text then [] else lines (trimRight text) ++ [""]

renderParams :: [(String, String)] -> [String]
renderParams [] = []
renderParams ps =
  "**Parameters:**" : "" :
  map (\(n,d) -> "- `" ++ n ++ "` — " ++ d) ps ++ [""]

renderReturnsSection :: Maybe String -> [String]
renderReturnsSection Nothing = []
renderReturnsSection (Just r) = ["**Returns:** " ++ r, ""]

renderExamples :: [String] -> [String]
renderExamples [] = []
renderExamples es =
  "**Examples:**" : "" :
  "```tulam" :
  es ++
  ["```", ""]

renderNotes :: [String] -> [String]
renderNotes [] = []
renderNotes ns = concatMap (\n -> ["> **Note:** " ++ n, ""]) ns

renderSee :: [String] -> [String]
renderSee [] = []
renderSee ss = ["**See also:** " ++ intercalate ", " ss, ""]

renderSince :: Maybe String -> [String]
renderSince Nothing = []
renderSince (Just v) = ["*Since " ++ v ++ "*", ""]

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  putStrLn "tldoc: Generating standard library documentation..."
  hFlush stdout

  -- Stage 1: Discover modules
  let libDir = "lib"
  modulePairs <- discoverModules libDir
  putStrLn $ "  Found " ++ show (length modulePairs) ++ " modules"

  -- Stage 2+3: Parse each file
  modules <- mapM (\(modName, path) -> do
    exists <- doesFileExist path
    if exists
      then do
        content <- readFile path
        let m = parseFile modName content
        return (Just m)
      else do
        putStrLn $ "  Warning: " ++ path ++ " not found, skipping"
        return Nothing
    ) modulePairs

  let parsedModules = mapMaybe id modules
  putStrLn $ "  Parsed " ++ show (length parsedModules) ++ " modules"

  -- Stage 4: Generate markdown
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" now
  let markdown = generateMarkdown parsedModules timestamp

  writeFile "doc/StandardLibrary.md" markdown
  putStrLn $ "  Generated doc/StandardLibrary.md (" ++ show (length (lines markdown)) ++ " lines)"
  putStrLn "Done!"
