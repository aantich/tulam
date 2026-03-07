{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser where

import Text.Parsec
import Text.Parsec.Prim (many)
-- import Text.Parsec.Token as Tok
-- import qualified Text.Parsec.Expr as Ex  -- replaced by Pratt parser
import Text.Parsec.Char (string)

-- import Text.Parsec.String (parseFromFile)
import Control.Applicative ((<$>), liftA2)
import Control.Monad (foldM, guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as L

import qualified Data.Vector.Unboxed as U

import State
import Lexer
import Surface
import Logs
import Util.PrettyPrinting

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL

dbg msg = liftIO $ putStrLn msg

-- sum type:
-- type Bool = True | False;
-- type Nat = Z | Succ(n:Nat);
pSumType :: Parser Expr
pSumType = do
    pos <- getPosition
    reserved "type"
    name <- uIdentifier
    args <- try pVars <|> pure []
    -- Default return type for constructors: Name(arg1, arg2, ...) or just Name
    let defaultTp = if null args
            then Id name
            else App (Id name) (map (\v -> Id (Surface.name v)) args)
    ex   <- reservedOp "=" *> sepBy1 (pConstructor defaultTp) (reservedOp "|")
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = Constructors ex
     , lamType    = Type
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }
    return $ SumType lam

-- Sum type with optional deriving clause, returns list (SumType + generated instances)
pSumTypeWithDeriving :: Parser [Expr]
pSumTypeWithDeriving = do
    pos <- getPosition
    reserved "type"
    name <- uIdentifier
    args <- try pVars <|> pure []
    let defaultTp = if null args
            then Id name
            else App (Id name) (map (\v -> Id (Surface.name v)) args)
    ex   <- reservedOp "=" *> sepBy1 (pConstructor defaultTp) (reservedOp "|")
    -- Optional deriving clause
    derivingNames <- option [] $ do
        reserved "deriving"
        sepBy1 identifier (reservedOp ",")
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = Constructors ex
     , lamType    = Type
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }
    let sumType = SumType lam
    -- Generate Instance declarations for each derived algebra
    let deriveInstances = map (\algName -> Instance algName [Id name] [Derive] []) derivingNames
    return (sumType : deriveInstances)

-- To properly parse the type definition we need to properly parse
-- CONSTRUCTORS inside the sum type
-- For now, simple constructors as in haskell (eventually for dependent types we'll need others)
-- passing the name of the SumType to set type of the constructors --
-- eventually we'll need to parse the type (for GADT support etc)
-- Constructor parser with optional GADT return type annotation
-- e.g. VNil : Vec(a, Z)  or  VCons(head:a, tail:Vec(a,n)) : Vec(a, Succ(n))
pConstructor :: Expr -> Parser Lambda
pConstructor defaultTp = do
    pos <- getPosition
    name <- uIdentifier
    args <- try pVars <|> pure []
    -- GADT: optional explicit return type  "Con(args) : ReturnType"
    tp <- try (reservedOp ":" >> concreteType) <|> pure defaultTp
    return Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }

-- STRUCTURES ---------------------------------------------------------
pStructureWith :: String -> StructKind -> Parser Expr
pStructureWith keyword kind = do
    pos <- getPosition
    reserved keyword
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    extends <- parseExtends
    requires <- parseRequires
    -- making structure arguments Implicit for easier manipulation during
    -- pipeline stages
    let args' = map (\x@(Var nm tp val) -> Var nm (Implicit tp) val) args
    let str = Lambda {
       lamName    = name
     , params = args'
     , body       = UNDEFINED
     , lamType    = tp
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }
    reservedOp "="
    (exs, deriveBlock) <- braces $ do
        -- Parse structure members and optional derive block
        -- We collect items (functions/laws/values) and at most one derive block
        (items, db) <- parseStructMembers
        return (items, db)
    let si = StructInfo {
        structKind      = kind,
        structExtends   = extends,
        structRequires  = requires,
        structMandatory = [],
        structDerive    = deriveBlock
    }
    return $ Structure (str{body = DeclBlock exs}) si

-- Parse structure members: comma-separated items with optional trailing derive block.
-- Returns (items, deriveBlock) where items are functions/laws/values and
-- deriveBlock is the contents of an optional derive { ... } block.
parseStructMembers :: Parser ([Expr], [Expr])
parseStructMembers = do
    -- Parse first item (required for non-empty structures)
    firstItem <- option Nothing $ try $ Just <$> pStructItem
    case firstItem of
        Nothing -> return ([], [])
        Just item -> do
            -- Parse more items and/or derive block
            (rest, db) <- parseMoreItems
            return (item : rest, db)
  where
    pStructItem = try pSumType <|> try pLaw <|> try pValue <|> try pFuncOrDecl <|> pAction

    parseMoreItems :: Parser ([Expr], [Expr])
    parseMoreItems = do
        -- Try comma followed by derive block or more items
        mComma <- option Nothing (Just <$> try (reservedOp ","))
        case mComma of
            Nothing -> return ([], []) -- no more items
            Just _ -> do
                -- After comma: either derive block or another item
                mDerive <- option Nothing $ try $ Just <$> do
                    reserved "derive"
                    braces (sepBy (try pValue <|> try pFuncOrDecl) (reservedOp ","))
                case mDerive of
                    Just db -> return ([], db)
                    Nothing -> do
                        -- Try another item
                        mItem <- option Nothing $ try $ Just <$> pStructItem
                        case mItem of
                            Nothing -> return ([], [])
                            Just item -> do
                                (rest, db) <- parseMoreItems
                                return (item : rest, db)

pStructure :: Parser Expr
pStructure = pStructureWith "structure" SGeneral

pAlgebra :: Parser Expr
pAlgebra = pStructureWith "algebra" SAlgebra

pTrait :: Parser Expr
pTrait = pStructureWith "trait" SAlgebra

pMorphism :: Parser Expr
pMorphism = pStructureWith "morphism" SMorphism

pBridge :: Parser Expr
pBridge = pStructureWith "bridge" SMorphism

-- Parse extends clause: extends Parent1(a), Parent2(a, b)
parseExtends :: Parser [Expr]
parseExtends = try (do
    reserved "extends"
    sepBy1 pStructRef (reservedOp ",")
    ) <|> pure []

-- Parse requires clause: requires Struct1(a), Struct2(a, b)
parseRequires :: Parser [Expr]
parseRequires = try (do
    reserved "requires"
    sepBy1 pStructRef (reservedOp ",")
    ) <|> pure []

-- Parse a structure reference like Eq(a) or Monoid(a)
pStructRef :: Parser Expr
pStructRef = do
    name <- identifier
    args <- try (parens (sepBy1 concreteType (reservedOp ","))) <|> pure []
    return $ if null args then Id name else App (Id name) args

-- RECORDS ----------------------------------------------------------
-- record Point = { x:Nat, y:Nat };
-- record Pair(a:Type, b:Type) = { fst:a, snd:b };
-- Desugars to single-constructor sum type
pRecord :: Parser Expr
pRecord = do
    pos <- getPosition
    reserved "record"
    recName <- uIdentifier
    tparams <- try pVars <|> pure []
    reservedOp "="
    fields <- braces (sepBy1 pRecordField (reservedOp ","))
    -- Desugar: record Foo = { x:A, y:B }  =>  type Foo = { Foo(x:A, y:B) }
    -- Spread fields (..Name) are stored as Var "..Name" UNDEFINED UNDEFINED
    -- and resolved in Pass 1 when the environment is available
    let vars = map (\(nm, tp) -> Var nm tp UNDEFINED) fields
    let psi = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    let consLam = Lambda recName vars UNDEFINED (Id recName) psi
    let lam = Lambda {
       lamName = recName
     , params = tparams
     , body = Constructors [consLam]
     , lamType = Type
     , lamSrcInfo = psi
    }
    return $ SumType lam

pRecordField :: Parser (Name, Expr)
pRecordField =
    try pSpreadField <|> pNormalField

pSpreadField :: Parser (Name, Expr)
pSpreadField = do
    reservedOp ".."
    nm <- uIdentifier
    return (".." ++ nm, UNDEFINED)  -- marker: name starts with ".."

pNormalField :: Parser (Name, Expr)
pNormalField = do
    nm <- identifier
    reservedOp ":"
    tp <- concreteType
    return (nm, tp)

-- PRIMITIVE TYPES --------------------------------------------------
pPrimitive :: Parser Expr
pPrimitive = do
    pos <- getPosition
    reserved "primitive"
    name <- uIdentifier
    args <- try pVars <|> pure []
    let uLevel = if null args then U 0 else U 1
    return $ Primitive $ Lambda name args UNDEFINED uLevel (SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) "")

-- REPR declarations -------------------------------------------------
-- repr Nat as Int default where { function toRepr(...) = ..., function fromRepr(...) = ..., invariant(i) = ... };
pRepr :: Parser Expr
pRepr = do
    reserved "repr"
    userTypeExpr <- pReprUserType
    reserved "as"
    reprType <- concreteType
    isDefault <- option False (reserved "default" >> return True)
    reserved "where"
    (fns, inv) <- braces $ do
        funcs <- sepBy1 (try pFunc) (reservedOp ",")
        inv <- optionMaybe (try (reservedOp "," >> pReprInvariant))
        return (funcs, inv)
    return $ Repr userTypeExpr reprType isDefault fns inv

-- Parse user type in repr declaration: either "Name" or "Name(Arg1, Arg2, ...)"
pReprUserType :: Parser Expr
pReprUserType = do
    name <- uIdentifier
    margs <- optionMaybe (parens (sepBy1 concreteType (reservedOp ",")))
    case margs of
        Nothing   -> return $ Id name
        Just args -> return $ App (Id name) args

pReprInvariant :: Parser Expr
pReprInvariant = do
    reserved "invariant"
    args <- pVars
    reservedOp "="
    bdy <- pExpr
    return $ Function (mkLambda "invariant" args bdy (Id "Bool"))

-- CLASSES -----------------------------------------------------------
-- [abstract|sealed] class Name(fields) [extends Parent[(args)]] [implements A, B] = { methods };
pClassDecl :: Parser Expr
pClassDecl = do
    pos <- getPosition
    modifier <- option ClassNormal $
            (reserved "abstract" >> return ClassAbstract)
        <|> (reserved "sealed" >> return ClassSealed)
    reserved "class"
    name <- uIdentifier
    fields <- try pVars <|> pure []
    -- extends clause: class Dog(breed:String) extends Animal
    -- or with super args: class MyButton(label:String) extends Button(label)
    parentInfo <- optionMaybe $ do
        reserved "extends"
        pname <- uIdentifier
        superArgs <- option [] (try (parens (sepBy pExpr (reservedOp ","))))
        return (pname, superArgs)
    -- implements clause
    implAlgebras <- option [] $ do
        reserved "implements"
        sepBy1 concreteType (reservedOp ",")
    -- method body
    reservedOp "="
    (methods, methodMods) <- braces pClassMethodList
    let si = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    let lam = Lambda {
           lamName    = name
         , params     = fields
         , body       = DeclBlock methods
         , lamType    = Type
         , lamSrcInfo = si
        }
    let ci = ClassInfo {
           classParent     = parentInfo
         , classImplements = implAlgebras
         , classModifier   = modifier
         , classExtern     = Nothing
         , classMethodMods = methodMods
        }
    return $ ClassDecl lam ci

-- Parse method list inside class body, collecting method modifiers
pClassMethodList :: Parser ([Expr], [(Name, MethodModifier)])
pClassMethodList = do
    items <- sepBy pClassMember (reservedOp ",")
    let methods = map fst items
    let mods = [(n, m) | (_, Just (n, m)) <- items]
    return (methods, mods)

pClassMember :: Parser (Expr, Maybe (Name, MethodModifier))
pClassMember = do
    modifier <- option MNone $
            (reserved "override" >> return MOverride)
        <|> (reserved "final" >> return MFinal)
        <|> (reserved "static" >> return MStatic)
    decl <- pFuncDecl
    let mod' = if modifier /= MNone then Just (lamName decl, modifier) else Nothing
    return (Function decl, mod')

-- INSTANCES ---------------------------------------------------------
pInstance :: Parser Expr
pInstance = do
    reserved "instance"
    name <- identifier
    targs <- parens (sepBy1 concreteType (reservedOp ","))
    reqs <- parseRequires
    reservedOp "="
    exs <- try (reserved "intrinsic" >> return [Intrinsic])
           <|> try (reserved "derive" >> return [Derive])
           <|> braces (sepBy (try pValue <|> try pFunc) (reservedOp ","))
    return $ Instance name targs exs reqs

-- FUNCTIONS ---------------------------------------------------------
pFuncHeader :: Parser Lambda
pFuncHeader = do
    pos <- getPosition
    reserved "function"
    name <- try identifier <|> (parens operator)
    -- Optional implicit type params in brackets: function foo [s:Type] (x:s) : Int
    implicitArgs <- option [] (try (brackets (sepBy pVar (reservedOp ","))))
    args <- try pVars <|> pure []
    tp <- typeSignature
    let implicitArgs' = map (\v -> v { typ = Implicit (typ v) }) implicitArgs
    return Lambda {
       lamName    = name
     , params = implicitArgs' ++ args
     , body       = UNDEFINED
     , lamType    = tp
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }

pFuncL :: Parser Lambda
pFuncL = do
    lam <- pFuncHeader
    reservedOp "="
    ex <- try (reserved "intrinsic" >> return Intrinsic)
          <|> try (reserved "match" >> PatternMatches <$> many1 (reservedOp "|" >> pPatternMatch lam))
          <|> pExpr
    mWhere <- optionMaybe pWhereClause
    case mWhere of
        Nothing -> return $ lam { body = ex }
        Just bindings -> return $ lam { body = LetIn bindings ex }

pWhereClause :: Parser [(Var, Expr)]
pWhereClause = do
    reserved "where"
    braces (sepBy1 pWhereBinding (reservedOp ","))

pWhereBinding :: Parser (Var, Expr)
pWhereBinding = try pWhereFn <|> pWhereVal

pWhereFn :: Parser (Var, Expr)
pWhereFn = do
    lam <- pFuncL
    return (Var (lamName lam) UNDEFINED UNDEFINED, Function lam)

pWhereVal :: Parser (Var, Expr)
pWhereVal = do
    nm <- identifier
    tp <- typeSignature
    reservedOp "="
    val <- pExpr
    return (Var nm tp UNDEFINED, val)

-- Function declaration: with body (= expr) or without (abstract, for structures)
pFuncDecl :: Parser Lambda
pFuncDecl = do
    lam <- pFuncHeader
    mbody <- optionMaybe (reservedOp "=" >> (try (reserved "match" >> PatternMatches <$> many1 (reservedOp "|" >> pPatternMatch lam)) <|> pExpr))
    case mbody of
        Just ex -> return $ lam { body = ex }
        Nothing -> return lam  -- abstract: body stays UNDEFINED

pFunc :: Parser Expr
pFunc = Function <$> pFuncL

-- pFuncOrDecl: allows bodyless function declarations (for use inside structures)
pFuncOrDecl :: Parser Expr
pFuncOrDecl = Function <$> pFuncDecl

-- VALUE declarations (inside structures/instances) ----------------------
-- value empty : a          (abstract, in structure)
-- value empty = Z          (concrete, in instance)
-- value empty : a = Z      (concrete with type, in either)
pValue :: Parser Expr
pValue = do
    reserved "value"
    nm <- identifier
    tp <- typeSignature
    mval <- optionMaybe (reservedOp "=" >> pExpr)
    let val = maybe UNDEFINED id mval
    return $ Value (Var nm tp UNDEFINED) val

-- LAW declarations (inside structures) ---------------------------------
-- law reflexivity(x:a) = (x == x) === True
pLaw :: Parser Expr
pLaw = do
    pos <- getPosition
    reserved "law"
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    let lam = Lambda {
       lamName    = name
     , params = args
     , body       = UNDEFINED
     , lamType    = tp
     , lamSrcInfo = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    }
    reservedOp "="
    lawBody <- pLawExpr
    return $ Law lam lawBody

-- Parse law body expressions with === and ==> operators
-- ==> binds loosest (right-associative), === binds tighter
pLawExpr :: Parser Expr
pLawExpr = do
    parts <- sepBy1 pLawAtom (reservedOp "==>")
    return $ foldr1 Implies parts

pLawAtom :: Parser Expr
pLawAtom = try pParenLawAtom <|> pPlainLawAtom

-- Parenthesized law atom: (expr === expr) — only matches if === is inside the parens
pParenLawAtom :: Parser Expr
pParenLawAtom = parens $ do
    lhs <- pExpr
    reservedOp "==="
    rhs <- pExpr
    return (PropEq lhs rhs)

pPlainLawAtom :: Parser Expr
pPlainLawAtom = do
    lhs <- pExpr
    mRhs <- optionMaybe (reservedOp "===" >> pExpr)
    case mRhs of
        Nothing  -> return lhs
        Just rhs -> return (PropEq lhs rhs)

-- eventually to be expanded to literals etc
-- but in general only Ids and constructor applications Apps should be allowed
-- in the left part of pattern matches
pInsideLeftPattern :: Parser Expr
pInsideLeftPattern = try pNamedFieldPattern
    <|> try pApp
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> charVal)
    <|> try (Lit <$> stringVal)
    <|> symbolId
    <?> "named field pattern, constructor application, literal, or a value"

-- Named field pattern: Point { x = Z, y = _ } or Point { x } (field punning)
pNamedFieldPattern :: Parser Expr
pNamedFieldPattern = do
    nm <- uIdentifier
    fields <- braces (sepBy1 pFieldPat (reservedOp ","))
    return $ RecordPattern nm fields
  where
    pFieldPat = try pFieldPatFull <|> pFieldPatPun
    pFieldPatFull = do
        fnm <- identifier
        reservedOp "="
        pat <- pInsideLeftPattern
        return (fnm, pat)
    pFieldPatPun = do
        fnm <- identifier
        return (fnm, Id fnm)  -- field punning: { x } means { x = x }

-- pattern match Expr -> Expr
-- Lambda is given for external context to do some basic error checking,
-- e.g. # of arguments correspondence etc
-- New syntax: match | pat1, pat2 -> expr | pat1, pat2 -> expr
pPatternMatch :: Lambda -> Parser Expr
pPatternMatch lam = do
    pos <- getPosition
    ex1 <- sepBy1 pInsideLeftPattern (reservedOp ",")
    -- Filter out implicit type params — they don't participate in pattern matching
    let valueParams = filter (not . isImplicitParam) (params lam)
    if (length valueParams /= (length ex1) )
    then parserFail $
            "\nWrong number of arguments in a pattern match in a function:\n\n"
            ++ (ppr lam) ++ "\n\nThe function expects "
            ++ show (length valueParams) ++ " arguments "
            ++ "but was given " ++ show (length ex1)
            ++ " in the pattern match shown above."
    else
      do
        reservedOp "->"
        ex2 <- pExpr
        -- ok so now we have function variables in (params lam) and
        -- respective pattern matches, we need to convert the expression
        -- to CaseOf [Var] Expr
        -- Since Parser has no context, we make a straightforward conversion:
        -- f(x:t1,y:t2) = match | a, b -> expr gets converted into
        -- CaseOf [Var "x" t1 a, Var "y" t2 b] expr
        let bnd = zipWith (\arg pat -> arg {val = pat} ) valueParams ex1
        return $ CaseOf bnd ex2 (SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) "")
  where
    isImplicitParam (Var _ (Implicit _) _) = True
    isImplicitParam _ = False

    
    
    
-- EFFECTS =====================================================

-- effect Console = { function readLine() : String, function putStrLn(s:String) : Unit };
pEffectDecl :: Parser Expr
pEffectDecl = do
    reserved "effect"
    name <- uIdentifier
    ps <- try pVars <|> pure []
    reservedOp "="
    ops <- braces (sepBy pFuncDecl (reservedOp ","))
    return $ EffectDecl name ps ops

-- handler StdConsole : Console = { function readLine() = intrinsic, ... };
-- handler RefState(init:a) : State = { let state = newRef(init), function get() = readRef(state), ... };
pHandlerDecl :: Parser Expr
pHandlerDecl = do
    reserved "handler"
    name <- identifier
    ps <- try pVars <|> pure []
    reservedOp ":"
    effName <- uIdentifier
    reservedOp "="
    impls <- braces (sepBy pHandlerMember (reservedOp ","))
    return $ HandlerDecl name effName ps impls

-- Parse a handler member: either a let binding or a function
pHandlerMember :: Parser Expr
pHandlerMember = try pHandlerLet <|> try pFunc
  where
    pHandlerLet = do
        reserved "let"
        nm <- identifier
        reservedOp "="
        e <- pExpr
        return $ Value (Var nm UNDEFINED UNDEFINED) e

-- handle expr with handlerExpr
pHandleExpr :: Parser Expr
pHandleExpr = do
    reserved "handle"
    e <- pExpr
    reserved "with"
    h <- pExpr
    return $ HandleWith e h

-- ACTIONS =====================================================
pBinding :: Parser Expr
pBinding = do
    name <- identifier
    tp <- typeSignature
    reservedOp "="
    ex <- pExpr
    return $ Binding $ Var name tp ex

-- New action syntax with <- for monadic bind:
-- action name(args) : Type = { name <- expr, name = expr, expr };
pAction :: Parser Expr
pAction = do
    pos <- getPosition
    reserved "action"
    name <- identifier
    args <- try pVars <|> pure []
    tp <- typeSignature
    reservedOp "="
    -- Try new-style action block with <- support first, fall back to legacy
    stmts <- braces (sepBy1 pActionStmt (reservedOp ","))
    -- Check if any statement uses <- (new-style) or all are legacy
    let hasBinds = any isActionBind stmts
    let psi = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    if hasBinds
    then return $ Action $ Lambda {
           lamName = name
         , params = args
         , body = ActionBlock stmts
         , lamType = if (tp /= UNDEFINED) then tp else UNDEFINED
         , lamSrcInfo = psi
         }
    else do
        -- Legacy: convert ActionLet/ActionExpr back to old Statements/Binding form
        let legacyExprs = map actionStmtToExpr stmts
        return $ Action $ Lambda {
           lamName    = name
         , params = args
         , body       = Statements legacyExprs
         , lamType    = if (tp /= UNDEFINED) then tp else Id "Action"
         , lamSrcInfo = psi
        }

isActionBind :: ActionStmt -> Bool
isActionBind (ActionBind _ _) = True
isActionBind _ = False

actionStmtToExpr :: ActionStmt -> Expr
actionStmtToExpr (ActionLet nm e) = Binding (Var nm UNDEFINED e)
actionStmtToExpr (ActionExpr e) = e
actionStmtToExpr (ActionBind nm e) = Binding (Var nm UNDEFINED e) -- shouldn't happen in legacy path

-- Parse a single action statement: name <- expr, name = expr, or bare expr
pActionStmt :: Parser ActionStmt
pActionStmt =
    try pActionBindStmt
    <|> try pActionLetStmt
    <|> (ActionExpr <$> pExpr)

-- name <- expr (monadic bind)
pActionBindStmt :: Parser ActionStmt
pActionBindStmt = do
    name <- identifier
    reservedOp "<-"
    ex <- pExpr
    return $ ActionBind name ex

-- name = expr (let binding) - careful not to consume too eagerly
pActionLetStmt :: Parser ActionStmt
pActionLetStmt = do
    name <- identifier
    reservedOp "="
    ex <- pExpr
    return $ ActionLet name ex

-- | Parse an action block as an expression: action { stmt, stmt, ... }
pActionBlockExpr :: Parser Expr
pActionBlockExpr = do
    reserved "action"
    stmts <- braces (sepBy1 pActionStmt (reservedOp ","))
    return $ ActionBlock stmts
    


-- Variable with optional type signature, to be used in DEFINITIONS!!!
-- (as opposed to function calls, as there it can be any expression)
-- TODO: eventually needs to parse (= EXPR) part
pVar :: Parser Var
pVar = do
  name <- identifier --emptyStringParser -- added unnamed variables for easier record parsing
  typ  <- typeSignature
  return $ Var name typ UNDEFINED

-- variables in parenthesis in function / type etc definitions:
-- Maybe (a:Type) etc
pVars :: Parser Record
pVars = parens (sepBy pVar (reservedOp ",") )

strictTypeSignature :: Parser Expr
strictTypeSignature =
        -- try (reservedOp ":" *> tArr) <|>
        -- try (reservedOp ":" *> parens tArr) <|>
        -- try (reservedOp ":" *> parens typeAp) <|>
        -- try (reservedOp ":" *> spaces *> string "Type" *> spaces *> return SmallType) <|> -- built in "Type" parsing as SmallType right away
        try (reservedOp ":" *> concreteType) -- <|>
        -- try (reservedOp ":" *> typeVar)
       
typeSignature = try strictTypeSignature <|> pure UNDEFINED

-- Full type expression parser: handles application, arrows, parens, universes
-- Examples: Nat, Vec(a, Succ(n)), a -> b, (a -> b) -> c, Type
concreteType :: Parser Expr
concreteType = pTypeArrow

-- Arrow types: a -> b -> c (right-associative)
pTypeArrow :: Parser Expr
pTypeArrow = do
    lhs <- pTypeApp
    rest <- optionMaybe (reservedOp "->" >> pTypeArrow)
    case rest of
        Nothing  -> return lhs
        Just rhs -> return $ ArrowType lhs rhs

-- Type application: Vec(a, n), Maybe(a), or bare name/universe
-- Also: Eff { label: Type, ... | r } ResultType
pTypeApp :: Parser Expr
pTypeApp = try pEffTypeApp
    <|> try (do
    nm <- identifier
    case nm of
      "Type"  -> return (U 0)
      "Type0" -> return (U 0)
      "Type1" -> return (U 1)
      "Type2" -> return (U 2)
      "Type3" -> return (U 3)
      _ -> do
        margs <- optionMaybe (parens (sepBy1 concreteType (reservedOp ",")))
        case margs of
            Nothing   -> return $ Id nm
            Just args -> return $ App (Id nm) args
    )
    <|> try (Lit . LTuple <$> braces (sepBy1 concreteType (reservedOp ",")))  -- tuple types {a, b}
    <|> parens concreteType

-- Eff { label: Type, ... | r } ResultType
-- or Eff {} ResultType
pEffTypeApp :: Parser Expr
pEffTypeApp = do
    nm <- identifier
    guard (nm == "Eff")
    row <- braces pEffectRow
    result <- concreteType
    return $ EffType row result

-- Parse effect row: label: Type, label: Type | r  or  empty
pEffectRow :: Parser Expr
pEffectRow = try pEffectRowFields <|> pure (RecordType [] False)

pEffectRowFields :: Parser Expr
pEffectRowFields = do
    fields <- sepBy1 pEffectRowField (reservedOp ",")
    isOpen <- optionMaybe (try (reservedOp "|" >> identifier))
    case isOpen of
        Nothing -> return $ RecordType fields False
        Just rv -> return $ RecordType (fields ++ [(rv, UNDEFINED)]) True  -- row variable marker

pEffectRowField :: Parser (Name, Expr)
pEffectRowField = do
    nm <- identifier
    reservedOp ":"
    tp <- concreteType
    return (nm, tp)


int :: Parser Literal
int = LInt . fromInteger <$> integer

floating :: Parser Literal
floating = LFloat <$> float

stringVal :: Parser Literal
stringVal = LString <$> stringLit

charVal :: Parser Literal
charVal = do
    _ <- char '\''
    c <- noneOf "'\\"
         <|> (char '\\' >> escapeChar)
    _ <- char '\''
    whitespace
    return (LChar c)
  where
    escapeChar = choice
        [ char 'n' >> return '\n'
        , char 't' >> return '\t'
        , char 'r' >> return '\r'
        , char '\\' >> return '\\'
        , char '\'' >> return '\''
        , char '0' >> return '\0'
        ]


pContainers :: Parser Expr
pContainers =
        try (brackets (commaSep pExpr) >>= return . ArrayLit)
        <|> try pRecordLitExpr
        <|> try pRecordTypeExpr
        <|> try (braces (commaSep pExpr) >>= return . Lit . LTuple)
        <|> (angles (commaSep pExpr) >>= return . Lit . LVec)

-- Record literal: { name = expr, name2 = expr2 }
-- Distinguished from tuple by "name =" pattern
pRecordLitExpr :: Parser Expr
pRecordLitExpr = braces $ do
    fields <- sepBy1 pRecordLitField (reservedOp ",")
    return $ RecordLit fields
  where
    pRecordLitField = do
        nm <- identifier
        reservedOp "="
        val <- pExpr
        return (nm, val)

-- Record type: { name : Type, name2 : Type } or { name : Type, .. }
-- Distinguished from tuple by "name :" pattern (using identifier then colon)
pRecordTypeExpr :: Parser Expr
pRecordTypeExpr = braces $ do
    fields <- sepBy1 pRecordTypeField (reservedOp ",")
    isOpen <- option False (reservedOp "," >> reservedOp ".." >> return True)
    return $ RecordType fields isOpen
  where
    pRecordTypeField = do
        nm <- identifier
        reservedOp ":"
        tp <- concreteType
        return (nm, tp)

{- 
=====================================================================================
-}
-- Building expression parser - for RIGHT HAND SIDE ONLY!!!
-- Uses a Pratt parser for correct operator precedence from the fixity table.

-- Look up fixity for an operator from the environment in the parser monad
getFixity :: Name -> Parser OperatorFixity
getFixity op = lift (State.lookupFixity op . currentEnvironment <$> get)

-- Pratt parser: parse expression with minimum precedence level
prattExpr :: Int -> Parser Expr
prattExpr minPrec = do
    lhs <- pPrefixExpr
    prattLoop minPrec lhs

-- Prefix expressions: unary minus or base factor
pPrefixExpr :: Parser Expr
pPrefixExpr = (try (reservedOp "-") >> UnaryOp "-" <$> pPrefixExpr)
          <|> pFactor

-- Pratt loop: consume binary operators while they bind tighter than minPrec
prattLoop :: Int -> Expr -> Parser Expr
prattLoop minPrec lhs = do
    mOp <- optionMaybe (try (lookAhead operator))
    case mOp of
        Nothing -> return lhs
        Just opName -> do
            fix <- getFixity opName
            let prec = fixPrec fix
            if prec < minPrec
                then return lhs
                else do
                    _ <- operator  -- consume the operator
                    let rightMinPrec = case fixAssoc fix of
                            AssocLeft  -> prec + 1
                            AssocRight -> prec
                            AssocNone  -> prec + 1
                    rhs <- prattExpr rightMinPrec
                    -- For non-associative operators, check that we don't chain
                    case fixAssoc fix of
                        AssocNone -> do
                            mNext <- optionMaybe (try (lookAhead operator))
                            case mNext of
                                Just nextOp -> do
                                    nextFix <- getFixity nextOp
                                    if fixPrec nextFix == prec
                                        then fail $ "Non-associative operator " ++ show opName ++ " cannot be chained"
                                        else prattLoop minPrec (BinaryOp opName lhs rhs)
                                Nothing -> return (BinaryOp opName lhs rhs)
                        _ -> prattLoop minPrec (BinaryOp opName lhs rhs)

pExprBase :: Parser Expr
pExprBase = try (Lit . LVec <$> angles (commaSep pFactor))  -- vector literals <1, 2, 3> (use pFactor to avoid > consumed as operator)
    <|> prattExpr 0

pExpr :: Parser Expr
pExpr = do
    e <- pExprBase
    mCast <- optionMaybe (try (reserved "as" >> concreteType))
    case mCast of
        Nothing -> return e
        Just tp -> return $ ReprCast e tp

pIfThenElse :: Parser Expr
pIfThenElse = do
    reserved "if"
    cond <- pExpr
    reserved "then"
    e1 <- pExpr
    reserved "else"
    e2 <- pExpr
    return $ IfThenElse cond e1 e2

pLetIn :: Parser Expr
pLetIn = do
    reserved "let"
    bindings <- sepBy1 pLetBinding (reservedOp ",")
    reserved "in"
    bdy <- pExpr
    return $ LetIn bindings bdy

pLetBinding :: Parser (Var, Expr)
pLetBinding = do
    nm <- identifier
    tp <- typeSignature
    reservedOp "="
    val <- pExpr
    return (Var nm tp UNDEFINED, val)

-- Anonymous lambda: \x -> expr  or  \x:Type, y -> expr
pAnonLambda :: Parser Expr
pAnonLambda = do
    reservedOp "\\"
    args <- sepBy1 pVar (reservedOp ",")
    reservedOp "->"
    bdy <- pExpr
    return $ Function (mkLambda "" args bdy UNDEFINED)

-- Inline match expression: match expr | pat -> body | pat -> body
-- Desugars to a lambda application: (\__m -> PatternMatches [CaseOf ...]) (scrutinee)
pMatchExpr :: Parser Expr
pMatchExpr = do
    reserved "match"
    scrutinee <- pExpr
    cases <- many1 (reservedOp "|" >> pMatchCase)
    -- Desugar: match expr | pat -> body  =>  (\__m -> PatternMatches [...]) (expr)
    -- Each case: | pat -> body  =>  CaseOf [Var "__m" UNDEFINED pat] body
    let matchVar = Var "__m" UNDEFINED UNDEFINED
    let casesExprs = map (\(pat, bdy, si) -> CaseOf [matchVar {val = pat}] bdy si) cases
    let matchLam = mkLambda "" [matchVar] (PatternMatches casesExprs) UNDEFINED
    return $ App (Function matchLam) [scrutinee]

pMatchCase :: Parser (Expr, Expr, SourceInfo)
pMatchCase = do
    pos <- getPosition
    pat <- pInsideLeftPattern
    reservedOp "->"
    bdy <- pExpr
    return (pat, bdy, SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) "")

-- Base factor without postfix operators
pFactorBase :: Parser Expr
pFactorBase = try pIfThenElse
    <|> try pLetIn
    <|> try pAnonLambda
    <|> try pHandleExpr
    <|> try pActionBlockExpr
    <|> try pMatchExpr
    <|> try pRecordConstruct
    <|> try pApp
    <|> try (parens pExpr)
    <|> try (Id <$> parens operator)   -- (op) as value: (+), (!=), etc.
    <|> try symbolId
    <|> try (Lit <$> floating)
    <|> try (Lit <$> int)
    <|> try (Lit <$> charVal)
    <|> try (Lit <$> stringVal)
    <|> pContainers
    <?> "if/then/else, let/in, match, handle, container, literal, symbol id or parenthesized expression"

-- Named record construction: Point { x = 1, y = 2 }
pRecordConstruct :: Parser Expr
pRecordConstruct = do
    nm <- uIdentifier
    fields <- braces (sepBy1 pFieldAssign (reservedOp ","))
    return $ RecordConstruct nm fields
  where
    pFieldAssign = do
        fnm <- identifier
        reservedOp "="
        val <- pExpr
        return (fnm, val)

-- Factor with postfix dot-access and record update
pFactor :: Parser Expr
pFactor = do
    base <- pFactorBase
    pPostfix base

-- Postfix operators: dot-access chains, method calls, and record update
pPostfix :: Expr -> Parser Expr
pPostfix e = do
    e' <- pDotMethodChain e
    mUpd <- optionMaybe (try pUpdateFields)
    return $ maybe e' (RecordUpdate e') mUpd

-- Chain of .fieldName and .method(args) accesses (left-associative)
-- Handles: obj.field, obj.method(args), obj.field.method(args).field2, etc.
pDotMethodChain :: Expr -> Parser Expr
pDotMethodChain e = do
    result <- pDotStep e
    case result of
        Nothing -> return e
        Just e' -> pDotMethodChain e'

pDotStep :: Expr -> Parser (Maybe Expr)
pDotStep e = optionMaybe $ try $ do
    reservedOp "."
    field <- identifier
    -- After dot-field, check if followed by (args) — method call
    mArgs <- optionMaybe (try (parens (sepBy pExpr (reservedOp ","))))
    case mArgs of
        Just args -> return $ App (RecFieldAccess (field, -1) e) args
        Nothing   -> return $ RecFieldAccess (field, -1) e

-- Record update fields: { field = expr, ... }
pUpdateFields :: Parser [(Name, Expr)]
pUpdateFields = braces (sepBy1 pFieldAssign (reservedOp ","))
  where
    pFieldAssign = do
        fnm <- identifier
        reservedOp "="
        val <- pExpr
        return (fnm, val)

symbolId :: Parser Expr
symbolId = do 
    s <- identifier
    return $ Id s

-- Clear function application
pApp :: Parser Expr
pApp = do
    func <- try (parens pExpr) <|> try (Id <$> parens operator) <|> (Id <$> identifier)
    args <- parens (sepBy pExpr (reservedOp ",") )
    return $ App func args

-- MODULE SYSTEM PARSING ---------------------------------------------------

-- Module declaration: module Algebra.Ring;
pModuleDecl :: Parser Expr
pModuleDecl = do
    reserved "module"
    path <- pModulePath
    return $ ModuleDecl path

-- Dotted module path: Algebra.Ring.Field
pModulePath :: Parser [Name]
pModulePath = sepBy1 uIdentifier (reservedOp ".")

-- Import declaration with all forms
pImport :: Parser Expr
pImport = do
    reserved "import"
    path <- pModulePath
    spec <- pImportSpec
    tgt <- optionMaybe (try (reserved "target" >> identifier))
    return $ Import path spec tgt

pImportSpec :: Parser ImportSpec
pImportSpec =
    try (reserved "hiding" >> parens (sepBy1 identifier (reservedOp ",")) >>= return . ImportHiding)
    <|> try (reserved "as" >> identifier >>= return . ImportAs)
    <|> try (parens (sepBy1 identifier (reservedOp ",")) >>= return . ImportOnly)
    <|> pure ImportAll

-- Open declaration: open Algebra.Ring;
pOpen :: Parser Expr
pOpen = do
    reserved "open"
    path <- pModulePath
    return $ Open path

-- Export declaration: export Algebra.Ring; or export Algebra.Ring (Semiring, Field);
pExport :: Parser Expr
pExport = do
    reserved "export"
    path <- pModulePath
    mNames <- optionMaybe (try (parens (sepBy1 identifier (reservedOp ","))))
    return $ Export path mNames

-- Private modifier: wraps the next declaration
pPrivate :: Parser Expr
pPrivate = do
    reserved "private"
    decl <- pDeclBody
    return $ PrivateDecl decl

-- Opaque type: opaque type Name = Type;
pOpaque :: Parser Expr
pOpaque = do
    reserved "opaque"
    reserved "type"
    name <- uIdentifier
    args <- try pVars <|> pure []
    reservedOp "="
    reprType <- concreteType
    let uLevel = if null args then U 0 else U 1
    return $ OpaqueTy (mkLambda name args UNDEFINED uLevel) reprType

-- Target block: target dotnet { ... };
pTargetBlock :: Parser Expr
pTargetBlock = do
    reserved "target"
    try pTargetSwitchExpr <|> pTargetBlockExpr

pTargetBlockExpr :: Parser Expr
pTargetBlockExpr = do
    tgt <- identifier
    exs <- braces (sepBy pDeclBody (reservedOp ";"))
    return $ TargetBlock tgt exs

pTargetSwitchExpr :: Parser Expr
pTargetSwitchExpr = do
    cases <- many1 (reservedOp "|" >> pTargetCase)
    return $ TargetSwitch cases

pTargetCase :: Parser (Name, Expr)
pTargetCase = do
    tgt <- identifier
    reservedOp "->"
    e <- pExpr
    return (tgt, e)

-- All non-module declarations (used by pPrivate and pDef)
pDeclBody :: Parser Expr
pDeclBody = try pFixityDecl
        <|> try pRepr
        <|> try pPrimitive
        <|> try pSumType
        <|> try pRecord
        <|> try pEffectDecl
        <|> try pHandlerDecl
        <|> try pClassDecl
        <|> try pAlgebra
        <|> try pTrait
        <|> try pMorphism
        <|> try pBridge
        <|> try pStructure
        <|> try pInstance
        <|> try pFunc
        <|> try pAction
        <|> pBinding

-- Building top level parsers: module-level declarations + all non-module declarations
pDef :: Parser Expr
pDef =  try pModuleDecl
        <|> try pImport
        <|> try pOpen
        <|> try pExport
        <|> try pPrivate
        <|> try pOpaque
        <|> try pTargetBlock
        <|> pDeclBody


pToplevel :: Parser [Expr]
pToplevel = fmap concat $ many $ do
    pos <- getPosition
    let si = SourceInfo (sourceLine pos) (sourceColumn pos) (sourceName pos) ""
    -- Try sum type with deriving first (returns multiple exprs), then single-expr pDef
    defs <- try (do { ds <- pSumTypeWithDeriving; reservedOp ";"; return ds })
            <|> (do { d <- pDef; reservedOp ";"; return [d] })
    ints <- lift get
    let pm = map (\d -> (d, si)) defs ++ parsedModule ints
    lift $ put ints {parsedModule = pm}
    return defs
    
--parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = runParserT (contents pDef) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
parseWholeFile s fn = runParserT (contents pToplevel) initialParserState fn s

-- testing parsers
hlpp p s = runParserT (contents p) initialParserState "<stdin>" s
testParser p str = liftIO $ runIntState (hlpp p (L.pack str)) emptyIntState

----------------------------------------------------
-- PARSER ----------------------------------------------------
----------------------------------------------------
op :: Parser String
op = operator

-- Fixity declaration: infixl/infixr/infix prec (op1), (op2), ...;
pFixityDecl :: Parser Expr
pFixityDecl = do
    assoc <- (reserved "infixl" >> return AssocLeft)
         <|> (reserved "infixr" >> return AssocRight)
         <|> (reserved "infix"  >> return AssocNone)
    prec <- fromInteger <$> integer
    ops <- sepBy1 (parens operator) (reservedOp ",")
    -- Register fixity into environment immediately (for same-file use)
    lift $ do
        s <- get
        let env = currentEnvironment s
        let env' = Prelude.foldl (\e o -> addFixity o (OperatorFixity assoc (fromIntegral prec)) e) env ops
        put s { currentEnvironment = env' }
    return $ FixityDecl assoc (fromIntegral prec) ops

    
-- helper parsers: lower case and upper case
lIdentifier = skipMany space >> lookAhead lower >> identifier
uIdentifier = skipMany space >> lookAhead upper >> identifier




-- contents :: Parser a -> Parser a
contents p = do
    whitespace
    r <- p
    eof
    return r

    
-- parseExpr :: String -> Either ParseError Expr
parseExpr s = runParserT (contents pExpr) initialParserState "<stdin>" s

--parseToplevel :: String -> Either ParseError [Expr]
-- parseToplevel s = runParserT (contents defn) initialParserState "<stdin>" s

-- give a text and then parse it - need to store source for error reporting
-- parseWholeFile s fn = runParserT (contents toplevel) initialParserState fn s

-- parse a given file
parseToplevelFile :: String -> IntState (Either ParseError [Expr])
parseToplevelFile name = parseFromFile (contents pToplevel) name initialParserState

parseFromFile :: Parser a -> String -> ParserState -> IntState (Either ParseError a)
-- redefining parse from file to work with our state - just a quick and dirty fix
parseFromFile p fname st
    = liftIO (T.readFile fname) >>= runParserT p st fname
             
    
    
    
   
-- used to show syntax errors together with source (first argument)
showSyntaxError :: L.Text -> ParseError -> String
showSyntaxError s err =
    let pos  = errorPos err
        line = fromIntegral $ sourceLine pos - 1
        col  = fromIntegral $ sourceColumn pos - 1
        fname = sourceName pos
        srcLines = L.lines s
        lineContents = if line >= 0 && line < Prelude.length srcLines
                       then srcLines !! line
                       else "<source line unavailable>"
        lineNumStr = show (sourceLine pos)
        gutterW = Prelude.length lineNumStr
        header = as [bold, red] "[Parse] Error" ++ " in "
                 ++ (if Prelude.null fname then "<interactive>" else fname)
                 ++ ":" ++ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
        gutter = replicate gutterW ' ' ++ " |"
        lineGutter = lineNumStr ++ " | "
        caret = replicate gutterW ' ' ++ " | " ++ replicate (max 0 (fromIntegral col)) ' '
                ++ as [bold, green] "^"
        msg = "    " ++ show err
    in Prelude.unlines ["", header, gutter, lineGutter ++ L.unpack lineContents, caret, msg]