{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Parser
-- Copyright   :  (c) Niklas Broberg 2004-2009
--                (c) Michael Sloan 2013
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Annotated parser for Haskell with extensions.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated.Parser
    (
    -- * General parsing
      Parseable(parse, parseWithMode, parseWithComments)
    , ListOf(..), unListOf
    -- * Parsing of specific AST elements
    -- ** Modules
    , parseModule, parseModuleWithMode, parseModuleWithComments
    -- ** Expressions
    , parseExp, parseExpWithMode, parseExpWithComments
    -- ** Statements
    , parseStmt, parseStmtWithMode, parseStmtWithComments
    -- ** Patterns
    , parsePat, parsePatWithMode, parsePatWithComments
    -- ** Declarations
    , parseDecl, parseDeclWithMode, parseDeclWithComments
    -- ** Types
    , parseType, parseTypeWithMode, parseTypeWithComments
    -- * Non-greedy parsers
    , NonGreedy(..)
    -- ** Module head parsers
    , getTopPragmas, readExtensions, pragmasToExtensions
    , PragmasAndModuleName(..), PragmasAndModuleHead(..), ModuleHeadAndImports(..)
    ) where

import Data.Data hiding (Fixity)
import Data.Either (partitionEithers)
import Language.Haskell.Exts.Annotated.Fixity as A
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.InternalParser
import Language.Haskell.Exts.ParseMonad hiding (getModuleName)
import Language.Haskell.Exts.SrcLoc

normalParser :: AppFixity a => P (a SrcSpanInfo) -> Maybe [A.Fixity] -> P (a SrcSpanInfo)
normalParser p Nothing = p
normalParser p (Just fixs) = p >>= \ast -> applyFixities fixs ast `atSrcLoc` noLoc

normalParserNoFixity :: P (a SrcSpanInfo) -> Maybe [A.Fixity] -> P (a SrcSpanInfo)
normalParserNoFixity p _ = p

instance Parseable (Decl           SrcSpanInfo) where parser = normalParser mparseDecl
instance Parseable (Exp            SrcSpanInfo) where parser = normalParser mparseExp
instance Parseable (Module         SrcSpanInfo) where parser = normalParser mparseModule
instance Parseable (Pat            SrcSpanInfo) where parser = normalParser mparsePat
instance Parseable (Stmt           SrcSpanInfo) where parser = normalParser mparseStmt
instance Parseable (Type           SrcSpanInfo) where parser = normalParserNoFixity mparseType

-- Non-greedy parsers (should use ng- prefixed parses exported by InternalParser)

-- ngnormalParser :: AppFixity a => P (a SrcSpanInfo) -> Maybe [Fixity] -> P (NonGreedy (a SrcSpanInfo))
-- ngnormalParser p = fmap NonGreedy . normalParser p

ngnormalParserNoFixity :: P (a SrcSpanInfo) -> Maybe [A.Fixity] -> P (NonGreedy (a SrcSpanInfo))
ngnormalParserNoFixity p = fmap NonGreedy . normalParserNoFixity p

nglistParserNoFixity :: P ([a SrcSpanInfo], [SrcSpan], SrcSpanInfo) -> Maybe [Fixity] -> P (NonGreedy (ListOf (a SrcSpanInfo)))
nglistParserNoFixity f = fmap (NonGreedy . toListOf) . normalParserNoFixity f

instance Parseable (NonGreedy (ModuleHead SrcSpanInfo)) where parser = ngnormalParserNoFixity ngparseModuleHead
instance Parseable (NonGreedy (ListOf (ModulePragma SrcSpanInfo))) where parser = nglistParserNoFixity ngparseModulePragmas

-- Type-specific functions

-- | Parse of a string, which should contain a complete Haskell module, using 'defaultParseMode'.
parseModule :: String -> ParseResult (Module SrcSpanInfo)
parseModule = parse

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode'.
parseModuleWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseModuleWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseModuleWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
parseModuleWithComments = parseWithComments

-- | Parse of a string containing a Haskell expression, using 'defaultParseMode'.
parseExp :: String -> ParseResult (Exp SrcSpanInfo)
parseExp = parse

-- | Parse of a string containing a Haskell expression, using an explicit 'ParseMode'.
parseExpWithMode :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo)
parseExpWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseExpWithComments :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo, [Comment])
parseExpWithComments = parseWithComments

-- | Parse of a string containing a Haskell pattern, using 'defaultParseMode'.
parsePat :: String -> ParseResult (Pat SrcSpanInfo)
parsePat = parse

-- | Parse of a string containing a Haskell pattern, using an explicit 'ParseMode'.
parsePatWithMode :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo)
parsePatWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parsePatWithComments :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo, [Comment])
parsePatWithComments = parseWithComments

-- | Parse of a string containing a Haskell top-level declaration, using 'defaultParseMode'.
parseDecl :: String -> ParseResult (Decl SrcSpanInfo)
parseDecl = parse

-- | Parse of a string containing a Haskell top-level declaration, using an explicit 'ParseMode'.
parseDeclWithMode :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo)
parseDeclWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseDeclWithComments :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo, [Comment])
parseDeclWithComments = parseWithComments

-- | Parse of a string containing a Haskell type, using 'defaultParseMode'.
parseType :: String -> ParseResult (Type SrcSpanInfo)
parseType = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseTypeWithMode :: ParseMode -> String -> ParseResult (Type SrcSpanInfo)
parseTypeWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseTypeWithComments :: ParseMode -> String -> ParseResult (Type SrcSpanInfo, [Comment])
parseTypeWithComments = parseWithComments

-- | Parse of a string containing a Haskell statement, using 'defaultParseMode'.
parseStmt :: String -> ParseResult (Stmt SrcSpanInfo)
parseStmt = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseStmtWithMode :: ParseMode -> String -> ParseResult (Stmt SrcSpanInfo)
parseStmtWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseStmtWithComments :: ParseMode -> String -> ParseResult (Stmt SrcSpanInfo, [Comment])
parseStmtWithComments = parseWithComments

-- Module head parsers

-- | Non-greedy parse of a string starting with a series of top-level option pragmas.
getTopPragmas :: String -> ParseResult [ModulePragma SrcSpanInfo]
getTopPragmas = fmap (unListOf . unNonGreedy) . parse

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str =
    case parse str of
        ParseOk xs ->
            case pragmasToExtensions (unListOf (unNonGreedy xs) :: [ModulePragma SrcSpanInfo]) of
                ([], es) -> Just (Nothing, es)
                ([l], es) -> Just (Just l, es)
                _ -> Nothing
        ParseFailed _ _ -> Nothing

pragmasToExtensions :: [ModulePragma l] -> ([Language], [Extension])
pragmasToExtensions = partitionEithers . concatMap getExts
  where
    getExts (LanguagePragma _ ns) = map readExt ns
    getExts _ = []
    readExt x =
        case classifyLanguage (nameStr x) of
            UnknownLanguage _ -> Right $ classifyExtension (nameStr x)
            lang -> Left lang
    nameStr (Ident _ n) = n
    nameStr (Symbol _ n) = n

-- | Instances of 'Parseable' for @NonGreedy a@ will only consume the input
--   until @a@ is fully parsed.  This means that parse errors that come later
--   in the input will be ignored.  It's also more efficient, as it's fully lazy
--   in the remainder of the input:
--
--   >>> parse (unlines ("module A where" : "main =" : repeat "blah")) :: ParseResult PragmasAndModuleHead
--   ParseOk (NonGreedy {unNonGreedy = PragmasAndModuleHead [] (ModuleName "A",Nothing,Nothing)})
--
--   (this example uses the simplified AST)
--
newtype NonGreedy a = NonGreedy { unNonGreedy :: a }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Functor NonGreedy where
    fmap f (NonGreedy x) = NonGreedy (f x)

-- | Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', then this means that there was
--   no module header.
data PragmasAndModuleName l = PragmasAndModuleName
    ([ModulePragma l], l)
    (Maybe (ModuleName l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (PragmasAndModuleName SrcSpanInfo)) where
    parser _ = do
        (ps, mn) <- ngparsePragmasAndModuleName
        return $ NonGreedy $ PragmasAndModuleName (handleSpans ps) mn

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', this means that there was no
--   module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data PragmasAndModuleHead l = PragmasAndModuleHead
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (PragmasAndModuleHead SrcSpanInfo)) where
    parser _ = do
        (ps, mn) <- ngparsePragmasAndModuleHead
        return $ NonGreedy $ PragmasAndModuleHead (handleSpans ps) mn

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module head, including top-level pragmas, module
--   name, export list, and import list. This means that if a parse error that
--   comes after the imports won't be returned.  If the 'Maybe' value is
--   'Nothing', this means that there was no module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data ModuleHeadAndImports l = ModuleHeadAndImports
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
    (Maybe ([ImportDecl l], l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (ModuleHeadAndImports SrcSpanInfo)) where
    parser _ = do
        (ps, mh, mimps) <- ngparseModuleHeadAndImports
        return $ NonGreedy $ ModuleHeadAndImports
            (handleSpans ps)
            mh
            (fmap handleSpans mimps)

handleSpans :: ([a], [SrcSpan], SrcSpanInfo) -> ([a], SrcSpanInfo)
handleSpans x = (xs, l)
  where
    ListOf l xs = toListOf x

-- | @ListOf a@ stores lists of the AST type @a@, along with a 'SrcSpanInfo',
--   in order to provide 'Parseable' instances for lists.  These instances are
--   provided when the type is used as a list in the syntax, and the same
--   delimiters are used in all of its usages. Some exceptions are made:
data ListOf a = ListOf SrcSpanInfo [a]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
#else
  deriving (Eq,Ord,Show)
#endif

unListOf :: ListOf a -> [a]
unListOf (ListOf _ xs) = xs

-- It's safe to forget about the previous SrcSpanInfo 'srcInfoPoints', because
-- I've checked all of the relevant parsers, and these particular ones
-- (presently) are all created with 'noInfoSpan' ('nIS'), '(<^^>)', or '(<++>)',
-- all of which have empty 'srcInfoPoints'. Ideally, the parsers would return
-- better types, but this works.
toListOf :: ([a], [SrcSpan], SrcSpanInfo) -> ListOf a
toListOf (xs, ss, l) = ListOf (infoSpan (srcInfoSpan l) ss) xs
