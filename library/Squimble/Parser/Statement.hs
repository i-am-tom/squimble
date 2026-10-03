{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Parser.Statement
  ( Block (..)
  , Statement (..)
  , StatementArm (..)
  , Argument (..)
  , Arguments (..)

  , block
  , statement
  , statementArm
  , argument
  , arguments
  ) where

import Data.Kind qualified as Hask
import GHC.Generics (Generic)
import Prelude hiding (span)
import Squimble.Parser.Expression
import Squimble.Parser.Token
import Squimble.Parser.Monad (MonadParser)
import Squimble.Parser.Span (Spanner (..), Span, spanning)
import Squimble.Parser.Type (Type, type')
import Text.Megaparsec

-- | An ordered set of statements.
type Block :: Hask.Type
data Block = Block Span [Statement]
  deriving (Eq, Ord) via Spanner Block
  deriving stock (Generic, Show)

-- | Parse a 'Block'.
block :: MonadParser e m => m Block
block = space *> spanning "block" \span -> do
  entries <- separatedBetween "{" ";" "}" statement
  pure (Block span entries)

-- | Statements in the language.
type Statement :: Hask.Type
data Statement
  = StatementLet Span (Name "variable") (Maybe Type) Expression
  | StatementRegister Span Binding (Maybe (Name "module")) (Name "interface") (Name "registration") Arguments
  | StatementIfElse Span Expression Block (Maybe Block)
  | StatementForeach Span Binding (Name "variable") (Maybe (Name "variable")) Expression (Name "family") Block
  | StatementCase Span Expression [StatementArm]
  | StatementOutput Span Field Expression
  deriving (Eq, Ord) via Spanner Statement
  deriving stock (Generic, Show)

-- | Parse a 'Statement'.
statement :: MonadParser e m => m Statement
statement = space *> spanning "statement" \span -> do
  alternatives
    [ let' span
    , output span
    , ifElse span
    , case' span
    , bound span
    ]

-- | Parse a 'StatementLet'.
let' :: MonadParser e m => Span -> m Statement
let' span = do
  key        <- keyword "let" *> name
  annotation <- optional (symbol ":" *> type')
  value      <- symbol "=" *> expression

  pure (StatementLet span key annotation value)

-- | Parse a 'StatementIfElse'.
ifElse :: MonadParser e m => Span -> m Statement
ifElse span = do
  condition   <- keyword "if" *> expression
  consequent  <- keyword "then" *> block
  alternative <- optional (keyword "else" *> block)

  pure (StatementIfElse span condition consequent alternative)

-- | Parse a 'StatementOutput'.
output :: MonadParser e m => Span -> m Statement
output span = do
  key   <- keyword "output" *> field
  value <- symbol "=" *> expression

  pure (StatementOutput span key value)

-- | Parse statements involving a binding.
bound :: MonadParser e m => Span -> m Statement
bound span = do
  key <- binding <* symbol "<-"

  alternatives [register span key, foreach span key]

-- | Parse a 'StatementCase'.
case' :: MonadParser e m => Span -> m Statement
case' span = do
  scrutinee <- keyword "case" *> expression
  arms      <- separatedBetween "{" "," "}" statementArm

  pure (StatementCase span scrutinee arms)

-- | One arm of a 'StatementCase'.
type StatementArm :: Hask.Type
data StatementArm = StatementArm Span (Name "constructor") (Maybe Binding) Block
  deriving (Eq, Ord) via Spanner StatementArm
  deriving stock (Generic, Show)

-- | Parse a 'StatementArm'.
statementArm :: MonadParser e m => m StatementArm
statementArm = space *> spanning "arm" \span -> do
  (key, content) <- pattern'
  result         <- symbol "->" *> block

  pure (StatementArm span key content result)

-- | Parse a 'StatementRegister'.
register :: MonadParser e m => Span -> Binding -> m Statement
register span key = do
  ns           <- keyword "register" *> namespace
  interface    <- name
  registration <- keyword "as" *> name
  content      <- arguments

  pure (StatementRegister span key ns interface registration content)

-- | Parse a 'StatementForeach'.
foreach :: MonadParser e m => Span -> Binding -> m Statement
foreach span key = do
  index  <- keyword "foreach" *> name
  value  <- optional (symbol "," *> name)
  source <- keyword "in" *> expression
  family <- keyword "as" *> name
  body   <- block

  pure (StatementForeach span key index value source family body)

-- | Arguments passed to a component.
type Arguments :: Hask.Type
data Arguments = Arguments Span [Argument]
  deriving (Eq, Ord) via Spanner Arguments
  deriving stock (Generic, Show)

-- | Parse 'Arguments'.
arguments :: MonadParser e m => m Arguments
arguments = space *> spanning "arguments" \span -> do
  entries <- option [] (separatedBetween "{" "," "}" argument)
  pure (Arguments span entries)

-- | A vaule to be passed to a component during registration.
type Argument :: Hask.Type
data Argument = Argument Span Field Expression
  deriving (Eq, Ord) via Spanner Argument
  deriving stock (Generic, Show)

-- | Parse an 'Argument'.
argument :: MonadParser e m => m Argument
argument = space *> spanning "argument" \span -> do
  key   <- field
  _     <- symbol "="
  value <- expression

  pure (Argument span key value)
