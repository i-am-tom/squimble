{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Parser.Declaration
  ( Module (..)
  , Declaration (..)
  , Parameter (..)

  , module'
  , declaration
  , parameter
  ) where

import Data.Kind qualified as Hask
import GHC.Generics (Generic)
import Prelude hiding (FilePath, span)
import Squimble.Parser.Expression
import Squimble.Parser.Interface
import Squimble.Parser.Statement
import Squimble.Parser.Token
import Squimble.Parser.Type
import Squimble.Parser.Monad (MonadParser)
import Squimble.Parser.Span (Spanner (..), Span, spanning)
import Text.Megaparsec

-- | A full module.
type Module :: Hask.Type
data Module = Module Span [Declaration]
  deriving (Eq, Ord) via Spanner Module
  deriving stock (Generic, Show)

-- | Parse a 'Module'.
module' :: MonadParser e m => m Module
module' = space *> spanning "module" \span -> do
  entries <- manyTill declaration $ try (space *> eof)
  pure (Module span entries)

-- | A top-level declaration in the file.
type Declaration :: Hask.Type
data Declaration
  = DeclarationImport Span (Name "module") FilePath
  | DeclarationInterface Span (Name "interface") [Name "stage"] Inputs Outputs
  | DeclarationImplementation Span (Maybe (Name "module")) (Name "interface") Block
  | DeclarationAlias Span (Name "type") Type
  | DeclarationFunction Span (Name "function") [Parameter] Type Expression
  deriving (Eq, Ord) via Spanner Declaration
  deriving stock (Generic, Show)

-- | Parse a 'Declaration'.
declaration :: MonadParser e m => m Declaration
declaration = space *> spanning "declaration" \span -> do
  alternatives
    [ import' span
    , interface span
    , implementation span
    , alias span
    , function span
    ]

-- | Parse a 'DeclarationImport'.
import' :: MonadParser e m => Span -> m Declaration
import' span = do
  key  <- keyword "import" *> name
  path <- keyword "from" *> filePath

  pure (DeclarationImport span key path)

-- | Parse a 'DeclarationInterface'.
interface :: MonadParser e m => Span -> m Declaration
interface span = do
  key <- keyword "interface" *> name

  braced do
    stages <- option [] do
      _ <- keyword "stages"
      separatedBetween "{" "," "}" name

    accepts  <- inputs
    produces <- outputs

    pure (DeclarationInterface span key stages accepts produces)

-- | Parse a 'DeclarationImplementation'.
implementation :: MonadParser e m => Span -> m Declaration
implementation span = do
  ns   <- keyword "implementation" *> namespace
  key  <- name
  body <- block

  pure (DeclarationImplementation span ns key body)

-- | Parse a 'DeclarationAlias'.
alias :: MonadParser e m => Span -> m Declaration
alias span = do
  key     <- keyword "alias" *> name
  content <- symbol "=" *> type'

  pure (DeclarationAlias span key content)

-- | Parse a 'DeclarationFunction'.
function :: MonadParser e m => Span -> m Declaration
function span = do
  key        <- keyword "function" *> name
  parameters <- separatedBetween "(" "," ")" parameter
  returns    <- symbol "->" *> type'
  body       <- braced expression

  pure (DeclarationFunction span key parameters returns body)

-- | One parameter of a 'DeclarationFunction'.
type Parameter :: Hask.Type
data Parameter = Parameter Span (Name "parameter") Type
  deriving (Eq, Ord) via Spanner Parameter
  deriving stock (Generic, Show)

-- | Parse a 'Parameter'.
parameter :: MonadParser e m => m Parameter
parameter = space *> spanning "parameter" \span -> do
  key     <- name <* symbol ":"
  content <- type'

  pure (Parameter span key content)
