{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Parser.Interface where

import Data.Kind qualified as Hask
import GHC.Generics (Generic)
import Prelude hiding (span)
import Squimble.Parser.Token
import Squimble.Parser.Type
import Squimble.Parser.Monad (MonadParser)
import Squimble.Parser.Span (Spanner (..), Span, spanning)
import Text.Megaparsec

-- | The inputs to an interface.
type Inputs :: Hask.Type
data Inputs = Inputs Span [Input]
  deriving (Eq, Ord) via Spanner Inputs
  deriving stock (Generic, Show)

-- | Parse 'Inputs'.
inputs :: MonadParser e m => m Inputs
inputs = lexeme $ spanning "inputs" \span -> do
  entries <- option [] do
    _ <- keyword "input"
    separatedBetween "{" "," "}" input

  pure (Inputs span entries)

-- | An input to an interface: what it is, and when it can be supplied.
type Input :: Hask.Type
data Input = Input Span Field Type (Name "stage") IsRequired
  deriving (Eq, Ord) via Spanner Input
  deriving stock (Generic, Show)

-- | Parse an 'Input'.
input :: MonadParser e m => m Input
input = space *> spanning "input" \span -> do
  key      <- field <* symbol ":"
  content  <- type'
  stage    <- symbol "@" *> name
  required <- isRequired

  pure (Input span key content stage required)

-- | The outputs to an interface.
type Outputs :: Hask.Type
data Outputs = Outputs Span [Output]
  deriving (Eq, Ord) via Spanner Outputs
  deriving stock (Generic, Show)

-- | Parse 'Outputs'.
outputs :: MonadParser e m => m Outputs
outputs = lexeme $ spanning "outputs" \span -> do
  entries <- option [] do
    _ <- keyword "output"
    separatedBetween "{" "," "}" output

  pure (Outputs span entries)

-- | An output from an interface: what it is, and when it can be retrieved.
type Output :: Hask.Type
data Output = Output Span Field Type (Name "stage")
  deriving (Eq, Ord) via Spanner Output
  deriving stock (Generic, Show)

-- | Parse an 'Output'.
output :: MonadParser e m => m Output
output = space *> spanning "output" \span -> do
  key     <- field <* symbol ":"
  content <- type'
  stage   <- symbol "@" *> name

  pure (Output span key content stage)

-- | Is this thing required or not?
type IsRequired :: Hask.Type
data IsRequired = IsRequired Span | Isn'tRequired Span
  deriving (Eq, Ord) via Spanner IsRequired
  deriving stock (Generic, Show)

-- | Parse whether it 'IsRequired'.
isRequired :: MonadParser e m => m IsRequired
isRequired = lexeme $ spanning "requirement" \span -> do
  present <- optional (keyword "required")

  pure case present of
    Just () -> IsRequired span
    Nothing -> Isn'tRequired span
