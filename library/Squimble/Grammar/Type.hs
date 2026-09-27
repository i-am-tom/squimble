{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Grammar.Type
  ( Type (..)
  , type'
  ) where

import Data.Kind qualified as Hask
import GHC.Generics (Generic)
import Prelude hiding (span)
import Squimble.Grammar.Token
import Squimble.Grammar.Monad (MonadParser)
import Squimble.Grammar.Span (Spanner (..), Span, spanning)
import Text.Megaparsec

-- | A type used in the program.
type Type :: Hask.Type
data Type
  = TypeName Span (Maybe (Name "module")) (Name "type")
  | TypeArray Span Type
  | TypeDictionary Span Type
  | TypeStruct Span [(Field, Type)]
  | TypeVariant Span [(Name "constructor", Type)]
  | TypeOptional Span Type
  deriving (Eq, Ord) via Spanner Type
  deriving stock (Generic, Show)

-- | Parse a 'Type'. Each trailing @?@ wraps it in a 'TypeOptional'.
type' :: MonadParser e m => m Type
type' = lexeme $ spanning "type" \whole -> do
  content <- spanning "type" \span -> alternatives
    [ array span
    , dictionary span
    , struct span
    , variant span
    , typename span
    ]

  questions <- many (symbol "?")

  let go :: Type -> () -> Type
      go inner () = TypeOptional whole inner

  pure (foldl go content questions)

-- | Parse a 'TypeArray'.
array :: MonadParser e m => Span -> m Type
array span = keyword "array" *> do
  content <- angled type'
  pure (TypeArray span content)

-- | Parse a 'TypeDictionary'.
dictionary :: MonadParser e m => Span -> m Type
dictionary span = keyword "map" *> do
  content <- angled type'
  pure (TypeDictionary span content)

-- | Parse a 'TypeStruct'.
struct :: MonadParser e m => Span -> m Type
struct span = do
  let entry :: MonadParser e m => m (Field, Type)
      entry = liftA2 (,) (field <* symbol ":") type'

  content <- separatedBetween "{" "," "}" entry
  pure (TypeStruct span content)

-- | Parse a 'TypeVariant'.
variant :: MonadParser e m => Span -> m Type
variant span = do
  content <- separatedBetween "<" "," ">" constructor
  pure (TypeVariant span content)

-- | Parse a 'TypeVariant' constructor. An empty struct payload can be emitted.
constructor :: MonadParser e m => m (Name "constructor", Type)
constructor = do
  let unit :: MonadParser e m => m Type
      unit = spanning "type" \span -> pure (TypeStruct span [])

  key     <- name
  content <- alternatives [symbol ":" *> type', unit]

  pure (key, content)

-- | Parse a 'TypeName'.
typename :: MonadParser e m => Span -> m Type
typename span = do
  ns  <- namespace
  key <- name

  optional (lookAhead (symbol "::")) >>= \case
    Just () -> fail "namespaces cannot be nested"
    Nothing -> pure (TypeName span ns key)
