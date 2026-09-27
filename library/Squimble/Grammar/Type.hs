{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Grammar.Type
  ( Type (..)
  , type'
  ) where

import Control.Monad (foldM)
import Data.Kind qualified as Hask
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (span)
import Squimble.Grammar.Monad (MonadParser)
import Squimble.Grammar.Span (Spanner (..), Span, spanning)
import Squimble.Grammar.Token
import Text.Megaparsec

-- | A type used in the program.
type Type :: Hask.Type
data Type
  = TypeName Span (Maybe (Name "module")) (Name "type")
  | TypeArray Span Type
  | TypeDictionary Span Type
  | TypeStruct Span (Map Field Type)
  | TypeVariant Span (Map (Name "constructor") Type)
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
  let entry :: MonadParser e m => m (Int, Field, Type)
      entry = do
        offset  <- space *> getOffset
        key     <- field <* symbol ":"
        content <- type'

        pure (offset, key, content)

  entries <- separatedBetween "{" "," "}" entry
  content <- unique entries "field" \(Field _ key) -> key

  pure (TypeStruct span content)

-- | Parse a 'TypeVariant'.
variant :: MonadParser e m => Span -> m Type
variant span = do
  entries <- separatedBetween "<" "," ">" constructor
  content <- unique entries "constructor" \(Name _ key) -> key

  pure (TypeVariant span content)

-- | Parse a 'TypeVariant' constructor. An empty struct payload can be emitted.
constructor :: MonadParser e m => m (Int, Name "constructor", Type)
constructor = do
  let unit :: MonadParser e m => m Type
      unit = spanning "type" \span -> pure (TypeStruct span Map.empty)

  offset  <- space *> getOffset
  key     <- name
  content <- alternatives [symbol ":" *> type', unit]

  pure (offset, key, content)

-- | Collect entries into a 'Map', failing at the first repeated key.
unique :: (MonadParser e m, Ord k) => [(Int, k, v)] -> String -> (k -> Text) -> m (Map k v)
unique entries entity describe = foldM insert Map.empty entries
  where
    insert result (offset, key, content) = case Map.lookup key result of
      Nothing -> pure (Map.insert key content result)
      Just __ -> region (setErrorOffset offset) (fail message)
        where
          message :: String
          message = "duplicate " ++ entity ++ " " ++ show (describe key)

-- | Parse a 'TypeName'.
typename :: MonadParser e m => Span -> m Type
typename span = do
  ns  <- namespace
  key <- name

  optional (lookAhead (symbol "::")) >>= \case
    Just () -> fail "namespaces cannot be nested"
    Nothing -> pure (TypeName span ns key)
