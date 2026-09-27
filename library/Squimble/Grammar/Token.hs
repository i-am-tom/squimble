{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Grammar.Token where

import Control.Monad (when)
import Data.Functor (void)
import Data.Kind qualified as Hask
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prelude hiding (FilePath, span)
import Prelude qualified as P
import Squimble.Grammar.Monad (MonadParser)
import Squimble.Grammar.Span (Spanner (..), Span, spanning)
import Text.Megaparsec
import Text.Megaparsec.Char (char, alphaNumChar, lowerChar, space1)
import Text.Megaparsec.Char.Lexer (charLiteral, skipLineComment)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | A record field name, input/output field name, etc.
type Field :: Hask.Type
data Field = Field Span Text
  deriving (Eq, Ord) via Spanner Field
  deriving stock (Generic, Show)

-- | Parse a 'Field' in the program.
field :: MonadParser e m => m Field
field = lexeme $ spanning "field" \span -> do
  content <- identifier <|> stringLiteral
  pure (Field span (Text.pack content))

-- | A name, tagged with whatever sort of thing it names. Unlike fields, they
-- can't be quoted to include characters outside of @[a-z][\w\d_]+@.
type Name :: k -> Hask.Type
data Name x = Name Span Text
  deriving (Eq, Ord) via Spanner (Name x)
  deriving stock (Generic, Show)

-- | Parse a name.
name :: MonadParser e m => m (Name x)
name = lexeme $ spanning "name" \span -> do
  content <- fmap Text.pack identifier
  pure (Name span content)

-- | The name something is bound to, or @_@ to say deliberately that it is
-- bound to nothing.
type Binding :: Hask.Type
data Binding = Bound Span (Name "variable") | Discarded Span
  deriving (Eq, Ord) via Spanner Binding
  deriving stock (Generic, Show)

-- | Parse a 'Binding'.
binding :: MonadParser e m => m Binding
binding = lexeme $ spanning "binding" \span ->
  alternatives
    [ fmap (const (Discarded span)) (keyword "_")
    , fmap (Bound span) name
    ]

-- | The constructor for a case arm, and a 'Binding' for its payload.
pattern' :: MonadParser e m => m (Name "constructor", Maybe Binding)
pattern' = alternatives [angled bracketed, bare]
  where
    bracketed :: MonadParser e m => m (Name "constructor", Maybe Binding)
    bracketed = do
      key     <- name
      content <- optional (symbol ":" *> binding)

      pure (key, content)

    bare :: MonadParser e m => m (Name "constructor", Maybe Binding)
    bare = fmap (, Nothing) name

-- | A path to another module somewhere.
type FilePath :: Hask.Type
data FilePath = FilePath Span P.FilePath
  deriving (Eq, Ord) via Spanner FilePath
  deriving stock (Generic, Show)

-- | Parse a file path.
filePath :: MonadParser e m => m FilePath
filePath = lexeme $ spanning "path" \span -> do
  content <- stringLiteral
  pure (FilePath span content)

-- | All identifiers in the program parse the same.
identifier :: MonadParser e m => m String
identifier = do
  content <- liftA2 (:) lowerChar do
    many (alphaNumChar <|> char '_')

  when (content `elem` keywords) do
    fail (show content <> " is a keyword")

  pure content

-- | Keywords that cannot be used as names.
keywords :: [String]
keywords =
  [ "alias"
  , "array"
  , "as"
  , "else"
  , "foreach"
  , "from"
  , "function"
  , "case"
  , "if"
  , "implementation"
  , "in"
  , "import"
  , "input"
  , "interface"
  , "let"
  , "map"
  , "output"
  , "register"
  , "required"
  , "stages"
  , "then"
  ]

-- | Parse a namespace.
namespace :: MonadParser e m => m (Maybe (Name x))
namespace = optional (try (name <* symbol "::"))

-- | A quote-wrapped string literal.
stringLiteral :: MonadParser e m => m String
stringLiteral = char '"' *> manyTill charLiteral (char '"')

-- | A program lexeme.
lexeme :: MonadParser e m => m x -> m x
lexeme x = try (space *> x)

-- | Spaces within the program.
space :: MonadParser e m => m ()
space = Lexer.space space1 (skipLineComment "#") empty

-- | A symbol non-keyword symbol.
symbol :: MonadParser e m => String -> m ()
symbol = void . lexeme . chunk

-- | A keyword in the program.
keyword :: MonadParser e m => String -> m ()
keyword x = lexeme (chunk x *> notFollowedBy rest)
  where rest = alphaNumChar <|> char '_'

-- | Something wrapped in braces.
braced :: MonadParser e m => m x -> m x
braced = between (symbol "{") (symbol "}")

-- | Something wrapped in angle brackets.
angled :: MonadParser e m => m x -> m x
angled = between (symbol "<") (symbol ">")

-- | A 'choice' whose whitespace is consumed once, up front.
alternatives :: MonadParser e m => [m x] -> m x
alternatives xs = space *> choice xs

-- | Comma-separated entries between a pair of delimiters, with a trailing
-- separator allowed: @{ a = 1, b = 2, }@.
separatedBetween :: MonadParser e m => String -> String -> String -> m x -> m [x]
separatedBetween open separator close entry = symbol open *> manyTill (entry <* separated) (symbol close)
  where
    separated :: MonadParser e m => m ()
    separated = alternatives [symbol separator, lookAhead (symbol close)]