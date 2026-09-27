{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Squimble.Grammar.Expression
  ( Expression (..)
  , expression
  , Operator (..)
  , ExpressionArm (..)
  , expressionArm
  ) where

import Control.Monad.Combinators.Expr qualified as Expr
import Data.Kind qualified as Hask
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prelude hiding (span)
import Squimble.Grammar.Token
import Squimble.Grammar.Monad (MonadParser)
import Squimble.Grammar.Span (Spanner (..), Span, spanning)
import Squimble.Grammar.Type (Type, type')
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal, float, signed)

-- | Expressions within the language.
type Expression :: Hask.Type
data Expression
  = ExpressionString Span Text
  | ExpressionInteger Span Integer
  | ExpressionNumber Span Double
  | ExpressionArray Span [Expression]
  | ExpressionObject Span [(Field, Expression)]
  | ExpressionVariant Span (Name "constructor") (Maybe Expression)
  | ExpressionVariable Span (Name "variable")
  | ExpressionApplication Span (Maybe (Name "module")) (Name "function") [Expression]
  | ExpressionAccess Span Expression Field
  | ExpressionIndex Span Expression Expression
  | ExpressionBinary Span Operator Expression Expression
  | ExpressionIfElse Span Expression Expression Expression
  | ExpressionCase Span Expression [ExpressionArm]
  | ExpressionLet Span (Name "variable") (Maybe Type) Expression Expression
  deriving (Eq, Ord) via Spanner Expression
  deriving stock (Generic, Show)

-- | Binary operators.
type Operator :: Hask.Type
data Operator
  = OperatorEquals Span
  | OperatorNotEquals Span
  | OperatorLessThan Span
  | OperatorGreaterThan Span
  | OperatorAtMost Span
  | OperatorAtLeast Span
  | OperatorPlus Span
  | OperatorMinus Span
  | OperatorTimes Span
  | OperatorDivide Span
  | OperatorPower Span
  | OperatorAnd Span
  | OperatorOr Span
  | OperatorCoalesce Span
  deriving (Eq, Ord) via Spanner Operator
  deriving stock (Generic, Show)

-- | Parse an 'Expression'.
expression :: MonadParser e m => m Expression
expression = fmap fst do
  let operator sigil build = do
        content <- lexeme $ spanning "operator" \span ->
          sigil *> pure (build span)

        pure \(left, before) (right, after) -> do
          let whole :: Span
              whole = before <> after

          (ExpressionBinary whole content left right, whole)

  Expr.makeExprParser term
    [ [ Expr.InfixR (operator (symbol "??") OperatorCoalesce) ]
    , [ Expr.InfixR (operator (symbol "^") OperatorPower) ]
    , [ Expr.InfixL (operator (symbol "*") OperatorTimes)
      , Expr.InfixL (operator (symbol "/") OperatorDivide)
      ]
    , [ Expr.InfixL (operator (symbol "+") OperatorPlus)
      , Expr.InfixL (operator (symbol "-") OperatorMinus)
      ]
    , [ Expr.InfixN (operator (symbol "==") OperatorEquals)
      , Expr.InfixN (operator (symbol "!=") OperatorNotEquals)
      , Expr.InfixN (operator (symbol "<=") OperatorAtMost)
      , Expr.InfixN (operator (symbol "<") OperatorLessThan)

        -- We can't conflict with the closing of a variant.
      , Expr.InfixN (operator (symbol ">=" <* lookAhead term) OperatorAtLeast)
      , Expr.InfixN (operator (symbol ">" <* lookAhead term) OperatorGreaterThan)
      ]
    , [ Expr.InfixL (operator (symbol "&&") OperatorAnd) ]
    , [ Expr.InfixL (operator (symbol "||") OperatorOr) ]
    ]

-- | Parse a non-operator term.
term :: MonadParser e m => m (Expression, Span)
term = do
  let access :: MonadParser e m => m ((Expression, Span) -> (Expression, Span))
      access = do
        key@(Field after _) <- symbol "." *> field

        pure \(inner, before) ->
          let whole = before <> after
           in (ExpressionAccess whole inner key, whole)

  let index :: MonadParser e m => m ((Expression, Span) -> (Expression, Span))
      index = spanning "index" \after -> do
        key <- symbol "[" *> expression <* symbol "]"

        pure \(inner, before) ->
          let whole = before <> after
           in (ExpressionIndex whole inner key, whole)

  base <- space *> spanning "expression" \span -> do
    content <- alternatives
      [ string span
      , number span
      , integer span
      , variant span
      , array span
      , object span
      , ifElse span
      , case' span
      , let' span
      , group
      , application span
      , variable span
      ]

    pure (content, span)

  steps <- many (choice [access, index])
  pure (foldl (\inner step -> step inner) base steps)

-- | Parse an 'ExpressionString'.
string :: MonadParser e m => Span -> m Expression
string span = do
  content <- fmap Text.pack stringLiteral
  pure (ExpressionString span content)

-- | Parse an 'ExpressionNumber'.
number :: MonadParser e m => Span -> m Expression
number span = do
  content <- try (signed (pure ()) float)
  pure (ExpressionNumber span content)

-- | Parse an 'ExpressionInteger'.
integer :: MonadParser e m => Span -> m Expression
integer span = do
  content <- signed (pure ()) decimal
  pure (ExpressionInteger span content)

-- | Parse an 'ExpressionVariant'.
variant :: MonadParser e m => Span -> m Expression
variant span = do
  key     <- symbol "<" *> name
  content <- optional (symbol ":" *> expression)
  _       <- symbol ">"

  pure (ExpressionVariant span key content)

-- | Parse an 'ExpressionArray'.
array :: MonadParser e m => Span -> m Expression
array span = do
  content <- separatedBetween "[" "," "]" expression
  pure (ExpressionArray span content)

-- | Parse an 'ExpressionObject'.
object :: MonadParser e m => Span -> m Expression
object span = do
  let entry :: MonadParser e m => m (Field, Expression)
      entry = liftA2 (,) (field <* symbol ":") expression

  content <- separatedBetween "{" "," "}" entry
  pure (ExpressionObject span content)

-- | Parse an 'ExpressionIfElse'.
ifElse :: MonadParser e m => Span -> m Expression
ifElse span = do
  condition   <- keyword "if" *> expression
  consequent  <- keyword "then" *> expression
  alternative <- keyword "else" *> expression

  pure (ExpressionIfElse span condition consequent alternative)

-- | Parse an 'ExpressionCase'.
case' :: MonadParser e m => Span -> m Expression
case' span = do
  scrutinee <- keyword "case" *> expression
  arms      <- separatedBetween "{" "," "}" expressionArm

  pure (ExpressionCase span scrutinee arms)

-- | Parse an 'ExpressionLet'.
let' :: MonadParser e m => Span -> m Expression
let' span = do
  key        <- keyword "let" *> name
  annotation <- optional (symbol ":" *> type')
  content    <- symbol "=" *> expression
  body       <- keyword "in" *> expression

  pure (ExpressionLet span key annotation content body)

-- | One arm of an 'ExpressionCase'.
type ExpressionArm :: Hask.Type
data ExpressionArm = ExpressionArm Span (Name "constructor") (Maybe Binding) Expression
  deriving (Eq, Ord) via Spanner ExpressionArm
  deriving stock (Generic, Show)

-- | Parse an 'ExpressionArm'.
expressionArm :: MonadParser e m => m ExpressionArm
expressionArm = space *> spanning "arm" \span -> do
  (key, content) <- pattern'
  result         <- symbol "->" *> expression

  pure (ExpressionArm span key content result)

-- | Parse a parenthesised 'Expression'.
group :: MonadParser e m => m Expression
group = symbol "(" *> expression <* symbol ")"

-- | Parse an 'ExpressionApplication'.
application :: MonadParser e m => Span -> m Expression
application span = do
  ns <- namespace

  key <- case ns of
    Just _  -> name
    Nothing -> try (name <* lookAhead (symbol "("))

  content <- label "an argument list" do
    separatedBetween "(" "," ")" expression

  pure (ExpressionApplication span ns key content)

-- | Parse an 'ExpressionVariable'.
variable :: MonadParser e m => Span -> m Expression
variable span = fmap (ExpressionVariable span) name
