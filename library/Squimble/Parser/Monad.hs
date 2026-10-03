{-# LANGUAGE UndecidableInstances #-}

module Squimble.Parser.Monad where

import Control.Monad.Fix (MonadFix)
import Data.Kind qualified as Hask
import Text.Megaparsec (MonadParsec)

-- | The MTL stack for the parsers.
type MonadParser :: Hask.Type -> (Hask.Type -> Hask.Type) -> Hask.Constraint
class (MonadParsec e String m, MonadFail m, MonadFix m) => MonadParser e m
instance (MonadParsec e String m, MonadFail m, MonadFix m) => MonadParser e m
