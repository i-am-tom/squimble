{-# LANGUAGE UndecidableInstances #-}

-- | The constraint every parser in the grammar carries.
module Squimble.Grammar.Monad where

import Control.Monad.Fix (MonadFix)
import Text.Megaparsec (MonadParsec)

-- | The MTL stack for the parsers.
class (MonadParsec e String m, MonadFail m, MonadFix m) => MonadParser e m
instance (MonadParsec e String m, MonadFail m, MonadFix m) => MonadParser e m
