{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for handling source spans.
module Squimble.Grammar.Span where

import Control.Monad.Fix (MonadFix (..))
import Data.Function (on)
import Data.Kind (Constraint, Type)
import GHC.Generics
import Prelude hiding (span)
import Squimble.Grammar.Monad (MonadParser)
import Text.Megaparsec (SourcePos, getSourcePos, label)

-- | The start and end of a source span in a program.
type Span :: Type
data Span = Span { _spanStart :: SourcePos, _spanEnd :: SourcePos }
  deriving stock (Eq, Ord, Show)

instance Semigroup Span where
  (<>) :: Span -> Span -> Span
  (<>) (Span before _) (Span _ after)
    = Span before after

-- | Parse something with access to its 'Span' thunk.
--
-- The end of a span isn't known at parse-time, so we provide access to the
-- thunk that can be figured out later on.
spanning :: MonadParser e m => String -> (Span -> m x) -> m x
spanning text k = label text do
  (output, _) <- mfix \pass -> do
    _spanStart <- getSourcePos
    output     <- k (snd pass)
    _spanEnd   <- getSourcePos

    pure (output, Span {..})
  
  pure output

-- | Spans aren't really important when for 'Eq' and 'Ord' in our AST; they're
-- mostly just going to slow them down. The 'Spanner' newtype ignores any 'Span'
-- in the structure for '(==)' and 'compare'.
type Spanner :: Type -> Type
newtype Spanner x = Spanner x
  deriving newtype (Generic, Show)

---

type GEq :: (Type -> Type) -> Constraint
class GEq rep where
  geq :: rep v -> rep v -> Bool

instance (Generic x, GEq (Rep x)) => Eq (Spanner x) where
  (==) :: (Generic x, GEq (Rep x)) => Spanner x -> Spanner x -> Bool
  (==) = geq `on` from

instance GEq x => GEq (M1 i m x) where
  geq :: GEq x => M1 i m x v -> M1 i m x v -> Bool
  geq = geq `on` unM1

instance GEq V1 where
  geq :: V1 v -> V1 v -> Bool
  geq = \case

instance (GEq x, GEq y) => GEq (x :+: y) where
  geq :: (GEq x, GEq y) => (x :+: y) v -> (x :+: y) v -> Bool
  geq (L1 x) (L1 y) = geq x y
  geq (R1 x) (R1 y) = geq x y
  geq  _      _     = False

instance GEq U1 where
  geq :: U1 v -> U1 v -> Bool
  geq = (==)

instance (GEq x, GEq y) => GEq (x :*: y) where
  geq :: (GEq x, GEq y) => (x :*: y) v -> (x :*: y) v -> Bool
  geq (x :*: y) (x' :*: y') = geq x x' && geq y y'

instance {-# OVERLAPPING #-} GEq (K1 r Span) where
  geq :: K1 r Span v -> K1 r Span v -> Bool
  geq _ _ = True

instance {-# OVERLAPPABLE #-} Eq x => GEq (K1 r x) where
  geq :: K1 r x v -> K1 r x v -> Bool
  geq = (==) `on` unK1

---

type GOrd :: (Type -> Type) -> Constraint
class GEq rep => GOrd rep where
  gcompare :: rep v -> rep v -> Ordering

instance GOrd x => GOrd (M1 i m x) where
  gcompare :: GOrd x => M1 i m x v -> M1 i m x v -> Ordering
  gcompare = gcompare `on` unM1

instance GOrd V1 where
  gcompare :: V1 v -> V1 v -> Ordering
  gcompare = \case

instance (GOrd x, GOrd y) => GOrd (x :+: y) where
  gcompare :: (GOrd x, GOrd y) => (x :+: y) v -> (x :+: y) v -> Ordering
  gcompare (L1 x) (L1 y) = gcompare x y
  gcompare (L1 _) (R1 _) = LT
  gcompare (R1 x) (R1 y) = gcompare x y
  gcompare (R1 _) (L1 _) = GT

instance (GOrd x, GOrd y) => GOrd (x :*: y) where
  gcompare :: (GOrd x, GOrd y) => (x :*: y) v -> (x :*: y) v -> Ordering
  gcompare (x :*: y) (x' :*: y') = gcompare x x' <> gcompare y y'

instance {-# OVERLAPPING #-} GOrd (K1 r Span) where
  gcompare :: K1 r Span v -> K1 r Span v -> Ordering
  gcompare _ _ = EQ

instance {-# OVERLAPPABLE #-} Ord x => GOrd (K1 r x) where
  gcompare :: K1 r x v -> K1 r x v -> Ordering
  gcompare = compare `on` unK1

instance GOrd U1 where
  gcompare :: U1 v -> U1 v -> Ordering
  gcompare = compare

instance (Generic x, GOrd (Rep x)) => Ord (Spanner x) where
  compare :: (Generic x, GOrd (Rep x)) => Spanner x -> Spanner x -> Ordering
  compare = gcompare `on` from