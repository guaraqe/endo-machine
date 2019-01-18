{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module EndoMachine
  ( HasWhole (..)
  , Whole (..)
  , HasContext
  , endo
  , endoR
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Capability.Reader
import Capability.State
import Data.Functor.Compose

--------------------------------------------------------------------------------

class HasWhole a where
  type Tag a -- for capability
  type Parameter a -- parameters that are common to elements in container
  type Container a -- monomorphic container composed of `a`

data Whole a = Whole
  { _parameter :: !(Parameter a)
  , _container :: !(Container a)
  }

--------------------------------------------------------------------------------

-- | Apply an endomorphism on @Whole a@ by using a parametric endomorphism on
-- @Container a@.
endo :: (Parameter a -> Container a -> Container a) -> Whole a -> Whole a
endo f (Whole p c) = Whole p (f p c)

--------------------------------------------------------------------------------

type HasContext a m = HasReader (Tag a) (Parameter a) m

newtype Function a b = ParFunction { runFunction :: a -> b }
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasReader tag a) via MonadReader ((->) a)

-- | Apply an endomorphism on @Whole a@ by using an endomorphism on @Container a@.
-- which has @Parameter a@ in the context.
endoR ::
  (forall m. HasContext a m => Container a -> m (Container a)) ->
  Whole a -> Whole a
endoR f (Whole p c) = Whole p (runFunction (f c) p)

--------------------------------------------------------------------------------

type CanChange a m = HasState (Tag a) (Container a) m

newtype RState r s a = RState { runRState :: ReaderT r (State s) a }
  deriving (Functor, Applicative, Monad)
  deriving (HasReader tag r) via MonadReader (ReaderT r (State s))
  deriving (HasState tag s) via MonadState (ReaderT r (State s))

-- | Apply an endomorphism on @Whole a@ by using a stateful operation on
-- @Container a@ which has @Parameter a@ in the context.
endoS ::
  (forall m. (HasContext a m, CanChange a m) => m ()) ->
  Whole a -> Whole a
endoS f (Whole p c) = Whole p (execState (runReaderT (runRState f) p) c)
