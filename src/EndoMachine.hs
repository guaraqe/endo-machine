{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module EndoMachine
  ( IsWhole (..)
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

class IsWhole a where
  type Tag a -- for capability
  type Parameters a -- parameters that are common to elements in container
  type Subparts a -- monomorphic container composed of `a`
  getParameters :: a -> Parameters a
  getSubparts :: a -> Subparts a
  mkWhole :: Parameters a -> Subparts a -> a

--------------------------------------------------------------------------------

-- | Apply an endomorphism on @Whole a@ by using a parametric endomorphism on
-- @Subparts a@.
endo :: IsWhole a => (Parameters a -> Subparts a -> Subparts a) -> a -> a
endo f w =
  let
    p = getParameters w
    s = getSubparts w
  in
    mkWhole p (f p s)

--------------------------------------------------------------------------------

type HasContext a m = HasReader (Tag a) (Parameters a) m

newtype Function a b = Function { runFunction :: a -> b }
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasReader tag a) via MonadReader ((->) a)

-- | Apply an endomorphism on @Whole a@ by using an endomorphism on @Subparts a@.
-- which has @Parameters a@ in the context.
endoR ::
  IsWhole a =>
  (forall m. HasContext a m => Subparts a -> m (Subparts a)) ->
  a -> a
endoR f w =
  let
    p = getParameters w
    s = getSubparts w
  in
    mkWhole p (runFunction (f s) p)

--------------------------------------------------------------------------------

type CanChange a m = HasState (Tag a) (Subparts a) m

newtype RState r s a = RState { runRState :: ReaderT r (State s) a }
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasReader tag r) via MonadReader (ReaderT r (State s))
  deriving (HasState tag s) via MonadState (ReaderT r (State s))

-- | Apply an endomorphism on @Whole a@ by using a stateful operation on
-- @Subparts a@ which has @Parameters a@ in the context.
endoS ::
  IsWhole a =>
  (forall m. (HasContext a m, CanChange a m) => m ()) ->
  a -> a
endoS f w =
  let
    p = getParameters w
    s = getSubparts w
  in
    mkWhole p (execState (runReaderT (runRState f) p) s)
