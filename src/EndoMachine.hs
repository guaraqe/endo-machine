{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module EndoMachine
  ( Whole (..)
  , Reads
  , Changes
  , endo
  , endoM
  , endoR
  , endoS
  ) where

import Capability.Reader
import Capability.State
import Control.Lens
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict (State, execState)

--------------------------------------------------------------------------------

class Whole a where
  type Parameters a
  parameters :: Lens' a (Parameters a)
  type Component a
  components :: Traversal' a (Component a)

--------------------------------------------------------------------------------

-- | Apply an endomorphism on @Whole a@ by using a parametric endomorphism on
-- @Component a@.
endo ::
  Whole a =>
  (Parameters a -> Component a -> Component a) ->
  a -> a
endo f a =
  let
    p = view parameters a
  in
    over components (f p) a

endoM ::
  (Whole a, Applicative m) =>
  (Parameters a -> Component a -> m (Component a)) ->
  a -> m a
endoM f a =
  let
    p = view parameters a
  in
    components (f p) a

--------------------------------------------------------------------------------

newtype Function a b = Function { unFunction :: a -> b }
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasReader tag a) via MonadReader ((->) a)

runFunction :: (b -> Function a b) -> a -> b -> b
runFunction f a b = unFunction (f b) a

-- | A constraint synonym for the Reader, which uses the Whole type as tag.
type Reads a m = HasReader a a m

-- | Apply an endomorphism on @Whole a@ by using an endomorphism on @Component a@.
-- which has @Parameters a@ in the context.
endoR ::
  Whole a =>
  (forall m. Reads (Parameters a) m => Component a -> m (Component a)) ->
  a -> a
endoR f a =
  let
    p = view parameters a
  in
    over components (runFunction f p) a

--------------------------------------------------------------------------------

newtype RState r s a = RState { unRState :: ReaderT r (State s) a }
  deriving newtype (Functor, Applicative, Monad)
  deriving (HasReader tag r) via MonadReader (ReaderT r (State s))
  deriving (HasState tag s) via MonadState (ReaderT r (State s))

runRState :: RState a b () -> a -> b -> b
runRState f a b = execState (runReaderT (unRState f) a) b

type Changes a m = HasState a a m

-- | Apply an endomorphism on @Whole a@ by using a stateful operation on
-- @Component a@ which has @Parameters a@ in the context.
endoS ::
  Whole a =>
  (forall m. (Reads (Parameters a) m, Changes (Component a) m) => m ()) ->
  a -> a
endoS f a =
  let
    p = view parameters a
  in
    over components (runRState f p) a
