{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exports the most basic functionality for creating data from pseudo random generators, as well as classy lenses for doing so.
--
-- Working with PRGs through lenses is a lovely experience. For example, if you would like to use a different PRG within a local context, you might create one from an `Int` using `HasStdGen`.
--
-- > import Faker (Fake, HasStdGen)
-- > import System.Random (mkStdGen, StdGen)
-- > import Control.Monad.Reader (local)
-- > import Your.Favorite.Lens.Library (set)
-- >
-- > useEvilPrg :: (Fake r m, HasStdGen r) => m a -> m a
-- > useEvilPrg = local (set stdGen $ mkStdGen 666)
--
-- Or, maybe you already have a type lying around that can be trivially generated from a `StdGen`. It can lifted with `asks` + `view` and is then a first-class citizen within Faker.
--
-- >
-- > stdGen2Foo :: StdGen -> Foo
-- > stdGen2Foo = ....
-- >
-- > fakeFoo :: (Fake r m, HasPrg r) => m Foo
-- > fakeFoo = asks (stdGen2Foo . view stdGen)
module Faker
  ( Fake,
    FakerT,
    Faker,
    runFakerT,
    runFaker,
    Prg,
    defaultPrg,
    prg,
    stdGen,
    HasPrg,
    HasStdGen,
    unFake,
    initPrg,
  )
where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Reader
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (filter, replicate)
import Data.Vector (Vector, fromList, length, (!))
import Lens.Micro (Lens, Lens', lens, over, set, _2)
import Lens.Micro.Extras (view)
import System.Random (Random, StdGen (..), initStdGen, mkStdGen, random, randomR, split)
import Prelude hiding (length)

-- | A newtype around `StdGen` that implements both `HashStdGen` and `HasPrg`
newtype Prg = Prg
  { _stdGen :: StdGen
  }

-- | A `Fake` monad is exactly defined as a `MonadReader` where the environment has access to a `Prg`. This typeclass is the main constraint that will appear in nearly all functionality defined in Faker. By not using a concrete type, we separate concerns and can plug in another monad in the future while keeping full backwards compatibility.
--
-- > fakePerson :: Fake r m => m Person
-- > fakePerson = liftA2 Person fakeName fakeAge
class (MonadReader r m, HasPrg r) => Fake r m | m -> r --where
--generate :: (MonadIO io) => m a -> io a
--generate :: (MonadIO io) => Prg -> m (io a) -> io a
--generate :: (MonadIO io) => Prg ->

instance (MonadReader r m, HasPrg r) => Fake r m

useEvilPrg :: (Fake r m, HasStdGen r) => m a -> m a
useEvilPrg = local (set stdGen $ mkStdGen 666)

--instance (HasPrg r) => Fake r (Reader)
type VanillaReaderT r m a = (HasPrg r) => ReaderT r m a

-- | The default type that implements `Fake` is `FakerT`.  A vanilla reader monad could also be used, but may give repetitive results in some cases.
--
-- > type VanillaReaderT r m a = (HasPrg r) => ReaderT r m a
--
-- (NOTE: The mtl classes have not yet been defined for `FakerT`)
newtype FakerT r m a = FakerT
  {_unFake :: r -> m a}

-- | A lens that provides access to the inner `ReaderT` of the `FakerT` monad. For example, here is how `runFakerT` is defined:
--
-- > runFakerT :: FakerT r m a -> r -> m a
-- > runFakerT = runReaderT . view unFake
unFake :: Lens (FakerT r m s) (FakerT r m a) (ReaderT r m s) (ReaderT r m a)
unFake = lens (ReaderT . _unFake) $ \f r -> f {_unFake = unReader r}
  where
    unReader (ReaderT x) = x

-- | `runFaker` is used to run a `Faker` (no side-effects)
--
-- > main :: IO ()
-- > main = do
-- >  let fakeFriend :: Faker Text = pickElement $ fromList ["Bob", "Alice", "Roberto", "Alicia"]
-- >  let fakeFriends :: Faker [Text] = listOf 100 fakeFriends
-- >  print $ runFaker fakeFriends
runFaker :: FakerT Prg Identity a -> Prg -> a
runFaker (FakerT f) = runIdentity . f

-- | Like `runFaker`, only allows monadic effects.
runFakerT :: FakerT r m a -> r -> m a
runFakerT = runReaderT . view unFake

instance (Functor m) => Functor (FakerT r m) where
  fmap f (FakerT g) = FakerT $ fmap f . g

instance (Monad m, HasPrg r) => Monad (FakerT r m) where
  return = FakerT . const . return
  m >>= k = joinFaker $ fmap k $ m

instance (Monad m, HasPrg r) => Applicative (FakerT r m) where
  pure = FakerT . const . pure
  f <*> a = f >>= (\xf -> a >>= (\xa -> return (xf xa)))

instance (Monad m, HasPrg r) => MonadReader r (FakerT r m) where
  --local r = over unFake (local r)
  reader = FakerT . fmap return
  ask = FakerT $ return . id

-- | a private fn just used to implement Monad
joinFaker :: (Monad m, HasPrg r) => FakerT r m (FakerT r m a) -> FakerT r m a
joinFaker (FakerT f) = FakerT g
  where
    g r = join $ fmap (\f -> f prgA) $ (fmap (fmap _unFake) f) prgB
      where
        (genA, genB) = split $ view (prg . stdGen) r
        (prgA, prgB) = (set (prg . stdGen) genA r, set (prg . stdGen) genB r)

defaultPrg = Prg $ mkStdGen 314156

class HasStdGen x where
  stdGen :: Lens' x StdGen

instance HasStdGen Prg where
  stdGen = lens _stdGen (\s x -> s {_stdGen = x})

instance HasStdGen StdGen where
  stdGen = id

class HasPrg x where
  prg :: Lens' x Prg

instance HasPrg Prg where
  prg = id

{-
class HasIntSeed x where
  intSeed :: Lens' x Int

instance HasIntSeed StdGen where
  intSeed =
-}
modifyStdGen x = x

-- | The Faker analog to `initStdGen`.
initPrg :: MonadIO m => m Prg
initPrg = Prg <$> initStdGen

fakeEnvTest :: (HasPrg r, Fake r m) => m ()
fakeEnvTest = do
  env <- ask
  let newEnv = over (prg . stdGen) modifyStdGen env
  return ()

-- | Like `FakerT`, but sans monadic effects.
type Faker = FakerT Prg Identity
