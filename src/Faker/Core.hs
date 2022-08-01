{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import Control.Lens.TH (makeClassy)

--import Control.Monad.Free (Free)

-- import Data.Text (Text)
module Faker.Core
  ( generateAny,
    generateBetween,
    pickElement,
    pickFake,
    pickWeightedElement,
    pickWeightedFake,
    weightedCoinFlip,
    listOf,
    variedLengthListOf,
  )
where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Reader
import qualified Data.Foldable as Fold
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (filter, replicate)
import Data.Vector (Vector, fromList, length, (!))
import Faker
import Lens.Micro (Lens, Lens', lens, over, set, _2)
import Lens.Micro.Extras (view)
import System.Random (Random, StdGen (..), mkStdGen, random, randomR, split)
import Prelude hiding (length)

coinFlip :: (Fake r m) => m Bool
coinFlip = fmap (< fiftyPercent) generateAny
  where
    fiftyPercent :: Float
    fiftyPercent = 0.50

-- | Generate an arbitrary item (using `System.Random.random`). For example, because `System.Random.random` by default generates a value between 0 and 1 (for `Float`). So we could define the following:
--
-- > coinFlip :: (Fake r m) => m Bool
-- > coinFlip = fmap (< fiftyPercent) generateAny
-- >   where
-- >     fiftyPercent :: Float
-- >     fiftyPercent = 0.50
generateAny :: (Random a, Fake r m) => m a
generateAny = do
  gen <- asks (view $ prg . stdGen)
  return $ fst $ random gen

-- | Generate an item, usually a number, between the upper and lower bound (inclusive)
--
-- > userAge <- generateBetween (18, 80)
generateBetween :: (Random a, Fake r m) => (a, a) -> m a
generateBetween range = do
  gen <- asks (view $ prg . stdGen)
  return $ fst $ randomR range gen

-- | Generates a list of 'n' fakes
--
-- > fakePeople :: (Fake r m) => m [Person]
-- > fakePeople = listOf 77 fakePerson
listOf :: (Fake r m) => Int -> m a -> m [a]
listOf = replicateM

-- | Generate list of 'n' fakes, but 'n' is faked as well. For example, real-world customers do not all have the same number of orders placed:
--
-- >
-- > fakeCustomerOrders :: [Order]
-- > fakeCustomerOrders =  variedLengthListOf (generateBetween (0, 100)) fakeOrder
variedLengthListOf :: (Fake r m) => m Int -> m a -> m [a]
variedLengthListOf fakeLength a = do
  length <- fakeLength
  listOf length a

--
-- type SimpleFake = FakerT Prg Identity

-- | Picks an elements from the given vector. A Vector is used so that indexing will be a constant-time operation.
--
-- > {-# LANGUAGE OverloadedLists #-}
-- >
-- > fakeFood :: (Fake r m) => m String
-- > fakeFood = pickElement ["pizza", "salad", "kimchi", "borscht"]
pickElement :: (Fake r m) => Vector a -> m a
pickElement xs =
  if len == 0
    then error "Cannot pickElement from empty set"
    else fmap (items !) $ generateBetween (0, len - 1)
  where
    items = xs --fromList $ Fold.toList xs
    len = length items

-- | Like `pickElement`, only more general. Each element of the given vector is a fake generator.
pickFake :: (Fake r m) => Vector (m a) -> m a
pickFake = join . pickElement

-- | Like `pickElement`, only you exactly control the likelihood of picking each individual element. NOTE: Space taken is O(N) where N is the sum of the weights, not the number of unique elements.
--
-- > data Handedness = LeftHanded | RightHanded
-- >
-- > handedness :: (Fake r m) => m Handedness
-- > handedness = pickWeightedElement [(11, LeftHanded), (89, RightHanded)]
pickWeightedElement :: (Fake r m, Foldable t, Functor t) => t (Int, a) -> m a
pickWeightedElement = pickWeightedFake . fmap (over _2 pure)

-- | See docs for `pickFake` and `pickWeightedElement`.
pickWeightedFake :: (Fake r m, Foldable t) => t (Int, m a) -> m a
pickWeightedFake = pickFake . fromList . (=<<) (\(i, x) -> replicate i x) . Fold.toList

weightedCoinFlip :: (Fake r m) => Float -> m Bool
weightedCoinFlip p = fmap (< p) generateAny

--growWhile :: (Fake r m, Functor f) => (a -> m Bool) -> f a -> m (Free f a)
--growWhile pred things = undefined
type Text = String

someFunc :: IO ()
someFunc = do
  let fakePerson :: Faker Text = pickElement $ fromList ["Bob", "Alice", "Roberto", "Alisia"]
  let fakePeople :: Faker [Text] = listOf 100 fakePerson
  print $ runFaker fakePeople defaultPrg
  return ()
