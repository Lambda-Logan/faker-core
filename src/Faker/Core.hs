{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    nDigitNumber,
    nDigitNumberBase,
    nDigitStr,
    coinFlip,
  )
where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as Fold
import Data.String (IsString (..))
import Data.Vector (Vector, fromList, length, (!))
import Faker
import Lens.Micro ( over, _2)
import Lens.Micro.Extras (view)
import System.Random (Random, random, randomR)
import Prelude hiding (length)

coinFlip :: (Applicative m, Fake m) => m Bool
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
generateAny :: (Random a, Applicative m, Fake m) => m a
generateAny = fst . random . view stdGen <$> fask

-- | Generate an item, usually a number, between the upper and lower bound (inclusive)
--
-- > userAge <- generateBetween (18, 80)
generateBetween :: (Applicative m, Random a, Fake m) => (a, a) -> m a
generateBetween range = fst . randomR range . view stdGen <$> fask

-- | Generates a list of 'n' fakes
--
-- > fakePeople :: (Fake r m) => m [Person]
-- > fakePeople = listOf 77 fakePerson
listOf :: (Applicative m) => Int -> m a -> m [a]
listOf = replicateM

-- | Generate list of 'n' fakes, but 'n' is faked as well. For example, real-world customers do not all have the same number of orders placed:
--
-- >
-- > fakeCustomerOrders :: [Order]
-- > fakeCustomerOrders =  variedLengthListOf (generateBetween (0, 100)) fakeOrder
variedLengthListOf :: (Monad m) => m Int -> m a -> m [a]
variedLengthListOf fakeLength a = do
  lngth <- fakeLength
  listOf lngth a

--
-- type SimpleFake = FakerT Prg Identity

-- | Picks an elements from the given vector. A Vector is used so that indexing will be a constant-time operation.
--
-- > {-# LANGUAGE OverloadedLists #-}
-- >
-- > fakeFood :: (Fake r m) => m String
-- > fakeFood = pickElement ["pizza", "salad", "kimchi", "borscht"]
pickElement :: (Applicative m, Fake m) => Vector a -> m a
pickElement xs =
  if len == 0
    then error "Cannot pickElement from empty set"
    else (items !) <$> generateBetween (0, len - 1)
  where
    items = xs --fromList $ Fold.toList xs
    len = length items

-- | Like `pickElement`, only more general. Each element of the given vector is a fake generator.
pickFake :: (Monad m, Fake m) => Vector (m a) -> m a
pickFake = join . pickElement

-- | Like `pickElement`, only you exactly control the likelihood of picking each individual element. NOTE: Space taken is O(N) where N is the sum of the weights, not the number of unique elements.
--
-- > data Handedness = LeftHanded | RightHanded
-- >
-- > handedness :: (Fake r m) => m Handedness
-- > handedness = pickWeightedElement [(11, LeftHanded), (89, RightHanded)]
pickWeightedElement :: (Monad m, Fake m, Foldable t, Functor t) => t (Int, a) -> m a
pickWeightedElement = pickWeightedFake . fmap (over _2 pure)

-- | See docs for `pickFake` and `pickWeightedElement`.
pickWeightedFake :: (Monad m, Fake m, Foldable t) => t (Int, m a) -> m a
pickWeightedFake = pickFake . fromList . (=<<) (uncurry replicate) . Fold.toList

weightedCoinFlip :: (Applicative m, Fake m) => Float -> m Bool
weightedCoinFlip p = fmap (< p) generateAny


nDigitNumberBase :: (Integral a, Random a, Applicative m, Fake m) => a -> a -> m a
nDigitNumberBase _ 0 = error "Cannot generate 0 digits"
nDigitNumberBase base n = generateBetween (base ^ (n - 1), (base ^ n) - 1)

nDigitNumber :: (Integral a, Random a, Applicative m, Fake m) => a -> m a
nDigitNumber = nDigitNumberBase 10

nDigitStr :: (IsString s, Applicative m, Fake m) => Int -> m s
nDigitStr n = fmap (fromString . show) (nDigitNumber n)



{-- 
type Text = String

scrip :: (Semigroup (m Text), Applicative m, Fake m) => m Text
scrip = book <> pure " " <> chapter <> pure ":" <> verse
  where
    book = pickElement ["A", "B", "C"]
    chapter = show <$> generateBetween (1 :: Int, 30)
    verse = show <$> generateBetween (1 :: Int, 50)

phoneNumber :: (Applicative m, Fake m, Semigroup (m Text)) => m Text
phoneNumber = pure "(" <> nDigitStr 3 <> pure ")-" <> nDigitStr 3 <> pure "-" <> nDigitStr 4


someFunc :: IO ()
someFunc = do
  let fakeScripts :: Faker [Text] = listOf 10 scrip
  print $ runFaker fakeScripts defaultPrg
  let two :: Int = 2
  print $ runFaker (listOf 300 $ nDigitNumber two) defaultPrg

  print $ runFaker (listOf 300 phoneNumber) defaultPrg
  return ()
--}
