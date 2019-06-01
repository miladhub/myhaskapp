module MyApp (MyApp(..), mainTest) where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data MyApp a =
  MyApp [String] a
  deriving (Eq, Show)

instance Functor MyApp where
  fmap f (MyApp xs a) = MyApp xs (f a)

instance Applicative MyApp where
  pure = MyApp []
  (MyApp xs fa) <*> (MyApp xs' a) = MyApp (xs <> xs') (fa a)

instance Monad MyApp where
  return = pure
  (MyApp xs a) >>= f =
    let (MyApp xs' b) = f a
    in (MyApp (xs <> xs') b)

instance Arbitrary a => Arbitrary (MyApp a) where
  arbitrary =
    MyApp <$> arbitrary <*> arbitrary

instance Eq a => EqProp (MyApp a) where
  (=-=) = eq

mainTest :: IO ()
mainTest =
  let xs = undefined :: MyApp (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs
