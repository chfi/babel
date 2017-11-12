{-|
Module      : Babel.Library
Description : Abstract representation of the infinite Library of Babel
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

module Babel.Library where

import Babel.Prelude hiding (Rep)

import Control.Comonad.Store

import Data.Proxy
import Data.Reflection

import Data.Distributive
import Data.Functor.Rep
import Data.Functor.Adjunction

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import System.Random

import Unsafe (unsafeIndex)

-- | A Library consists of a coordinate system, an alphabet,
-- | and a producer of containers of that alphabet;
-- | the alphabet is provided as a list of characters.

newtype Library s a = Library (s -> a) deriving (Functor)

-- instance Reifies

-- data Coord a = Coord { x :: Int
--                      , y :: Int
--                      , shelf :: Int
--                      , volume :: Int
--                      , book :: a
--                      } deriving (Eq, Ord, Show, Functor)

data Coord a = Coord Int a deriving (Eq, Ord, Show, Functor)


data LibIndex s a = LibIndex s a deriving (Functor)

instance Distributive (Library s) where
  distribute :: Functor f => f (Library s a) -> Library s (f a)
  distribute fs = Library $ \s -> fmap (\(Library f) -> f s) fs

instance Representable (Library s) where
  type Rep (Library s) = s

  index :: Library s a -> Rep (Library s) -> a
  index (Library f) s = f s

  tabulate :: (s -> a) -> Library s a
  tabulate f = Library f


instance Adjunction (LibIndex s) (Library s) where
  -- `unit` fills the Library with a constant value
  -- tagged by its position in the library
  unit :: a -> Library s (LibIndex s a)
  unit a = Library $ \s -> LibIndex s a

  -- `counit` takes a Library indexed by coordinates `s`, and a coordinate,
  -- and extracts the appropriate item
  counit :: LibIndex s (Library s a) -> a
  counit (LibIndex s (Library f)) = f s


  -- Given a function that transforms an item tagged with a coordinate into some `b`,
  -- the left adjunction returns a function that takes untagged items and returns
  -- a way to index into entries of `b`
  leftAdjunct :: (LibIndex s a -> b) -> a -> Library s b
  leftAdjunct f a = Library $ \s -> f (LibIndex s a)

  -- Given a function that produces a library (i.e. a way to produce entries!)
  -- the right adjunction returns a function from coordinates to entries;
  -- i.e. indexes into the library
  rightAdjunct :: (a -> Library s b) -> LibIndex s a -> b
  rightAdjunct f (LibIndex s a) = let (Library sb) = f a
                                in sb s


-- we can create a list of books simply using Int

type LibIndexInt = LibIndex Int
type LibraryInt = Library Int


{-
to create a library we need an indexing function.
to create something a bit interesting, this function
needs to produce random books;
we need a type Int -> randomGen
-}


-- Given a list of possible outputs,
-- produces an infinite stream of elements from that list, randomly chosen
-- based on the provided random generator
randomStream :: (RandomGen g) => g -> NonEmpty a -> NonEmpty a
randomStream g as = NE.unfold next' g
  where l = length as - 1
        next' gen = let (i, gen') = randomR (0, l) gen
                    in (NE.toList as `unsafeIndex` i, Just gen')

intGen :: Int -> StdGen
intGen = mkStdGen
