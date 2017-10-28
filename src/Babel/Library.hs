{-|
Module      : Babel.Library
Description : Abstract representation of the infinite Library of Babel
-}
module Babel.Library where

import Babel.Prelude

import Control.Comonad.Store

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import System.Random

-- | A Library consists of a coordinate system, an alphabet,
-- | and a producer of containers of that alphabet;
-- | the alphabet is provided as a list of characters.

newtype Library coord room a =
  Library { getRoom :: Store coord (room a) } deriving (Functor)


{-

This is overcomplicated -- or overly specific.
the Library is really just an index into books.

The rooms don't matter; they're just small trees. They can also be indexed into.

newtype Li

-}

withLibrary :: (Store c (f a) -> Store d (g b)) -> Library c f a -> Library d g b
withLibrary f (Library s) = Library $ f s


-- to create a library we need:
--   An alphabet to create books from,
--   A coordinate system,
--   A type of our rooms (containers of books),
--   An indexing from coordinates to rooms, which uses the alphabet
mkLibrary :: (coord -> room a)
           -> coord
           -> Library coord room a
mkLibrary f = Library . store f

-- 2D library of babel
babelLibrary2D :: Library (Int, Int) QRoom Char
babelLibrary2D = mkLibrary f (0,0)
  where f (x, y) = genRoom babelAlphabet $ mkStdGen $ x `mod` y


newtype Alphabet a = Alphabet a deriving (Eq, Ord, Functor, Show)

-- In the book, the alphabet consists of "22 letters, the period, the comma, and the space"
babelAlphabet :: Alphabet [Char]
babelAlphabet = Alphabet $ ['A'..'V'] <> "., "

alphaStream :: (RandomGen g, Random a, Ord a)=>  g -> Alphabet [a] -> NonEmpty a
alphaStream g (Alphabet as) = head' :| tail'
  where (l, r)      = (minimum as, maximum as)
        (head', g') = randomR (l, r) g
        tail'       = randomRs (l, r) g'

type Book a  = [a]
type Shelf a = [Book a]

data QRoom a = QRoom { s1 :: Shelf a
                     , s2 :: Shelf a
                     , s3 :: Shelf a
                     , s4 :: Shelf a
                     } deriving (Eq, Ord, Functor)

-- these streams are always infinite, by construction
newtype BookStream a = BookStream (NonEmpty a) deriving (Eq, Ord, Functor, Show)

bookStream :: (RandomGen g, Ord a, Random a) => g -> Alphabet [a] -> BookStream a
bookStream gen = BookStream . alphaStream gen


genBook :: Int -> BookStream a -> (Book a, BookStream a)
genBook len (BookStream s) = (h, t')
  where (h, t) = NonEmpty.splitAt len s
        t' = BookStream $ NonEmpty.fromList t

iterState :: (s -> (a, s)) -> s -> [a]
iterState f = unfoldr (Just . f)

genBooks :: Int -> BookStream a -> [Book a]
genBooks len str = iterState (genBook len) str


genRoom :: (RandomGen g, Ord a, Random a) => Alphabet [a] -> (g -> QRoom a)
genRoom alpha g = QRoom a b c d
  where lineLen = 80
        pageLen = 40
        bookLen = 410
        numBooks = 40
        bookStr = bookStream g alpha
        genShelves = unfoldr (Just . splitAt numBooks) $ genBooks (lineLen * pageLen * bookLen) bookStr
        [a,b,c,d] = take 4 genShelves
