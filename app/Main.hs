module Main where

import Protolude
import Babel
-- import Babel.Library
import Data.Functor.Adjunction
import Control.Comonad.Store
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Text (Text)
import qualified Data.Text as T

{-
interacting with the library consists of
picking a book, and displaying its contents.

if we use Coord Int, we get a list of books;
if we use Coord (Int, Int), a plane;
if we use Coord (Int, Int, Int, Int, Int), either a 5D hyperspace,
  or, a 3d space with shelves and a number of volumes per shelf
  (assuming two of the ints are bounded appropriately)
-}


displayIndexNE :: (Show s, Show a) => LibIndex s (NonEmpty a) -> Text
displayIndexNE (LibIndex s a) = "Coordinate: " <> show s <>
                                (show $ NE.take 20 a) <> "..."



library :: Library Int (NonEmpty Char)
library = Library f
  where f :: Int -> NonEmpty Char
        f i = randomStream (intGen i) ('a' :| "bcde")


-- showRoom :: (Show s, Show a) => NonEmpty a -> Library s Text
-- showRoom = leftAdjunct displayIndexNE


-- showRoom' :: LibIndex s a ->

-- mkLib :: Int -> Library Int b
-- mkLib pos = Library f

doLibrary :: LibIndex Int b -> NonEmpty Char
doLibrary = rightAdjunct (const library)

showRoom :: Library Int (NonEmpty Char) -> LibIndex Int b -> Text
showRoom lib = fmap (T.pack . NE.take 20) (rightAdjunct (const lib))


-- mainLoop :: Library Int (NonEmpty Char) -> Int  -> IO ()
-- mainLoop l i = do
--   -- let showRoom = fmap (T.pack . NE.take 20) $ rightAdjunct (const l)
--   print $ "You are at index: " <> (show i :: Text)
--   print $ "1. Read index"
--   print $ "2. Step left"
--   print $ "3. Step right"
--   print $ "4. Quit"
--   cmd <- getLine
--   -- case readMaybe cmd :: Maybe Int of
--   case cmd of
--     "1" -> do
--       print $ showRoom l (LibIndex i ())
--       -- mainLoop l i
--     "2" -> mainLoop l (i - 1)
--     "3" -> mainLoop l (i + 1)
--     "4" -> print "goodbye"
--     _ -> mainLoop l i


main :: IO ()
main = do
  print "sad"
  -- mainLoop library 0
