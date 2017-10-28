module Main where

import Protolude
import Babel
-- import Babel.Library
import Control.Comonad.Store

library :: Library (Int, Int) QRoom Char
library = babelLibrary2D

move :: (Int, Int)
     -> Library (Int, Int) r a
     -> Library (Int, Int) r a
move (x,y) = withLibrary (seeks (\(x',y') -> (x+x', y+y')))


data ActLibrary2DQRoom a =
    Move (Int, Int) a
  | ReadA a
  | ReadB a
  | ReadC a
  | ReadD a


type Library2DQRoom a = Library (Int, Int) QRoom a

-- instance Representable (Library (Int, Int) QRoom) where

instance Representable TPossible where
  type Rep TPossible = TChoice

  index :: TPossible a -> TChoice -> a
  index (TPossible l _) L = l
  index (TPossible _ r) R = r

  tabulate :: (TChoice -> a) -> TPossible a
  tabulate describe = TPossible (describe L) (describe R)

-- instance Adjunction


{-
use an adjunction.

Construct a zipper through the library.

since it's a store, it pairs with state, right?

so this would just be using State to update the pointer.

hmmm ye makes sense
-}





-- showRoom :: QRoom Char -> Text
-- showRoom (QRoom a b c d) = pack $
--     "Shelf a: " <> take 20 a <>
--   "\nShelf b: " <> take 20 b <>
--   "\nShelf c: " <> take 20 c <>
--   "\nShelf d: " <> take 20 d


getShelf :: Char -> QRoom a -> Maybe [[a]]
getShelf 'a' (QRoom a _ _ _) = Just a
getShelf 'b' (QRoom _ b _ _) = Just b
getShelf 'c' (QRoom _ _ c _) = Just c
getShelf 'd' (QRoom _ _ _ d) = Just d
getShelf _ _ = Nothing

getBook :: Int -> [[a]] -> Maybe [a]
getBook i l = l `atMay` i

data Choices =
    Step (Int, Int)
  | Look
  | ExamineBook (forall a. QRoom a -> Int -> Maybe [a])


runChoice :: Library (Int, Int) QRoom Char -> Choices -> IO ()
runChoice l (Step x) = mainLoop (move x l)
runChoice l Look     = print "This room is big" *> mainLoop l
runChoice l (ExamineBook f) = do
  print "Look at which shelf? <a, b, c, d> "
  char <- undefined
  print "Which book? <1..80> "
  i <- undefined




mainLoop :: Library (Int, Int) QRoom Char -> IO ()
mainLoop l = do
  let curPos = pos l
      curRoom = peek curPos l
  print $ "You are in gallery at coordinates: " <> curPos
  -- print curRoom
  -- print $


main :: IO ()
main = someFunc
