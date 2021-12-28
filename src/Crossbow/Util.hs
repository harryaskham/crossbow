module Crossbow.Util where

import Data.Text.Read as TR

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

readOne :: TR.Reader a -> Text -> a
readOne r text =
  case r text of
    Left e -> error (show e)
    Right (a, _) -> a
