module Crossbow.Util where

import Data.Text.Read as TR

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
