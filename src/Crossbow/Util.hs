module Crossbow.Util where

import Data.Text.Read as TR

readOne :: TR.Reader a -> Text -> a
readOne r text =
  case r text of
    Left e -> error (show e)
    Right (a, _) -> a
